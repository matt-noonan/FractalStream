/* What a tool's handler does to tables, for the wasm backend (module
   "tables"; see Backend.Wasm.Code). dev-tools/wasm/build.sh compiles this
   to fractalstream-backend-wasm/js/tables.wasm.

   The LLVM backend calls back into Haskell for these (the host's table
   callbacks, Backend.LLVM.Table); a worker can't, so they're here, with the
   same results as fractalstream-core's Language.Table. They work in the
   kernel's own memory (they import env.memory), so they mustn't keep
   anything there themselves: no static data, and no stack (the build
   checks).

   A table is the address of its descriptor, laid out as Backend.Wasm.Table
   says:

     offset  0: i32     the address of the elements
     offset  4: i32     the number of dimensions, n
     offset  8: i32     frozen: nonzero if others may be looking at the table
     offset 16: f64     origin x, the center of cell (1, 1)
     offset 24: f64     origin y
     offset 32: f64     a cell's width
     offset 40: f64     a cell's height (rows go down)
     offset 48: i32[n]  the size along each dimension

   A new table goes in the tool's arena, from `bump` (rounded up to 8): its
   descriptor, then its elements. Each function that makes one gives back
   where the arena's free space starts after it (so the new descriptor is
   at `bump` rounded up to 8), or 0 if it doesn't fit below `end`, or -1 if
   a size is negative (or there are the wrong number of them). */
#include <stdint.h>

typedef struct {
  int32_t elems;
  int32_t n;
  int32_t frozen;
  int32_t pad;
  double ox, oy, sx, sy;
  int32_t dims[];
} table;

static inline int32_t align8(int32_t a) { return (a + 7) & ~7; }

static inline uint8_t *at(int32_t a) { return (uint8_t *)(uintptr_t)a; }

/* How many cells, or -1 if a size is negative or there are too many. */
static int64_t cell_count(int32_t n, const int32_t *dims) {
  int64_t cells = 1;
  for (int32_t k = 0; k < n; k++) {
    if (dims[k] < 0) return -1;
    cells *= dims[k];
    if (cells > 0x7fffffff) return -1;
  }
  return cells;
}

/* Lay out a table's descriptor at bump (rounded up), with room for its
   elements after it: the descriptor, or 0 if it doesn't fit. (The arena's
   free space then starts at its elements' end: see `top`.) */
static table *place(int32_t esize, int32_t n, const int32_t *dims, int64_t cells,
                    int32_t bump, int32_t end) {
  int32_t d = align8(bump);
  int64_t e = align8(d + 48 + 4 * n);
  int64_t t = e + (int64_t)esize * cells;
  if (t > end) return 0;
  table *tbl = (table *)at(d);
  tbl->elems = (int32_t)e;
  tbl->n = n;
  tbl->frozen = 0;
  tbl->pad = 0;
  for (int32_t k = 0; k < n; k++) tbl->dims[k] = dims[k];
  return tbl;
}

static inline int32_t top(const table *tbl, int32_t esize, int64_t cells) {
  return (int32_t)(tbl->elems + (int64_t)esize * cells);
}

/* Every cell the element at fill, or zeros. */
static void fill_cells(table *tbl, int32_t esize, int64_t cells, const uint8_t *fill) {
  uint8_t *p = at(tbl->elems);
  if (!fill) { __builtin_memset(p, 0, (uint32_t)(esize * cells)); return; }
  for (int64_t c = 0; c < cells; c++) __builtin_memcpy(p + c * esize, fill, (uint32_t)esize);
}

/* `reset T to (sizes) [filled with v]` over the rectangle from (x1, y1)
   (top left) to (x2, y2): as Language.Table.allocateTable. */
__attribute__((export_name("new")))
int32_t fs_table_new(int32_t esize, int32_t n, const int32_t *dims,
                     double x1, double y1, double x2, double y2,
                     const uint8_t *fill, int32_t bump, int32_t end) {
  int64_t cells = cell_count(n, dims);
  if (cells < 0) return -1;
  table *tbl = place(esize, n, dims, cells, bump, end);
  if (!tbl) return 0;
  int32_t w = n >= 1 ? dims[0] : 1;
  int32_t h = n >= 2 ? dims[1] : 1;
  double dx = (x2 - x1) / (double)(w > 1 ? w : 1);
  double dy = (y1 - y2) / (double)(h > 1 ? h : 1);
  tbl->ox = x1 + dx / 2;
  tbl->oy = y1 - dy / 2;
  tbl->sx = dx;
  tbl->sy = dy;
  fill_cells(tbl, esize, cells, fill);
  return top(tbl, esize, cells);
}

/* A copy of a table (to write, when it's frozen). */
__attribute__((export_name("thaw")))
int32_t fs_table_thaw(int32_t esize, const table *src, int32_t bump, int32_t end) {
  int64_t cells = cell_count(src->n, src->dims);
  if (cells < 0) return -1;
  table *tbl = place(esize, src->n, src->dims, cells, bump, end);
  if (!tbl) return 0;
  tbl->ox = src->ox; tbl->oy = src->oy; tbl->sx = src->sx; tbl->sy = src->sy;
  __builtin_memcpy(at(tbl->elems), at(src->elems), (uint32_t)(esize * cells));
  return top(tbl, esize, cells);
}

/* `resize T to (sizes) [filled with v]`: as Language.Table.resizeTable, a
   new table in the same place (cell (1, 1) where it was, cells the same
   size), keeping the cells the two share; new cells get fill, or zeros. */
__attribute__((export_name("resize")))
int32_t fs_table_resize(int32_t esize, const table *src, int32_t n, const int32_t *dims,
                        const uint8_t *fill, int32_t bump, int32_t end) {
  if (n != src->n) return -1;
  int64_t cells = cell_count(n, dims);
  if (cells < 0) return -1;
  table *tbl = place(esize, n, dims, cells, bump, end);
  if (!tbl) return 0;
  tbl->ox = src->ox; tbl->oy = src->oy; tbl->sx = src->sx; tbl->sy = src->sy;
  fill_cells(tbl, esize, cells, fill);
  if (n == 0) return top(tbl, esize, cells);
  /* one run along the first dimension for each choice of the other
     indices that both have */
  int32_t run = dims[0] < src->dims[0] ? dims[0] : src->dims[0];
  int64_t others = 1;
  for (int32_t k = 1; k < n; k++) {
    int32_t common = dims[k] < src->dims[k] ? dims[k] : src->dims[k];
    others *= common;
  }
  if (run <= 0 || others <= 0) return top(tbl, esize, cells);
  for (int64_t o = 0; o < others; o++) {
    int64_t rest = o, newOff = 0, oldOff = 0, newStride = dims[0], oldStride = src->dims[0];
    for (int32_t k = 1; k < n; k++) {
      int32_t common = dims[k] < src->dims[k] ? dims[k] : src->dims[k];
      int64_t i = rest % common;
      rest /= common;
      newOff += i * newStride;
      oldOff += i * oldStride;
      newStride *= dims[k];
      oldStride *= src->dims[k];
    }
    __builtin_memcpy(at(tbl->elems) + newOff * esize, at(src->elems) + oldOff * esize, (uint32_t)(run * esize));
  }
  return top(tbl, esize, cells);
}

/* Where a slice is (as Language.Table.sliceView): its indices, one per
   dimension (1-based; ignored where `stars` is nonzero, for a *).
   Gives its first cell, or -1 if there are the wrong number of indices or
   one is out of range. */
static int64_t slice_base(const table *t, int32_t n, const int32_t *ixs, const int32_t *stars) {
  if (n != t->n) return -1;
  int64_t base = 0, stride = 1;
  for (int32_t k = 0; k < n; k++) {
    if (!stars[k]) {
      if (ixs[k] < 1 || ixs[k] > t->dims[k]) return -1;
      base += (int64_t)(ixs[k] - 1) * stride;
    }
    stride *= t->dims[k];
  }
  return base;
}

/* The c-th cell of a slice (counting from 0, the first * varying fastest). */
static int64_t slice_cell(const table *t, int32_t n, const int32_t *stars, int64_t base, int64_t c) {
  int64_t off = base, stride = 1;
  for (int32_t k = 0; k < n; k++) {
    if (stars[k]) {
      off += (c % t->dims[k]) * stride;
      c /= t->dims[k];
    }
    stride *= t->dims[k];
  }
  return off;
}

/* `T @ (...) <- U @ (...)`: copy a slice of src onto one of dst (as
   Language.Table.copySliceBytes): their *s must span the same numbers of
   cells, in order. Through a buffer in the arena (at bump), in case they
   overlap. 1 if it went well; 0 if an index is out of range or the
   shapes differ; -1 if the buffer doesn't fit. */
__attribute__((export_name("copy")))
int32_t fs_table_copy(int32_t esize, table *dst, int32_t n, const int32_t *dixs, const int32_t *dstars,
                      const table *src, int32_t m, const int32_t *sixs, const int32_t *sstars,
                      int32_t bump, int32_t end) {
  int64_t dbase = slice_base(dst, n, dixs, dstars);
  int64_t sbase = slice_base(src, m, sixs, sstars);
  if (dbase < 0 || sbase < 0) return 0;
  /* the *s' spans, in order, must match */
  int32_t ks = 0;
  int64_t count = 1;
  for (int32_t k = 0; k < n; k++) {
    if (!dstars[k]) continue;
    while (ks < m && !sstars[ks]) ks++;
    if (ks >= m || src->dims[ks] != dst->dims[k]) return 0;
    count *= dst->dims[k];
    ks++;
  }
  while (ks < m && !sstars[ks]) ks++;
  if (ks < m) return 0;
  int32_t tmp = align8(bump);
  if ((int64_t)tmp + count * esize > end) return -1;
  for (int64_t c = 0; c < count; c++)
    __builtin_memcpy(at(tmp) + c * esize, at(src->elems) + slice_cell(src, m, sstars, sbase, c) * esize, (uint32_t)esize);
  for (int64_t c = 0; c < count; c++)
    __builtin_memcpy(at(dst->elems) + slice_cell(dst, n, dstars, dbase, c) * esize, at(tmp) + c * esize, (uint32_t)esize);
  return 1;
}
