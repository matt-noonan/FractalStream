/*
 * Autocolor lookups for compiled code (see Actor.Autocolor).
 *
 * Calling back into Haskell for every pixel is slow, and nearly every
 * lookup finds a point the table already has. So compiled code calls
 * fs_ac_lookup, which reads an immutable snapshot of the project's
 * autocolor groups -- published by the Haskell side whenever anything in
 * them changes -- without taking any lock, and calls back into Haskell only
 * to add a point the snapshot doesn't have.
 *
 * Snapshot layout (built by Actor.Autocolor.publishAutocolor), all in one
 * block of memory:
 *
 *   fs_ac_header                      8 bytes
 *   fs_ac_group  x header.ngroups     40 bytes each
 *   fs_ac_entry  x (sum of counts)    24 bytes each, at each group's offset
 *
 * Reclaiming old snapshots: a render may still be reading a snapshot after
 * a newer one is published, so it can only be freed once no lookup that
 * might have seen it is still running. Lookups are short and never block,
 * so a small epoch scheme does it (a simple form of read-copy-update):
 *
 * - A lookup reads the epoch, adds itself to that epoch parity's reader
 *   count, and checks that the epoch hasn't changed meanwhile (retrying if
 *   it has); only then does it load the snapshot. It leaves the count
 *   before calling back into Haskell, so a thread adding a point never
 *   waits on itself.
 * - Publishing (one at a time: the Haskell side holds a lock) swaps in the
 *   new snapshot, moves the epoch on, waits for the old parity's readers
 *   to finish -- each one is only ever a single lookup -- and frees the old
 *   snapshot. A reader could hold the snapshot it frees, or the one after
 *   it; the publish after this one can't start until this one is done
 *   waiting for that reader, so neither is freed while it's in use.
 */

#include <stdint.h>
#include <stdlib.h>
#ifdef _WIN32
#include <windows.h>
#define fs_ac_yield() SwitchToThread()
#else
#include <sched.h>
#define fs_ac_yield() sched_yield()
#endif

typedef struct {
  int32_t ngroups;
  int32_t pad;
} fs_ac_header;

typedef struct {
  int32_t group;
  int32_t can_add;   /* not locked, and not full */
  uint32_t miss;     /* 0x00RRGGBB */
  int32_t count;
  double tol2;       /* the tolerance, squared */
  int64_t offset;    /* of this group's entries, from the snapshot's start */
  int32_t blend_misses; /* when it can't add a point: blend, or the miss color? */
  int32_t pad;
} fs_ac_group;

typedef struct {
  double x;
  double y;
  uint32_t rgb;      /* 0x00RRGGBB */
  uint32_t pad;
} fs_ac_entry;

/* How a lookup treats a point (Language.Type.AutocolorMode):
   the nearest entry's color, or else ... */
#define FS_AC_NEAREST 0  /* ... the group's miss color (nearest_autocolor) */
#define FS_AC_ADD     1  /* ... a new entry, if it can add one (autocolor) */
#define FS_AC_BLEND   2  /* ... a blend of every entry's color (get_autocolor) */

typedef uint32_t (*fs_ac_add_fn)(uint8_t mode, int32_t group, double x, double y);

typedef struct {
  fs_ac_header *snapshot;  /* read and written atomically */
  fs_ac_add_fn add;        /* the Haskell lookup, for points to add */
  uint64_t epoch;
  int64_t readers[2];      /* lookups in progress, by epoch parity */
} fs_ac_dir;

fs_ac_dir *fs_ac_new(fs_ac_add_fn add)
{
  fs_ac_dir *dir = calloc(1, sizeof(fs_ac_dir));
  if (dir) dir->add = add;
  return dir;
}

/* Only once nothing can be using the directory any more. */
void fs_ac_free(fs_ac_dir *dir)
{
  if (!dir) return;
  free(dir->snapshot);
  free(dir);
}

/* Overwrite a snapshot's colors before freeing it, so that a lookup that
   read one anyway would give a recognizably wrong color (and fail the
   tests), rather than a plausible one. */
#define FS_AC_FREED 0x00DEAD01u
static void poison(fs_ac_header *s)
{
  fs_ac_group *groups = (fs_ac_group *)(s + 1);
  for (int32_t k = 0; k < s->ngroups; k++) {
    fs_ac_entry *entries = (fs_ac_entry *)((char *)s + groups[k].offset);
    for (int32_t i = 0; i < groups[k].count; i++) entries[i].rgb = FS_AC_FREED;
    groups[k].miss = FS_AC_FREED;
  }
}

/* For tests: keep replaced snapshots (poisoned) instead of freeing them,
   so that malloc can't hand the memory straight back for the next
   snapshot -- which would hide a lookup reading a snapshot it shouldn't. */
static int quarantine = 0;
void fs_ac_debug_quarantine(int on) { quarantine = on; }

/* Publish a snapshot (malloc'd), freeing the one it replaces once no
   lookup can be reading it. Not to be called by two threads at once. */
void fs_ac_publish(fs_ac_dir *dir, fs_ac_header *snapshot)
{
  fs_ac_header *old = __atomic_exchange_n(&dir->snapshot, snapshot, __ATOMIC_SEQ_CST);
  uint64_t parity = __atomic_fetch_add(&dir->epoch, 1, __ATOMIC_SEQ_CST) & 1;
  while (__atomic_load_n(&dir->readers[parity], __ATOMIC_SEQ_CST) != 0)
    fs_ac_yield();
  if (old) {
    poison(old);
    if (!quarantine) free(old);
  }
}

/* Every entry's color, weighted by one over its squared distance from
   (x, y), rounded half up (as Actor.Autocolor.blendAutocolor does). */
static uint32_t blend(const fs_ac_entry *entries, int32_t count, double x, double y)
{
  double total = 0, r = 0, gr = 0, b = 0;
  for (int32_t i = 0; i < count; i++) {
    double dx = entries[i].x - x, dy = entries[i].y - y;
    double w = 1.0 / (dx * dx + dy * dy);
    uint32_t c = entries[i].rgb;
    total += w;
    r += w * ((c >> 16) & 0xff);
    gr += w * ((c >> 8) & 0xff);
    b += w * (c & 0xff);
  }
  uint32_t rr = (uint32_t)(r / total + 0.5), gg = (uint32_t)(gr / total + 0.5),
           bb = (uint32_t)(b / total + 0.5);
  if (rr > 255) rr = 255;
  if (gg > 255) gg = 255;
  if (bb > 255) bb = 255;
  return (rr << 16) | (gg << 8) | bb;
}

uint32_t fs_ac_lookup(fs_ac_dir *dir, uint8_t mode, int32_t group, double x, double y)
{
  /* join the readers of the current epoch */
  uint64_t parity;
  for (;;) {
    uint64_t epoch = __atomic_load_n(&dir->epoch, __ATOMIC_SEQ_CST);
    parity = epoch & 1;
    __atomic_add_fetch(&dir->readers[parity], 1, __ATOMIC_SEQ_CST);
    if (__atomic_load_n(&dir->epoch, __ATOMIC_SEQ_CST) == epoch) break;
    __atomic_sub_fetch(&dir->readers[parity], 1, __ATOMIC_SEQ_CST);
  }

  int found = 0;
  uint32_t result = 0;
  fs_ac_header *s = __atomic_load_n(&dir->snapshot, __ATOMIC_SEQ_CST);
  if (s) {
    fs_ac_group *groups = (fs_ac_group *)(s + 1);
    for (int32_t k = 0; k < s->ngroups; k++) {
      fs_ac_group *g = &groups[k];
      if (g->group != group) continue;
      /* the nearest entry within the tolerance */
      fs_ac_entry *entries = (fs_ac_entry *)((char *)s + g->offset);
      int32_t best = -1;
      double bestd = g->tol2;
      for (int32_t i = 0; i < g->count; i++) {
        double dx = entries[i].x - x, dy = entries[i].y - y;
        double d = dx * dx + dy * dy;
        if (d <= bestd && (best < 0 || d < bestd)) { best = i; bestd = d; }
      }
      if (best >= 0) { found = 1; result = entries[best].rgb; }
      else if (mode == FS_AC_ADD && g->can_add) { /* to add: ask Haskell */ }
      else {
        /* no entry near enough, and none to be added: get_autocolor
           blends; so does autocolor, in a locked (or full) group that
           blends misses; otherwise, the miss color */
        int blends = mode == FS_AC_BLEND || (mode == FS_AC_ADD && g->blend_misses);
        found = 1;
        result = (blends && g->count > 0) ? blend(entries, g->count, x, y) : g->miss;
      }
      break;
    }
  }

  /* done with the snapshot: leave before calling back into Haskell */
  __atomic_sub_fetch(&dir->readers[parity], 1, __ATOMIC_SEQ_CST);
  if (found) return result;
  /* a point to add, or a group the snapshot doesn't have yet */
  return dir->add(mode, group, x, y);
}
