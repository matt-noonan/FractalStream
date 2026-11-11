/*
 * The autocolor snapshot's layout, and looking a point up in it: shared by
 * autocolor_cache.c (for code the LLVM backend compiles) and the wasm
 * backend's autocolor module (fractalstream-backend-wasm/libm/fsautocolor.c),
 * so the two find, and blend, alike. See autocolor_cache.c and
 * Actor.Autocolor.publishAutocolor for the layout:
 *
 *   fs_ac_header                      8 bytes
 *   fs_ac_group  x header.ngroups     40 bytes each
 *   fs_ac_entry  x (sum of counts)    24 bytes each, at each group's offset
 *
 * Offsets are from the snapshot's start, so a copy of it works anywhere.
 */
#ifndef FS_AUTOCOLOR_LOOKUP_H
#define FS_AUTOCOLOR_LOOKUP_H

#include <stdint.h>

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

/* The snapshot's size in bytes. */
static inline int64_t fs_ac_size(const fs_ac_header *s)
{
  const fs_ac_group *groups = (const fs_ac_group *)(s + 1);
  int64_t n = 0;
  for (int32_t k = 0; k < s->ngroups; k++) n += groups[k].count;
  return (int64_t)sizeof(fs_ac_header) + (int64_t)s->ngroups * (int64_t)sizeof(fs_ac_group)
         + n * (int64_t)sizeof(fs_ac_entry);
}

/* Every entry's color, weighted by one over its squared distance from
   (x, y), rounded half up (as Actor.Autocolor.blendAutocolor does). */
static inline uint32_t fs_ac_blend(const fs_ac_entry *entries, int32_t count, double x, double y)
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

/* Look a point up in group `group` of the snapshot. Gives 1, with the
   color in *result, if the snapshot decides it: the nearest entry within
   the tolerance, or else (when no point is to be added) get_autocolor's
   blend, a locked group's blend or miss color, or nearest_autocolor's
   miss color. Gives 0 if a point is to be added (autocolor, in a group
   that can add one), or the snapshot hasn't the group; *result is then
   what the group gives without adding a point (its blend or miss color,
   as if it were locked), or 0 if it isn't there. */
static inline int fs_ac_find(const fs_ac_header *s, uint8_t mode, int32_t group,
                             double x, double y, uint32_t *result)
{
  *result = 0;
  if (!s) return 0;
  const fs_ac_group *groups = (const fs_ac_group *)(s + 1);
  for (int32_t k = 0; k < s->ngroups; k++) {
    const fs_ac_group *g = &groups[k];
    if (g->group != group) continue;
    /* the nearest entry within the tolerance */
    const fs_ac_entry *entries = (const fs_ac_entry *)((const char *)s + g->offset);
    int32_t best = -1;
    double bestd = g->tol2;
    for (int32_t i = 0; i < g->count; i++) {
      double dx = entries[i].x - x, dy = entries[i].y - y;
      double d = dx * dx + dy * dy;
      if (d <= bestd && (best < 0 || d < bestd)) { best = i; bestd = d; }
    }
    if (best >= 0) { *result = entries[best].rgb; return 1; }
    /* no entry near enough: get_autocolor blends; so does autocolor in a
       group that blends misses (when it's locked or full, or until the
       point is added); otherwise, the miss color */
    int blends = mode == FS_AC_BLEND || (mode == FS_AC_ADD && g->blend_misses);
    *result = (blends && g->count > 0) ? fs_ac_blend(entries, g->count, x, y) : g->miss;
    return !(mode == FS_AC_ADD && g->can_add);
  }
  return 0;
}

#endif
