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
 *   fs_ac_group  x header.ngroups     32 bytes each
 *   fs_ac_entry  x (sum of counts)    24 bytes each, at each group's offset
 *
 * Snapshots are never freed: a render may still be reading one after a
 * newer one is published. They're small, and only replaced when a group
 * changes.
 */

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
} fs_ac_group;

typedef struct {
  double x;
  double y;
  uint32_t rgb;      /* 0x00RRGGBB */
  uint32_t pad;
} fs_ac_entry;

typedef uint32_t (*fs_ac_add_fn)(uint8_t add, int32_t group, double x, double y);

typedef struct {
  fs_ac_header *snapshot;  /* read and written atomically */
  fs_ac_add_fn add;        /* the Haskell lookup, for points to add */
} fs_ac_dir;

void fs_ac_publish(fs_ac_dir *dir, fs_ac_header *snapshot)
{
  __atomic_store_n(&dir->snapshot, snapshot, __ATOMIC_RELEASE);
}

uint32_t fs_ac_lookup(fs_ac_dir *dir, uint8_t add, int32_t group, double x, double y)
{
  fs_ac_header *s = __atomic_load_n(&dir->snapshot, __ATOMIC_ACQUIRE);
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
      if (best >= 0) return entries[best].rgb;
      if (!add || !g->can_add) return g->miss;
      break;
    }
  }
  /* a point to add, or a group the snapshot doesn't have yet */
  return dir->add(add, group, x, y);
}
