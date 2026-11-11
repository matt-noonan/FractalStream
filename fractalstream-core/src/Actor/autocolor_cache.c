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
 * The snapshot's layout (built by Actor.Autocolor.publishAutocolor), and
 * looking a point up in it, are in autocolor_lookup.h (which the wasm
 * backend shares).
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

#include "autocolor_lookup.h"

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
/* What a lookup gives when it has to ask Haskell but can't: magenta. */
#define FS_AC_NO_CALLBACK 0x00FF00FFu
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

/* The current snapshot (or NULL), for a single-threaded caller (the wasm
   build, which copies it into a kernel's memory): with other threads, it
   could be freed at any moment. */
const fs_ac_header *fs_ac_current(fs_ac_dir *dir)
{
  return __atomic_load_n(&dir->snapshot, __ATOMIC_SEQ_CST);
}

int64_t fs_ac_snapshot_size(const fs_ac_header *s)
{
  return s ? fs_ac_size(s) : 0;
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

  uint32_t result;
  int found = fs_ac_find(__atomic_load_n(&dir->snapshot, __ATOMIC_SEQ_CST),
                         mode, group, x, y, &result);

  /* done with the snapshot: leave before calling back into Haskell */
  __atomic_sub_fetch(&dir->readers[parity], 1, __ATOMIC_SEQ_CST);
  if (found) return result;
  /* a point to add, or a group the snapshot doesn't have yet (a
     directory without a callback, as on WebAssembly, gives magenta: it
     can't ask) */
  if (!dir->add) return FS_AC_NO_CALLBACK;
  return dir->add(mode, group, x, y);
}
