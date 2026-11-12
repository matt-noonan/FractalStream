/* Autocolor lookups for the wasm backend's kernels (module "autocolor";
   see Backend.Wasm.Value). dev-tools/wasm/build.sh compiles this to
   fractalstream-backend-wasm/js/autocolor.wasm.

   It works on the kernel's own memory (it imports env.memory), so it
   mustn't keep anything there itself: no static data, and no stack (the
   build checks). It reads a copy of the autocolor snapshot the kernel was
   given, with the same search as the LLVM backend's code (autocolor_lookup.h,
   shared with fractalstream-core's autocolor_cache.c). A point it can't
   decide -- one to add, or in a group the snapshot hasn't got -- goes on
   the misses list, for the engine to add (Backend.Wasm), and meanwhile
   gets what the group gives without adding a point (or 0). */
#include "../../fractalstream-core/src/Actor/autocolor_lookup.h"

typedef struct {
  int32_t group;
  int32_t mode;
  double x;
  double y;
} fs_ac_miss;

typedef struct {
  int32_t count;
  int32_t capacity;
  fs_ac_miss misses[];
} fs_ac_misses;

__attribute__((export_name("lookup")))
uint32_t fs_ac_kernel_lookup(const fs_ac_header *snapshot, int32_t mode, int32_t group,
                             double x, double y, fs_ac_misses *misses)
{
  uint32_t result;
  if (fs_ac_find(snapshot, (uint8_t)mode, group, x, y, &result)) return result;
  if (misses && misses->count < misses->capacity) {
    fs_ac_miss *m = &misses->misses[misses->count++];
    m->group = group;
    m->mode = mode;
    m->x = x;
    m->y = y;
  }
  return result;
}
