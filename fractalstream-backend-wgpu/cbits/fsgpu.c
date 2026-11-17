/* The desktop's GPU, for the GPU backends (fractalstream-backend-wgsl,
 * through Backend.WGPU): wgpu-native's webgpu.h, behind a few synchronous
 * calls, as js/gpu.mjs (fractalstream-backend-wasm) is the web's.
 *
 * WebGPU's waits are callbacks; each call here waits for its own, so it
 * returns when the work's done. Calls take turns with one lock (an error
 * scope is the device's, so two threads' work would see each other's
 * errors), but a block's run doesn't hold it while the GPU draws: each
 * block costs a round trip to the GPU, so the render threads' blocks are
 * in flight together. And a block waits by polling without blocking,
 * napping 50 us between polls: wgpu's blocking poll took about 1.2 ms
 * more a block (on an M5: 1.45 ms a 64x64 block, against 0.24 ms, and
 * 0.07 ms with blocks from several threads at once).
 *
 * Errors come back as strings the caller frees (fsgpu_free_string).
 * Every kernel has the bindings Backend.WGSL.Runtime declares: the
 * parameters (a uniform), the arguments (read-only storage), the pixels
 * (storage, a u32 each, 0xIIBBGGRR), and the tables (read-only storage).
 */
#if !defined(_WIN32)
/* (nanosleep: POSIX, which -std=c11 leaves out on Linux) */
#define _POSIX_C_SOURCE 200809L
#endif
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "webgpu/webgpu.h"
#include "webgpu/wgpu.h"

/* The lock, and a nap between polls: POSIX's, or Windows'. (Windows can't
 * sleep for 50 us, so a block's wait there yields the rest of its time
 * slice instead.) */
#if defined(_WIN32)
#include <windows.h>
static SRWLOCK lock = SRWLOCK_INIT;
static void take_lock(void) { AcquireSRWLockExclusive(&lock); }
static void give_lock(void) { ReleaseSRWLockExclusive(&lock); }
static void nap(void) { SwitchToThread(); }
#else
#include <pthread.h>
#include <time.h>
static pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;
static void take_lock(void) { pthread_mutex_lock(&lock); }
static void give_lock(void) { pthread_mutex_unlock(&lock); }
static void nap(void) { nanosleep(&(struct timespec){ 0, 50000 }, NULL); }
#endif
static WGPUInstance instance;
static WGPUAdapter adapter;
static WGPUDevice device;
static WGPUQueue queue;
static WGPUBindGroupLayout layout;
static WGPUPipelineLayout pipeline_layout;
static WGPUBuffer no_tables;
static uint64_t max_binding;
static char *lost;               /* why the device was lost, once it was */

static char *copy_view(const char *prefix, WGPUStringView v) {
  size_t n = v.data ? (v.length == WGPU_STRLEN ? strlen(v.data) : v.length) : 0;
  size_t p = strlen(prefix);
  char *s = malloc(p + n + 1);
  memcpy(s, prefix, p);
  if (n) memcpy(s + p, v.data, n);
  s[p + n] = 0;
  return s;
}

static char *copy_string(const char *s) {
  size_t n = strlen(s) + 1;
  char *c = malloc(n);
  memcpy(c, s, n);
  return c;
}

void fsgpu_free_string(char *s) { free(s); }

/* ---- waiting on callbacks ---- */

struct waited { atomic_int done; int ok; char *message; void *handle; };

/* (for what wgpu answers without the GPU: an adapter, a device, an error
 * scope; it never blocks on the GPU's work) */
static void wait_for(struct waited *w) {
  while (!atomic_load(&w->done)) {
    wgpuInstanceProcessEvents(instance);
    if (!atomic_load(&w->done) && device) wgpuDevicePoll(device, 0, NULL);
  }
}

static void on_adapter(WGPURequestAdapterStatus status, WGPUAdapter a, WGPUStringView message, void *u1, void *u2) {
  (void)u2;
  struct waited *w = u1;
  w->ok = status == WGPURequestAdapterStatus_Success;
  w->handle = a;
  if (!w->ok) w->message = copy_view("No GPU adapter: ", message);
  atomic_store(&w->done, 1);
}

static void on_device(WGPURequestDeviceStatus status, WGPUDevice d, WGPUStringView message, void *u1, void *u2) {
  (void)u2;
  struct waited *w = u1;
  w->ok = status == WGPURequestDeviceStatus_Success;
  w->handle = d;
  if (!w->ok) w->message = copy_view("No GPU device: ", message);
  atomic_store(&w->done, 1);
}

static void on_lost(WGPUDevice const *d, WGPUDeviceLostReason reason, WGPUStringView message, void *u1, void *u2) {
  (void)d; (void)u1; (void)u2;
  if (reason == WGPUDeviceLostReason_Destroyed || reason == WGPUDeviceLostReason_CallbackCancelled) return;
  if (!lost) lost = copy_view("The GPU was lost: ", message);
}

static void on_uncaptured(WGPUDevice const *d, WGPUErrorType type, WGPUStringView message, void *u1, void *u2) {
  (void)d; (void)type; (void)u1; (void)u2;
  /* (every call's work is in error scopes; this is only for what escapes them) */
  fprintf(stderr, "GPU error: %.*s\n", (int)(message.length == WGPU_STRLEN ? strlen(message.data) : message.length), message.data);
}

static void on_scope(WGPUPopErrorScopeStatus status, WGPUErrorType type, WGPUStringView message, void *u1, void *u2) {
  (void)u2;
  struct waited *w = u1;
  w->ok = status == WGPUPopErrorScopeStatus_Success && type == WGPUErrorType_NoError;
  if (!w->ok) {
    const char *what = type == WGPUErrorType_OutOfMemory ? "out of GPU memory: " : "";
    w->message = status == WGPUPopErrorScopeStatus_Success ? copy_view(what, message)
                                                           : copy_string("the GPU's errors couldn't be read");
  }
  atomic_store(&w->done, 1);
}

static void on_map(WGPUMapAsyncStatus status, WGPUStringView message, void *u1, void *u2) {
  (void)u2;
  struct waited *w = u1;
  w->ok = status == WGPUMapAsyncStatus_Success;
  if (!w->ok) w->message = copy_view("the GPU's results couldn't be read: ", message);
  atomic_store(&w->done, 1);
}

/* Work in error scopes: begin, then end with NULL or the first error. */
static void scopes_begin(void) {
  wgpuDevicePushErrorScope(device, WGPUErrorFilter_Validation);
  wgpuDevicePushErrorScope(device, WGPUErrorFilter_OutOfMemory);
}

static char *scopes_end(void) {
  char *first = NULL;
  for (int k = 0; k < 2; k++) {   /* (wgpu has no Internal filter) */
    struct waited w = {0};
    WGPUPopErrorScopeCallbackInfo cb = WGPU_POP_ERROR_SCOPE_CALLBACK_INFO_INIT;
    cb.mode = WGPUCallbackMode_AllowProcessEvents;
    cb.callback = on_scope;
    cb.userdata1 = &w;
    wgpuDevicePopErrorScope(device, cb);
    wait_for(&w);
    if (!w.ok && !first) first = w.message; else free(w.message);
  }
  return first;
}

static WGPUBuffer make_buffer(uint64_t size, WGPUBufferUsage usage) {
  WGPUBufferDescriptor d = WGPU_BUFFER_DESCRIPTOR_INIT;
  d.size = size < 16 ? 16 : (size + 3) / 4 * 4;
  d.usage = usage;
  return wgpuDeviceCreateBuffer(device, &d);
}

/* ---- the calls ---- */

/* Start the GPU: NULL, or why there's none. Once; later calls give the
 * first's answer. */
static char *init_error;
static int initialized;

char *fsgpu_init(void) {
  take_lock();
  if (initialized) {
    char *e = init_error ? copy_string(init_error) : NULL;
    give_lock();
    return e;
  }
  initialized = 1;
  instance = wgpuCreateInstance(NULL);
  if (!instance) { init_error = copy_string("No WebGPU here (wgpu couldn't start)"); goto done; }

  struct waited wa = {0};
  WGPURequestAdapterOptions opts = WGPU_REQUEST_ADAPTER_OPTIONS_INIT;
  opts.powerPreference = WGPUPowerPreference_HighPerformance;
  WGPURequestAdapterCallbackInfo acb = WGPU_REQUEST_ADAPTER_CALLBACK_INFO_INIT;
  acb.mode = WGPUCallbackMode_AllowProcessEvents;
  acb.callback = on_adapter;
  acb.userdata1 = &wa;
  wgpuInstanceRequestAdapter(instance, &opts, acb);
  wait_for(&wa);
  if (!wa.ok || !wa.handle) { init_error = wa.message ? wa.message : copy_string("No GPU adapter"); goto done; }
  adapter = wa.handle;

  /* (as large a storage buffer as the GPU allows, for tables) */
  WGPULimits supported = WGPU_LIMITS_INIT;
  wgpuAdapterGetLimits(adapter, &supported);
  WGPULimits required = WGPU_LIMITS_INIT;
  required.maxStorageBufferBindingSize = supported.maxStorageBufferBindingSize;
  required.maxBufferSize = supported.maxBufferSize;
  max_binding = supported.maxStorageBufferBindingSize;

  struct waited wd = {0};
  WGPUDeviceDescriptor dd = WGPU_DEVICE_DESCRIPTOR_INIT;
  dd.requiredLimits = &required;
  dd.deviceLostCallbackInfo.mode = WGPUCallbackMode_AllowSpontaneous;
  dd.deviceLostCallbackInfo.callback = on_lost;
  dd.uncapturedErrorCallbackInfo.callback = on_uncaptured;
  WGPURequestDeviceCallbackInfo dcb = WGPU_REQUEST_DEVICE_CALLBACK_INFO_INIT;
  dcb.mode = WGPUCallbackMode_AllowProcessEvents;
  dcb.callback = on_device;
  dcb.userdata1 = &wd;
  wgpuAdapterRequestDevice(adapter, &dd, dcb);
  wait_for(&wd);
  if (!wd.ok || !wd.handle) { init_error = wd.message ? wd.message : copy_string("No GPU device"); goto done; }
  device = wd.handle;
  queue = wgpuDeviceGetQueue(device);

  WGPUBindGroupLayoutEntry entries[4];
  WGPUBufferBindingType types[4] = { WGPUBufferBindingType_Uniform, WGPUBufferBindingType_ReadOnlyStorage,
                                     WGPUBufferBindingType_Storage, WGPUBufferBindingType_ReadOnlyStorage };
  for (int k = 0; k < 4; k++) {
    WGPUBindGroupLayoutEntry e = WGPU_BIND_GROUP_LAYOUT_ENTRY_INIT;
    e.binding = k;
    e.visibility = WGPUShaderStage_Compute;
    e.buffer.type = types[k];
    entries[k] = e;
  }
  WGPUBindGroupLayoutDescriptor ld = WGPU_BIND_GROUP_LAYOUT_DESCRIPTOR_INIT;
  ld.entryCount = 4;
  ld.entries = entries;
  layout = wgpuDeviceCreateBindGroupLayout(device, &ld);
  WGPUPipelineLayoutDescriptor pd = WGPU_PIPELINE_LAYOUT_DESCRIPTOR_INIT;
  pd.bindGroupLayoutCount = 1;
  pd.bindGroupLayouts = &layout;
  pipeline_layout = wgpuDeviceCreatePipelineLayout(device, &pd);
  no_tables = make_buffer(16, WGPUBufferUsage_Storage);

done:;
  char *e = init_error ? copy_string(init_error) : NULL;
  give_lock();
  return e;
}

/* A WGSL module, compiled: a pipeline, or NULL and why not (in *err). */
WGPUComputePipeline fsgpu_compile(const char *source, char **err) {
  take_lock();
  WGPUComputePipeline pipeline = NULL;
  *err = NULL;
  if (lost) { *err = copy_string(lost); goto done; }
  scopes_begin();
  WGPUShaderSourceWGSL wgsl = WGPU_SHADER_SOURCE_WGSL_INIT;
  wgsl.code = (WGPUStringView){ source, WGPU_STRLEN };
  WGPUShaderModuleDescriptor md = WGPU_SHADER_MODULE_DESCRIPTOR_INIT;
  md.nextInChain = &wgsl.chain;
  WGPUShaderModule module = wgpuDeviceCreateShaderModule(device, &md);
  WGPUComputePipelineDescriptor cd = WGPU_COMPUTE_PIPELINE_DESCRIPTOR_INIT;
  cd.layout = pipeline_layout;
  cd.compute.module = module;
  cd.compute.entryPoint = (WGPUStringView){ "main", WGPU_STRLEN };
  pipeline = wgpuDeviceCreateComputePipeline(device, &cd);
  *err = scopes_end();
  if (module) wgpuShaderModuleRelease(module);
  if (*err && pipeline) { wgpuComputePipelineRelease(pipeline); pipeline = NULL; }
done:
  give_lock();
  return pipeline;
}

void fsgpu_release_pipeline(WGPUComputePipeline p) {
  take_lock();
  wgpuComputePipelineRelease(p);
  give_lock();
}

/* A buffer of read-only storage with these bytes (a kernel's arguments,
 * or a set of tables, to fill with fsgpu_write): NULL and why not. */
WGPUBuffer fsgpu_storage(uint64_t size, char **err) {
  take_lock();
  WGPUBuffer b = NULL;
  *err = NULL;
  if (lost) { *err = copy_string(lost); goto done; }
  if (size > max_binding) {
    char msg[200];
    snprintf(msg, sizeof msg, "its tables (%llu MB) are more than this GPU can use at once (%llu MB)",
             (unsigned long long)((size + 1048575) / 1048576), (unsigned long long)((max_binding + 1048575) / 1048576));
    *err = copy_string(msg);
    goto done;
  }
  scopes_begin();
  b = make_buffer(size, WGPUBufferUsage_Storage | WGPUBufferUsage_CopyDst);
  *err = scopes_end();
  if (*err) {
    char *e = *err;
    *err = copy_view("the GPU couldn't make room for it: ", (WGPUStringView){ e, WGPU_STRLEN });
    free(e);
    if (b) { wgpuBufferRelease(b); b = NULL; }
  }
done:
  give_lock();
  return b;
}

/* Bytes into a buffer, at an offset (a multiple of 4). */
void fsgpu_write(WGPUBuffer b, uint64_t offset, const uint8_t *bytes, size_t n) {
  take_lock();
  size_t whole = n / 4 * 4;
  if (whole) wgpuQueueWriteBuffer(queue, b, offset, bytes, whole);
  if (n > whole) {   /* (writes are of whole words) */
    uint8_t last[4] = {0};
    memcpy(last, bytes + whole, n - whole);
    wgpuQueueWriteBuffer(queue, b, offset + whole, last, 4);
  }
  give_lock();
}

void fsgpu_release_buffer(WGPUBuffer b) {
  take_lock();
  wgpuBufferDestroy(b);
  wgpuBufferRelease(b);
  give_lock();
}

/* A block: the kernel with its arguments and tables (NULL: none), these
 * parameters (80 bytes), over width x height pixels; its pixels into rgb
 * (3 bytes each, rows), their issues or'ed into *issues. NULL, or why
 * it couldn't. */
char *fsgpu_run(WGPUComputePipeline pipeline, WGPUBuffer args, WGPUBuffer tables,
                const uint8_t *params, uint32_t width, uint32_t height,
                uint8_t *rgb, uint32_t *issues) {
  take_lock();
  char *err = NULL;
  if (lost) { err = copy_string(lost); goto done; }
  uint64_t n = (uint64_t)width * height;
  scopes_begin();
  WGPUBuffer uniform = make_buffer(80, WGPUBufferUsage_Uniform | WGPUBufferUsage_CopyDst);
  wgpuQueueWriteBuffer(queue, uniform, 0, params, 80);
  WGPUBuffer out = make_buffer(4 * n, WGPUBufferUsage_Storage | WGPUBufferUsage_CopySrc);
  WGPUBuffer read = make_buffer(4 * n, WGPUBufferUsage_MapRead | WGPUBufferUsage_CopyDst);
  WGPUBuffer buffers[4] = { uniform, args, out, tables ? tables : no_tables };
  WGPUBindGroupEntry entries[4];
  for (int k = 0; k < 4; k++) {
    WGPUBindGroupEntry e = WGPU_BIND_GROUP_ENTRY_INIT;
    e.binding = k;
    e.buffer = buffers[k];
    entries[k] = e;
  }
  WGPUBindGroupDescriptor bd = WGPU_BIND_GROUP_DESCRIPTOR_INIT;
  bd.layout = layout;
  bd.entryCount = 4;
  bd.entries = entries;
  WGPUBindGroup bind = wgpuDeviceCreateBindGroup(device, &bd);
  WGPUCommandEncoder enc = wgpuDeviceCreateCommandEncoder(device, NULL);
  WGPUComputePassEncoder pass = wgpuCommandEncoderBeginComputePass(enc, NULL);
  wgpuComputePassEncoderSetPipeline(pass, pipeline);
  wgpuComputePassEncoderSetBindGroup(pass, 0, bind, 0, NULL);
  wgpuComputePassEncoderDispatchWorkgroups(pass, (width + 7) / 8, (height + 7) / 8, 1);
  wgpuComputePassEncoderEnd(pass);
  wgpuComputePassEncoderRelease(pass);
  if (n) wgpuCommandEncoderCopyBufferToBuffer(enc, out, 0, read, 0, 4 * n);
  WGPUCommandBuffer cmds = wgpuCommandEncoderFinish(enc, NULL);
  wgpuCommandEncoderRelease(enc);
  wgpuQueueSubmit(queue, 1, &cmds);
  wgpuCommandBufferRelease(cmds);
  err = scopes_end();
  if (!err && n) {
    struct waited w = {0};
    WGPUBufferMapCallbackInfo cb = WGPU_BUFFER_MAP_CALLBACK_INFO_INIT;
    /* (called from whichever thread's poll sees it done) */
    cb.mode = WGPUCallbackMode_AllowSpontaneous;
    cb.callback = on_map;
    cb.userdata1 = &w;
    wgpuBufferMapAsync(read, WGPUMapMode_Read, 0, 4 * n, cb);
    /* (the GPU draws without the lock: other blocks go in meanwhile) */
    give_lock();
    while (!atomic_load(&w.done)) {
      wgpuDevicePoll(device, 0, NULL);
      if (!atomic_load(&w.done)) nap();
    }
    take_lock();
    if (!w.ok) err = w.message;
    else {
      const uint32_t *px = wgpuBufferGetConstMappedRange(read, 0, 4 * n);
      uint32_t acc = 0;
      for (uint64_t k = 0; k < n; k++) {
        uint32_t p = px[k];
        rgb[3 * k] = p & 255; rgb[3 * k + 1] = (p >> 8) & 255; rgb[3 * k + 2] = (p >> 16) & 255;
        acc |= p >> 24;
      }
      *issues = acc;
      wgpuBufferUnmap(read);
    }
  }
  if (!err && lost) err = copy_string(lost);
  wgpuBindGroupRelease(bind);
  wgpuBufferDestroy(uniform); wgpuBufferRelease(uniform);
  wgpuBufferDestroy(out); wgpuBufferRelease(out);
  wgpuBufferDestroy(read); wgpuBufferRelease(read);
done:
  give_lock();
  return err;
}

/* What GPU this is ("" before fsgpu_init succeeds): for messages. */
char *fsgpu_name(void) {
  take_lock();
  char *s;
  if (!adapter) s = copy_string("");
  else {
    WGPUAdapterInfo info = WGPU_ADAPTER_INFO_INIT;
    wgpuAdapterGetInfo(adapter, &info);
    s = copy_view("", info.device);
    wgpuAdapterInfoFreeMembers(info);
  }
  give_lock();
  return s;
}
