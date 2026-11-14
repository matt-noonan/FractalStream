// The GPU, for the GPU backends (fractalstream-backend-wgsl, through
// Backend.Wasm.Gpu): the host (the engine's worker) makes one and puts it
// at globalThis.fractalstreamGpu before starting the engine.
//
//   const gpu = await createGpu()
//
// gpu.error              why there's no GPU (then nothing else is there)
// gpu.compile(source)    -> Promise<kernel | { error }>   a WGSL module
//                        (or the compiler's messages, if it won't compile)
// gpu.bind(kernel, words) -> args   a kernel with its arguments (a Uint32Array)
// gpu.run(args, params, width, height) -> Promise<Uint8Array | { error }>
//                        a block: the parameters (80 bytes), and the
//                        block's pixels back, as RGB rows (or why not: the
//                        GPU failed, or was lost)
//
// Every kernel has the same bindings (see Backend.WGSL.Runtime): the
// parameters (a uniform), the arguments (read-only storage), and the
// pixels (storage, a u32 each, 0x00BBGGRR).

export async function createGpu() {
  const gpu = globalThis.navigator?.gpu;
  if (!gpu) return { error: "No WebGPU in this browser" };
  let adapter;
  try {
    adapter = await gpu.requestAdapter({ powerPreference: "high-performance" });
  } catch (e) {
    return { error: `No GPU: ${e.message ?? e}` };
  }
  if (!adapter) return { error: "No GPU adapter in this browser" };
  // (as large a storage buffer as the GPU allows, for tables later)
  const device = await adapter.requestDevice({
    requiredLimits: { maxStorageBufferBindingSize: adapter.limits.maxStorageBufferBindingSize },
  });
  let lost = null;
  device.lost.then((info) => { lost = `The GPU was lost: ${info.message || info.reason}`; });

  const layout = device.createBindGroupLayout({ entries: [
    { binding: 0, visibility: GPUShaderStage.COMPUTE, buffer: { type: "uniform" } },
    { binding: 1, visibility: GPUShaderStage.COMPUTE, buffer: { type: "read-only-storage" } },
    { binding: 2, visibility: GPUShaderStage.COMPUTE, buffer: { type: "storage" } }] });
  const pipelineLayout = device.createPipelineLayout({ bindGroupLayouts: [layout] });

  // Run GPU work, failing on any validation error (which WebGPU otherwise
  // only reports to the console).
  async function checked(work) {
    device.pushErrorScope("validation");
    device.pushErrorScope("out-of-memory");
    let result, failure;
    try { result = await work(); } catch (e) { failure = e; }
    const oom = await device.popErrorScope();
    const invalid = await device.popErrorScope();
    if (failure) throw failure;
    if (oom) throw new Error(`out of GPU memory: ${oom.message}`);
    if (invalid) throw new Error(invalid.message);
    return result;
  }

  // (a promise's failure as its result, { error })
  const failure = (p) => p.catch((e) => ({ error: String(e?.message ?? e) }));

  return {
    compile(code) {
      if (lost) return Promise.resolve({ error: lost });
      return failure(checked(async () => {
        const module = device.createShaderModule({ code });
        const info = await module.getCompilationInfo();
        const errors = info.messages.filter((m) => m.type === "error");
        if (errors.length) throw new Error(errors.map((m) => `${m.lineNum}:${m.linePos} ${m.message}`).join("; "));
        return device.createComputePipelineAsync({ layout: pipelineLayout, compute: { module, entryPoint: "main" } });
      }));
    },

    bind(pipeline, words) {
      const buffer = device.createBuffer({ size: Math.max(16, words.byteLength), usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST });
      device.queue.writeBuffer(buffer, 0, words);
      return { pipeline, buffer };
    },

    run(args, params, width, height) {
      if (lost) return Promise.resolve({ error: lost });
      const n = width * height;
      return failure(checked(async () => {
        const uniform = device.createBuffer({ size: params.byteLength, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST });
        device.queue.writeBuffer(uniform, 0, params);
        const out = device.createBuffer({ size: Math.max(16, 4 * n), usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_SRC });
        const read = device.createBuffer({ size: Math.max(16, 4 * n), usage: GPUBufferUsage.MAP_READ | GPUBufferUsage.COPY_DST });
        try {
          const bind = device.createBindGroup({ layout, entries: [
            { binding: 0, resource: { buffer: uniform } },
            { binding: 1, resource: { buffer: args.buffer } },
            { binding: 2, resource: { buffer: out } }] });
          const enc = device.createCommandEncoder();
          const pass = enc.beginComputePass();
          pass.setPipeline(args.pipeline);
          pass.setBindGroup(0, bind);
          pass.dispatchWorkgroups(Math.ceil(width / 8), Math.ceil(height / 8));
          pass.end();
          enc.copyBufferToBuffer(out, 0, read, 0, 4 * n);
          device.queue.submit([enc.finish()]);
          await read.mapAsync(GPUMapMode.READ);
          const pixels = new Uint32Array(read.getMappedRange(), 0, n);
          const rgb = new Uint8Array(3 * n);
          for (let i = 0, j = 0; i < n; i++, j += 3) {
            const p = pixels[i];
            rgb[j] = p & 255; rgb[j + 1] = (p >>> 8) & 255; rgb[j + 2] = (p >>> 16) & 255;
          }
          read.unmap();
          return rgb;
        } finally {
          uniform.destroy(); out.destroy(); read.destroy();
        }
      }));
    },
  };
}
