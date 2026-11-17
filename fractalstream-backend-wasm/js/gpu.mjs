// The GPU, for the GPU backends (fractalstream-backend-wgsl, through
// Backend.Wasm.Gpu): the host (the engine's worker) makes one and puts it
// at globalThis.fractalstreamGpu before starting the engine.
//
//   const gpu = await createGpu()
//
// gpu.error              why there's no GPU (then nothing else is there)
// gpu.compile(source)    -> Promise<kernel | { error }>   a WGSL module
//                        (or the compiler's messages, if it won't compile)
// gpu.hasTables(key)    whether it has the tables with this key (their
//                        versions and places: see Backend.Wasm.Gpu)
// gpu.newTables(key, size) -> error | null   room for them (size bytes)
// gpu.writeTable(key, offset, bytes)  a table's elements, into that room
// gpu.bind(kernel, words, key) -> args   a kernel with its arguments (a
//                        Uint32Array) and those tables
// gpu.run(args, params, width, height) -> Promise<{ pixels, issues } | { error }>
//                        a block: the parameters (80 bytes), and the
//                        block's pixels back, as RGB rows, with their
//                        issues (or'ed); or why not (the GPU failed, or
//                        was lost)
//
// Every kernel has the same bindings (see Backend.WGSL.Runtime): the
// parameters (a uniform), the arguments (read-only storage), the pixels
// (storage, a u32 each, 0xIIBBGGRR), and the tables (read-only storage).

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
    { binding: 2, visibility: GPUShaderStage.COMPUTE, buffer: { type: "storage" } },
    { binding: 3, visibility: GPUShaderStage.COMPUTE, buffer: { type: "read-only-storage" } }] });
  const pipelineLayout = device.createPipelineLayout({ bindGroupLayouts: [layout] });

  // Run GPU work, failing on any validation error (which WebGPU otherwise
  // only reports to the console).
  async function checked(work) {
    device.pushErrorScope("validation");
    device.pushErrorScope("out-of-memory");
    let result, failure;
    try { result = await work(); } catch (e) { failure = e; }
    // (waits on the GPU too: see awaitingGpu)
    const oom = await awaitingGpu(device.popErrorScope());
    const invalid = await awaitingGpu(device.popErrorScope());
    if (failure) throw failure;
    if (oom) throw new Error(`out of GPU memory: ${oom.message}`);
    if (invalid) throw new Error(invalid.message);
    return result;
  }

  // The tables of the last few sets of them, by key (each set in one
  // buffer, a table's elements at its offset); none: a small empty one.
  const tableSets = new Map();
  const noTables = device.createBuffer({ size: 16, usage: GPUBufferUsage.STORAGE });
  const keepSets = 2;

  // (a promise's failure as its result, { error })
  const failure = (p) => p.catch((e) => ({ error: String(e?.message ?? e) }));

  // Firefox finds that GPU work has finished (resolving mapAsync) only
  // every 100 ms, unless something wakes its device: which caps a viewer
  // at 10 frames a second, however little it draws. A submit does, even an
  // empty one; so while any block waits for its pixels, an empty submit
  // goes every few milliseconds (the wait is then about 8 ms). Elsewhere
  // it's harmless, and costs next to nothing.
  let waiting = 0;
  let nudging = null;
  const nudge = () => {
    if (waiting > 0 && !lost) device.queue.submit([]);
    else { clearInterval(nudging); nudging = null; }
  };
  async function awaitingGpu(promise) {
    waiting++;
    if (nudging === null) nudging = setInterval(nudge, 4);
    try { return await promise; } finally { waiting--; }
  }

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

    hasTables(key) { return key === "" || tableSets.has(key); },

    newTables(key, size) {
      const limit = device.limits.maxStorageBufferBindingSize;
      if (size > limit) {
        const mb = (n) => `${Math.ceil(n / 1048576)} MB`;
        return `its tables (${mb(size)}) are more than this GPU can use at once (${mb(limit)})`;
      }
      // (the oldest set goes, to keep a few)
      while (tableSets.size >= keepSets) {
        const [oldest, buffer] = tableSets.entries().next().value;
        buffer.destroy();
        tableSets.delete(oldest);
      }
      try {
        tableSets.set(key, device.createBuffer({ size: Math.max(16, Math.ceil(size / 4) * 4),
                                                 usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST }));
      } catch (e) {
        return `the GPU couldn't make room for its tables: ${e.message ?? e}`;
      }
      return null;
    },

    writeTable(key, offset, bytes) {
      const padded = bytes.length % 4 === 0 ? bytes : (() => {
        const p = new Uint8Array(Math.ceil(bytes.length / 4) * 4);
        p.set(bytes);
        return p;
      })();
      if (padded.length > 0) device.queue.writeBuffer(tableSets.get(key), offset, padded);
    },

    bind(pipeline, words, key) {
      const buffer = device.createBuffer({ size: Math.max(16, words.byteLength), usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST });
      device.queue.writeBuffer(buffer, 0, words);
      return { pipeline, buffer, key };
    },

    run(args, params, width, height) {
      if (lost) return Promise.resolve({ error: lost });
      const tables = args.key === "" ? noTables : tableSets.get(args.key);
      if (!tables) return Promise.resolve({ error: "its tables were dropped" });
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
            { binding: 2, resource: { buffer: out } },
            { binding: 3, resource: { buffer: tables } }] });
          const enc = device.createCommandEncoder();
          const pass = enc.beginComputePass();
          pass.setPipeline(args.pipeline);
          pass.setBindGroup(0, bind);
          pass.dispatchWorkgroups(Math.ceil(width / 8), Math.ceil(height / 8));
          pass.end();
          enc.copyBufferToBuffer(out, 0, read, 0, 4 * n);
          device.queue.submit([enc.finish()]);
          await awaitingGpu(read.mapAsync(GPUMapMode.READ));
          const pixels = new Uint32Array(read.getMappedRange(), 0, n);
          const rgb = new Uint8Array(3 * n);
          let issues = 0;
          for (let i = 0, j = 0; i < n; i++, j += 3) {
            const p = pixels[i];
            rgb[j] = p & 255; rgb[j + 1] = (p >>> 8) & 255; rgb[j + 2] = (p >>> 16) & 255;
            issues |= p >>> 24;
          }
          read.unmap();
          return { pixels: rgb, issues };
        } finally {
          uniform.destroy(); out.destroy(); read.destroy();
        }
      }));
    },
  };
}
