// Compile GPU shaders with naga, the WGSL compiler in Firefox (and Deno's
// WebGPU: wgpu), through to a compute pipeline with FractalStream's bindings,
// and print any errors. naga is stricter than Chrome's and Safari's
// compilers in places (it can't bitcast in a const), and Playwright's
// Firefox has no WebGPU on macOS, so this stands in for Firefox:
//
//   (cd fractalstream-backend-wgsl &&
//    stack exec --package hspec -- runghc -itest ../dev-tools/gpu/dump-shaders.hs DIR LIST)
//   deno run --allow-read dev-tools/gpu/naga-check.ts DIR/*.wgsl
//
// (LIST: a file of project paths, one a line: every viewer's shaders, in
// both precisions, go to DIR.)
const adapter = await navigator.gpu.requestAdapter();
const device = await adapter!.requestDevice();
const layout = device.createBindGroupLayout({ entries: [
  { binding: 0, visibility: GPUShaderStage.COMPUTE, buffer: { type: "uniform" } },
  { binding: 1, visibility: GPUShaderStage.COMPUTE, buffer: { type: "read-only-storage" } },
  { binding: 2, visibility: GPUShaderStage.COMPUTE, buffer: { type: "storage" } },
  { binding: 3, visibility: GPUShaderStage.COMPUTE, buffer: { type: "read-only-storage" } }] });
const pipelineLayout = device.createPipelineLayout({ bindGroupLayouts: [layout] });
let ok = 0;
for (const file of Deno.args) {
  device.pushErrorScope("validation");
  device.pushErrorScope("internal");
  let thrown = "";
  try {
    const module = device.createShaderModule({ code: await Deno.readTextFile(file) });
    await device.createComputePipelineAsync({ layout: pipelineLayout, compute: { module, entryPoint: "main" } });
  } catch (e) { thrown = String((e as Error).message ?? e); }
  const internal = await device.popErrorScope();
  const invalid = await device.popErrorScope();
  const msg = [thrown, internal?.message, invalid?.message].filter(Boolean).join(" | ");
  if (msg) console.log(`${file.split("/").pop()}: ${msg.split("\n").slice(0, 6).join(" / ")}`); else ok++;
}
console.log(`${ok} of ${Deno.args.length} made pipelines`);
