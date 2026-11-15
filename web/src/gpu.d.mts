// Types for ../../fractalstream-backend-wasm/js/gpu.mjs, which the build
// copies next to engine.js.
export interface Gpu {
  /** Why there's no GPU (then nothing else is there). */
  error?: string;
}

/** The GPU, for the GPU backends (or why there's none). */
export function createGpu(): Promise<Gpu>;
