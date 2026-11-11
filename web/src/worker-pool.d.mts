// Types for ../../fractalstream-backend-wasm/js/worker-pool.mjs, which the
// build copies next to engine.js.
export interface PoolWorker {
  post(message: unknown, transfer: Transferable[]): void;
  onMessage(callback: (message: any) => void): void;
  terminate(): void;
}

export interface Pool {
  size: number;
  addKernel(bytes: Uint8Array): number;
  run(...args: unknown[]): Promise<unknown>;
  terminate(): void;
}

export function createPool(workers: PoolWorker[]): Pool;
