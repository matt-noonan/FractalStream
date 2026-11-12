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
  runTool(...args: unknown[]): Promise<unknown>;
  hasTable(version: number): boolean;
  addTable(version: number, bytes: Uint8Array): void;
  /** Stop the tool jobs with this tag (replacing a worker running one). */
  stop(tag: number): void;
  terminate(): void;
}

/** A pool of the given workers; `makeWorker` makes one to replace a
    stopped worker. */
export function createPool(workers: PoolWorker[], makeWorker: () => PoolWorker): Pool;
