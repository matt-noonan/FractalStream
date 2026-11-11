// Opening and saving project files.
//
// Where the browser has the File System Access API (Chrome, Edge), a
// project opened from a file can be saved back to it (after the browser
// asks permission), and "Save as" asks where to save. Elsewhere (Safari,
// Firefox), opening uses a file input, and saving downloads the file.
// Projects from the library are only ever saved as copies.
//
// Pickers and permission prompts need the user's click, so they come
// first; then the engine is asked for the file's contents, and given
// `write`.

const projectTypes = [{ description: "FractalStream projects", accept: { "application/x-fractalstream": [".fs"] } }];

// (not in TypeScript's DOM types yet)
export interface FileHandle {
  name: string;
  getFile(): Promise<File>;
  createWritable(): Promise<{ write(data: BufferSource): Promise<void>; close(): Promise<void> }>;
  queryPermission?(options: { mode: "readwrite" }): Promise<PermissionState>;
  requestPermission?(options: { mode: "readwrite" }): Promise<PermissionState>;
}
const w = window as unknown as {
  showOpenFilePicker?: (options: unknown) => Promise<FileHandle[]>;
  showSaveFilePicker?: (options: unknown) => Promise<FileHandle>;
};

export interface Opened {
  file: File;
  /** Where to save it back, if the browser allows that. */
  handle: FileHandle | null;
}

/** Ask the user for a project file to open (null if they don't pick one). */
export async function pickFile(): Promise<Opened | null> {
  if (w.showOpenFilePicker) {
    try {
      const [handle] = await w.showOpenFilePicker({ types: projectTypes });
      return { file: await handle.getFile(), handle };
    } catch (e) {
      if ((e as Error).name === "AbortError") return null;
      throw e;
    }
  }
  const input = Object.assign(document.createElement("input"), { type: "file", accept: ".fs" });
  return new Promise((resolve) => {
    input.addEventListener("change", () => resolve(input.files?.[0] ? { file: input.files[0], handle: null } : null));
    input.addEventListener("cancel", () => resolve(null));
    input.click();
  });
}

/** A file dropped on the page, with its handle if the browser gives one. */
export async function droppedFile(e: DragEvent): Promise<Opened | null> {
  const item = e.dataTransfer?.items?.[0] as (DataTransferItem & { getAsFileSystemHandle?: () => Promise<FileHandle | null> }) | undefined;
  const file = e.dataTransfer?.files?.[0];
  if (!file) return null;
  let handle: FileHandle | null = null;
  try { handle = (await item?.getAsFileSystemHandle?.()) ?? null; } catch { handle = null; }
  return { file, handle };
}

/**
 * Where to save, chosen now (while the user's click counts): a function
 * that writes the file's contents there, or null if the user cancelled.
 * `handle` is where the project came from (to save back to, unless
 * `saveAs`); `onSavedTo` hears of a new place it was saved to.
 */
export async function chooseDestination(handle: FileHandle | null, saveAs: boolean, suggestedName: string,
                                        onSavedTo: (handle: FileHandle) => void)
    : Promise<((name: string, bytes: ArrayBuffer) => Promise<string>) | null> {
  if (handle && !saveAs) {
    if (handle.requestPermission && (await handle.requestPermission({ mode: "readwrite" })) !== "granted") return null;
    return async (_name, bytes) => { await write(handle, bytes); return handle.name; };
  }
  if (w.showSaveFilePicker) {
    let chosen: FileHandle;
    try {
      chosen = await w.showSaveFilePicker({ suggestedName, types: projectTypes });
    } catch (e) {
      if ((e as Error).name === "AbortError") return null;
      throw e;
    }
    return async (_name, bytes) => { await write(chosen, bytes); onSavedTo(chosen); return chosen.name; };
  }
  return async (name, bytes) => { download(name, bytes); return name; };
}

async function write(handle: FileHandle, bytes: ArrayBuffer) {
  const writable = await handle.createWritable();
  await writable.write(bytes);
  await writable.close();
}

function download(name: string, bytes: ArrayBuffer) {
  const url = URL.createObjectURL(new Blob([bytes], { type: "application/x-fractalstream" }));
  const a = Object.assign(document.createElement("a"), { href: url, download: name });
  document.body.append(a);
  a.click();
  a.remove();
  setTimeout(() => URL.revokeObjectURL(url), 10000);
}
