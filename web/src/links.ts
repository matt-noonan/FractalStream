// Links to a project, as it is: the part of the page's address after
// "#", which the browser never sends to the server. Three kinds:
//
//   #open=<library id>&s=<state>       a library project, its script as
//                                      the library has it, with a state:
//                                      its values, views, colorizers and
//                                      autocolor (compressed JSON)
//   #project=<file>&name=<file name>   a whole project file (compressed)
//   #url=<address>                     a project file on the web (where
//                                      its host lets other pages read it)
//
// Compressed with the browser's own deflate, in base64url.

import type { Message } from "./protocol.js";

export type Link =
  | { kind: "library"; id: string; state: string }
  | { kind: "project"; name: string; bytes: Uint8Array }
  | { kind: "url"; url: string };

async function squeeze(bytes: Uint8Array, how: "compress" | "decompress"): Promise<Uint8Array> {
  const stream = new Blob([bytes]).stream()
    .pipeThrough(how === "compress" ? new CompressionStream("deflate-raw") : new DecompressionStream("deflate-raw"));
  return new Uint8Array(await new Response(stream).arrayBuffer());
}

function toBase64Url(bytes: Uint8Array): string {
  let s = "";
  for (let i = 0; i < bytes.length; i += 0x8000) s += String.fromCharCode(...bytes.subarray(i, i + 0x8000));
  return btoa(s).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}

function fromBase64Url(text: string): Uint8Array {
  const s = atob(text.replace(/-/g, "+").replace(/_/g, "/"));
  return Uint8Array.from(s, (c) => c.charCodeAt(0));
}

/** A link to what the engine said a link shows (see "share"). */
export async function makeLink(m: Extract<Message, { type: "share" }>): Promise<string> {
  const base = location.origin + location.pathname + location.search;
  if (m.library !== null && m.state !== null) {
    const state = await squeeze(new TextEncoder().encode(JSON.stringify(m.state)), "compress");
    return `${base}#open=${m.library}&s=${toBase64Url(state)}`;
  }
  const file = await squeeze(new TextEncoder().encode(m.file ?? ""), "compress");
  return `${base}#project=${toBase64Url(file)}&name=${encodeURIComponent(m.fileName)}`;
}

/** The link in an address's "#" part, if there is one: a link this page
    made, or a project file's address. (Throws if it's one that can't be
    read.) */
export async function readLink(hash: string): Promise<Link | null> {
  const params = new URLSearchParams(hash.replace(/^#/, ""));
  const open = params.get("open"), project = params.get("project"), url = params.get("url");
  if (open) {
    const state = params.get("s");
    return { kind: "library", id: open,
             state: state ? new TextDecoder().decode(await squeeze(fromBase64Url(state), "decompress")) : "{}" };
  }
  if (project) {
    return { kind: "project", name: params.get("name") || "Shared project.fs",
             bytes: await squeeze(fromBase64Url(project), "decompress") };
  }
  if (url) return { kind: "url", url };
  return null;
}

/** Whether an address's "#" part has a link in it (without reading it). */
export function hasLink(hash: string): boolean {
  const params = new URLSearchParams(hash.replace(/^#/, ""));
  return params.has("open") || params.has("project") || params.has("url");
}

/** A project file from the web, by its address: its name and contents. */
export async function fetchProject(url: string): Promise<{ name: string; bytes: ArrayBuffer }> {
  const response = await fetch(url);
  if (!response.ok) throw new Error(`the server said ${response.status} ${response.statusText}`);
  const last = decodeURIComponent(new URL(url, location.href).pathname.split("/").pop() || "");
  return { name: /\.fs$/i.test(last) ? last : "Shared project.fs", bytes: await response.arrayBuffer() };
}
