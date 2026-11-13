// Where a project's windows go on the desk: as saved (as fractions of
// the desk, so that a layout made on one screen fits another), or side by
// side to begin with; shrunk, all alike, when they don't fit; and on a
// phone, tiled over the page (down it, or across it, or in a grid:
// whichever makes them biggest), so that every viewer's in view at once.
//
// A place is where a window's content (its body, under its title bar)
// goes, in CSS pixels: its window's top-left corner, and its body's size.

export interface Place { x: number; y: number; width: number; height: number }

/** The part of the desk windows go in: below the project's title, inside
    a margin. */
export interface Room { left: number; top: number; width: number; height: number }

/** A window's title bar's height, and the gap between windows. */
export const TITLE_BAR = 26;
export const GAP = 12;
/** The smallest a window's body is made, fitting windows to the desk. */
const MIN_SIDE = 96;

/** A phone's screen (narrow, or turned sideways, short): windows are
    tiled, not placed. */
export const narrowScreen = window.matchMedia("(max-width: 720px), (max-height: 480px)");

/** Windows side by side, in as many rows as lets them be biggest (each
    row filled in turn), at the sizes given or smaller (all alike). */
export function sideBySide(sizes: { width: number; height: number }[], room: Room): Place[] {
  const n = sizes.length;
  if (n === 0) return [];
  let best: { scale: number; rows: number[][] } | null = null;
  for (let r = 1; r <= n; r++) {
    const perRow = Math.ceil(n / r);
    const rows: number[][] = [];
    for (let i = 0; i < n; i += perRow) rows.push([...Array(Math.min(perRow, n - i)).keys()].map((k) => i + k));
    const widest = Math.max(...rows.map((row) => row.reduce((w, i) => w + sizes[i].width, 0)));
    const tallest = rows.reduce((h, row) => h + Math.max(...row.map((i) => sizes[i].height)), 0);
    const gapsX = (perRow - 1) * GAP, gapsY = (rows.length - 1) * GAP + rows.length * TITLE_BAR;
    const scale = Math.min(1, (room.width - gapsX) / widest, (room.height - gapsY) / tallest);
    if (!best || scale > best.scale + 1e-9) best = { scale, rows };
  }
  const { scale, rows } = best!;
  const places: Place[] = new Array(n);
  let y = room.top;
  for (const row of rows) {
    let x = room.left, rowHeight = 0;
    for (const i of row) {
      const w = Math.max(MIN_SIDE, Math.floor(sizes[i].width * scale));
      const h = Math.max(MIN_SIDE, Math.floor(sizes[i].height * scale));
      places[i] = { x, y, width: w, height: h };
      x += w + GAP;
      rowHeight = Math.max(rowHeight, h);
    }
    y += rowHeight + TITLE_BAR + GAP;
  }
  return places;
}

/** Windows as they were placed, shrunk all alike (toward the room's
    corner) if they don't reach no further right and down than it; else as
    they are, but moved onto the desk if they're off its top or left.
    (Keeping them out from under the title is the desk's job.)
    (`minimized`: which are just their title bars.) */
export function fitted(places: Place[], room: Room, minimized: boolean[] = []): Place[] {
  if (places.length === 0) return [];
  const bodyHeight = (p: Place, i: number) => (minimized[i] ? 0 : p.height);
  const right = Math.max(...places.map((p) => p.x + p.width));
  const bottom = Math.max(...places.map((p, i) => p.y + TITLE_BAR + bodyHeight(p, i)));
  const left = Math.min(...places.map((p) => p.x)), top = Math.min(...places.map((p) => p.y));
  const shiftX = Math.max(0, -left), shiftY = Math.max(0, -top);
  const fits = right + shiftX <= room.left + room.width && bottom + shiftY <= room.top + room.height;
  if (fits) return places.map((p) => ({ ...p, x: p.x + shiftX, y: p.y + shiftY }));
  // (from the room's corner: where the windows start, and how much room
  // they have from there; title bars don't shrink)
  const x0 = Math.max(left, room.left), y0 = Math.max(top, room.top);
  const scale = Math.min(1, (room.left + room.width - x0) / (right - left),
                         (room.top + room.height - y0) / (bottom - top));
  return places.map((p) => ({
    x: Math.round(x0 + (p.x - left) * scale),
    y: Math.round(y0 + (p.y - top) * scale),
    width: Math.max(MIN_SIDE, Math.floor(p.width * scale)),
    height: Math.max(MIN_SIDE, Math.floor((p.height + TITLE_BAR) * scale) - TITLE_BAR),
  }));
}

/** On a phone: the windows tiled over the room, in as many columns as
    makes them biggest (their shorter sides longest): down it, upright,
    across it, turned sideways, or a grid. Minimized ones are only their
    title bars, in a row of their own under the rest. None is less than
    `MIN_TILE` high (or as wide as it is, if that's less): if they don't
    fit, the page scrolls, down. */
const MIN_TILE = 200;
export function tiled(minimized: boolean[], room: Room): Place[] {
  const open = minimized.filter((m) => !m).length;
  const shut = minimized.length - open;
  // (the minimized ones' row, under the rest)
  const shutRows = shut ? TITLE_BAR + GAP : 0;
  const cell = (columns: number) => {
    const rows = Math.ceil(open / columns);
    const width = Math.floor((room.width - (columns - 1) * GAP) / columns);
    const height = Math.floor((room.height - shutRows - rows * TITLE_BAR - (rows - 1) * GAP) / rows);
    return { columns, width, height };
  };
  let best = cell(1);
  for (let columns = 2; columns <= Math.max(1, open); columns++) {
    const c = cell(columns);
    if (Math.min(c.width, c.height) > Math.min(best.width, best.height)) best = c;
  }
  const width = best.width, height = Math.max(Math.min(MIN_TILE, width), best.height);
  const shutWidth = shut ? Math.floor((room.width - (shut - 1) * GAP) / shut) : 0;
  let k = 0, s = 0;
  const openRows = open ? Math.ceil(open / best.columns) : 0;
  const shutTop = room.top + openRows * (TITLE_BAR + height + GAP);
  return minimized.map((m) => {
    if (m) {
      const p = { x: room.left + s * (shutWidth + GAP), y: shutTop, width: shutWidth, height };
      s++;
      return p;
    }
    const col = k % best.columns, row = Math.floor(k / best.columns);
    k++;
    return { x: room.left + col * (width + GAP), y: room.top + row * (TITLE_BAR + height + GAP), width, height };
  });
}

/** A place as fractions of the desk (to save): its whole window's box,
    title bar and all (so that windows that don't overlap on one desk
    don't on another); and back: as big as fits in that box, with the
    window's shape (its body's width over its height). */
export function toFractions(p: Place, desk: { width: number; height: number }): [number, number, number, number] {
  const r = (x: number) => Math.round(x * 10000) / 10000;
  return [r(p.x / desk.width), r(p.y / desk.height), r(p.width / desk.width), r((p.height + TITLE_BAR) / desk.height)];
}

export function fromFractions(f: number[], shape: number, desk: { width: number; height: number }): Place {
  const [fx, fy, fw, fh] = f;
  const boxW = fw * desk.width, boxH = fh * desk.height - TITLE_BAR;
  const width = Math.min(boxW, boxH * shape), height = width / shape;
  return { x: Math.round(fx * desk.width), y: Math.round(fy * desk.height),
           width: Math.max(MIN_SIDE, Math.round(width)), height: Math.max(MIN_SIDE, Math.round(height)) };
}
