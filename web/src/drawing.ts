// A viewer's drawings (what its tools and overlay drew), over the picture,
// with Canvas2D: as Actor.Viewer.Overlay draws them for fsrun's captures,
// and the desktop app draws them on screen. The shapes come from the
// engine (fractalstream-ui-web/src/Drawing.hs) in screen-plane
// coordinates, and are placed here in the view the page shows, so they
// move with it at once.
//
// A shape can reach very far from the view: the next step of an escaping
// orbit, or a line from 0 to 10^10 at any zoom, which is about 10^12
// pixels away. Canvas2D's handling of such coordinates varies (it can be
// slow, or draw the wrong pixels), so a shape is placed in pixels in
// double precision, drawn as it is if it stays near the view, and
// otherwise first cut down to the part of it near the view: the same
// clipping as Actor.Viewer.Clip, so its pixels on screen are those it
// would have had.

import type { Shape } from "./protocol.js";

export type Px = [number, number];

/** Where a screen-plane point goes on the canvas. */
export type ToPx = (p: [number, number]) => Px;

interface Box { x0: number; y0: number; x1: number; y1: number }

/** How far outside the view a shape can reach and be drawn as it is. */
const NEAR_DISTANCE = 1e4;

const grow = (m: number, b: Box): Box => ({ x0: b.x0 - m, y0: b.y0 - m, x1: b.x1 + m, y1: b.y1 + m });
const inBox = (b: Box, [x, y]: Px) => x >= b.x0 && x <= b.x1 && y >= b.y0 && y <= b.y1;
const finite = ([x, y]: Px) => Number.isFinite(x) && Number.isFinite(y);

/** Draw a viewer's layers. `scale` is canvas pixels per CSS pixel (pens,
    dots and text are sized in CSS pixels, as on the desktop); `pixelSize`
    is a canvas pixel's width and height in the screen plane. */
export function drawLayers(ctx: CanvasRenderingContext2D, layers: Shape[][], toPx: ToPx,
                           pixelSize: [number, number], scale: number) {
  const W = ctx.canvas.width, H = ctx.canvas.height;
  const image: Box = { x0: 0, y0: 0, x1: W, y1: H };
  const near = grow(NEAR_DISTANCE, image);
  // where a shape is cut off: far enough outside that the pen along the
  // cut doesn't show
  const cut = grow(4 * scale, image);
  const isNear = (pts: Px[]) => pts.every((p) => inBox(near, p));

  ctx.save();
  ctx.lineWidth = 2 * scale;
  ctx.lineCap = "round";
  ctx.lineJoin = "round";
  ctx.font = `${(10 * 96 / 72) * scale}px "DejaVu Sans", sans-serif`;

  // add a polyline (cut down, if it reaches far) to the current path
  const tracePolyline = (pts: Px[]) => {
    if (pts.length < 2 || !pts.every(finite)) return;
    const runs = isNear(pts) ? [pts] : clipPolyline(cut, pts);
    for (const run of runs) {
      ctx.moveTo(run[0][0], run[0][1]);
      for (let i = 1; i < run.length; i++) ctx.lineTo(run[i][0], run[i][1]);
    }
  };
  const strokePolyline = (pts: Px[]) => {
    ctx.beginPath();
    tracePolyline(pts);
    ctx.stroke();
  };
  // fill a polygon and outline it in the pen (as near the view, or the
  // part of it that is: the cut's edges are outside the image)
  const fillPolygon = (pts: Px[]) => {
    if (!pts.every(finite)) return;
    const poly = isNear(pts) ? pts : clipPolygon(cut, pts);
    if (poly.length < 3) return;
    ctx.beginPath();
    ctx.moveTo(poly[0][0], poly[0][1]);
    for (let i = 1; i < poly.length; i++) ctx.lineTo(poly[i][0], poly[i][1]);
    ctx.closePath();
    ctx.fill();
    ctx.stroke();
  };
  const nearPoint = (p: Px) => finite(p) && inBox(near, p);

  for (const layer of layers) {
    for (let i = 0; i < layer.length; i++) {
      const s = layer[i];
      ctx.strokeStyle = s.s;
      switch (s.k) {
        case "point": {
          const p = toPx(s.p);
          if (!nearPoint(p)) break;
          ctx.fillStyle = s.s;
          ctx.beginPath();
          ctx.arc(p[0], p[1], 2 * scale, 0, 2 * Math.PI);
          ctx.fill();
          ctx.stroke();
          break;
        }
        case "line": {
          // (a run of lines in one color is one path: a traced orbit is
          // thousands of them; each is its own subpath, with its own caps)
          ctx.beginPath();
          tracePolyline([toPx(s.a), toPx(s.b)]);
          for (let n = layer[i + 1]; n?.k === "line" && n.s === s.s; n = layer[i + 1]) {
            tracePolyline([toPx(n.a), toPx(n.b)]);
            i++;
          }
          ctx.stroke();
          break;
        }
        case "runs":
          for (const run of s.runs) strokePolyline(run.map(toPx));
          break;
        case "polygon":
          ctx.fillStyle = s.f;
          fillPolygon(s.pts.map(toPx));
          break;
        case "rect": {
          const [x1, y1] = toPx(s.a), [x2, y2] = toPx(s.b);
          const corners: Px[] = [[x1, y1], [x2, y1], [x2, y2], [x1, y2]];
          if (s.f) { ctx.fillStyle = s.f; fillPolygon(corners); }
          else strokePolyline([...corners, corners[0]]);
          break;
        }
        case "circle": {
          // a circle in the model: an ellipse, when pixels aren't square
          const c = toPx(s.c);
          const rx = Math.abs(s.r / pixelSize[0]), ry = Math.abs(s.r / pixelSize[1]);
          if (!finite(c) || !Number.isFinite(rx) || !Number.isFinite(ry)) break;
          if (s.f) ctx.fillStyle = s.f;
          if (isNear([[c[0] - rx, c[1] - ry], [c[0] + rx, c[1] + ry]])) {
            ctx.beginPath();
            ctx.ellipse(c[0], c[1], rx, ry, 0, 0, 2 * Math.PI);
            if (s.f) ctx.fill();
            ctx.stroke();
            break;
          }
          const arc = ellipseNear(cut, c, rx, ry);
          if (arc === null) break;
          if (arc.length === 0) { if (s.f) fillPolygon(boxCorners(cut)); }
          else if (s.f) fillPolygon([...arc, c]);
          else strokePolyline(arc);
          break;
        }
        case "text": {
          const p = toPx(s.p);
          if (!nearPoint(p)) break;
          // centered on its own width, and on the font's line height (so
          // a string's place doesn't depend on its letters)
          const ref = ctx.measureText("Ág");
          const baseline = p[1] + (ref.actualBoundingBoxAscent - ref.actualBoundingBoxDescent) / 2;
          ctx.fillStyle = s.s;
          ctx.fillText(s.t, p[0] - ctx.measureText(s.t).width / 2, baseline);
          break;
        }
      }
    }
  }
  ctx.restore();
}

// ---------------------------------------------------------------------
// Clipping (as Actor.Viewer.Clip)

/** The part of a segment in a box, if any (Cohen and Sutherland's method):
    an end outside it is moved onto the edge it's past, worked out from the
    other end, so the segment keeps its direction however far away its
    ends are. */
export function clipSegment(b: Box, p0: Px, q0: Px): [Px, Px] | null {
  const outside = ([x, y]: Px) => (x < b.x0 ? 1 : 0) | (x > b.x1 ? 2 : 0) | (y < b.y0 ? 4 : 0) | (y > b.y1 ? 8 : 0);
  const onEdge = ([px, py]: Px, [qx, qy]: Px): Px => {
    const alongX = (c: number) => qy + (c - qx) * (py - qy) / (px - qx);
    const alongY = (c: number) => qx + (c - qy) * (px - qx) / (py - qy);
    if (px < b.x0) return [b.x0, alongX(b.x0)];
    if (px > b.x1) return [b.x1, alongX(b.x1)];
    if (py < b.y0) return [alongY(b.y0), b.y0];
    return [alongY(b.y1), b.y1];
  };
  let p = p0, q = q0;
  for (let n = 8; ; n--) {
    const op = outside(p), oq = outside(q);
    if (op === 0 && oq === 0) return [p, q];
    if ((op & oq) !== 0 || n === 0) return null;
    if (op !== 0) p = onEdge(p, q);
    else q = onEdge(q, p);
  }
}

/** The parts of a polyline in a box, each a polyline of its own. */
export function clipPolyline(b: Box, pts: Px[]): Px[][] {
  const runs: Px[][] = [];
  let run: Px[] = [];
  for (let i = 0; i + 1 < pts.length; i++) {
    const p = pts[i], q = pts[i + 1];
    const c = clipSegment(b, p, q);
    if (c === null) { if (run.length) runs.push(run); run = []; continue; }
    const [p2, q2] = c;
    const end = run[run.length - 1];
    if (end && p2[0] === p[0] && p2[1] === p[1] && end[0] === p[0] && end[1] === p[1]) run.push(q2);
    else { if (run.length) runs.push(run); run = [p2, q2]; }
  }
  if (run.length) runs.push(run);
  return runs.filter((r) => r.length >= 2);
}

/** The part of a polygon in a box (Sutherland and Hodgman's method). */
export function clipPolygon(b: Box, pts: Px[]): Px[] {
  const against = (poly: Px[], inside: (p: Px) => boolean, cross: (a: Px, c: Px) => Px) => {
    const out: Px[] = [];
    for (let i = 0; i < poly.length; i++) {
      const a = poly[i], c = poly[(i + 1) % poly.length];
      if (inside(c)) { if (!inside(a)) out.push(cross(a, c)); out.push(c); }
      else if (inside(a)) out.push(cross(a, c));
    }
    return out;
  };
  const atX = (x: number) => ([ax, ay]: Px, [cx, cy]: Px): Px => [x, ay + (x - ax) * (cy - ay) / (cx - ax)];
  const atY = (y: number) => ([ax, ay]: Px, [cx, cy]: Px): Px => [ax + (y - ay) * (cx - ax) / (cy - ay), y];
  let poly = pts;
  poly = against(poly, ([, y]) => y <= b.y1, atY(b.y1));
  poly = against(poly, ([, y]) => y >= b.y0, atY(b.y0));
  poly = against(poly, ([x]) => x <= b.x1, atX(b.x1));
  poly = against(poly, ([x]) => x >= b.x0, atX(b.x0));
  return poly;
}

/** An ellipse far from the box, or too big for it: null if it can't reach
    the box; [] if the box is all inside it; otherwise points along the
    part of it facing the box, within a tenth of a pixel of the curve. */
export function ellipseNear(b: Box, [cx, cy]: Px, rx: number, ry: number): Px[] | null {
  if (b.x1 < cx - rx || b.x0 > cx + rx || b.y1 < cy - ry || b.y0 > cy + ry) return null;
  const corners = boxCorners(b);
  const unit = ([x, y]: Px): Px => [(x - cx) / rx, (y - cy) / ry];
  if (corners.every((c) => { const [u, v] = unit(c); return u * u + v * v <= 1; })) return [];
  const arcFrom = (a: number, z: number): Px[] => {
    const n = Math.min(100000, Math.ceil((z - a) / Math.sqrt(0.8 / Math.max(rx, ry))) + 1);
    const pts: Px[] = [];
    for (let k = 0; k <= n; k++) {
      const t = a + (z - a) * k / n;
      pts.push([cx + rx * Math.cos(t), cy + ry * Math.sin(t)]);
    }
    return pts;
  };
  if (inBox(b, [cx, cy])) return arcFrom(0, 2 * Math.PI);
  // the box doesn't hold the center, so from it the box spans a sector
  // less than a half-turn wide: the one its corners span
  const [mu, mv] = unit([(b.x0 + b.x1) / 2, (b.y0 + b.y1) / 2]);
  const mid = Math.atan2(mv, mu);
  const turn = (a: number) => a - 2 * Math.PI * Math.round(a / (2 * Math.PI));
  const offsets = corners.map((c) => { const [u, v] = unit(c); return turn(Math.atan2(v, u) - mid); });
  return arcFrom(mid + Math.min(...offsets), mid + Math.max(...offsets));
}

function boxCorners(b: Box): Px[] {
  return [[b.x0, b.y0], [b.x1, b.y0], [b.x1, b.y1], [b.x0, b.y1]];
}
