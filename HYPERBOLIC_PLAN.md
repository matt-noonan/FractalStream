# Hyperbolic viewers: plan

A living plan, like `WEB_PLAN.md`. Started 2026-11-13, after porting the
Per_n(0) atlas (github.com/callieshlb/per_n-viewer), whose Per_6 .. Per_9
models are drawn on the uniformizing disk of a hyperbolic surface. The port
worked around the missing pieces in script (a "Recenter" tool, folding into
the fundamental polygon by hand, data in saved tables); this plan builds
them into FractalStream, in general, so that a specialized atlas is just a
script and its data.

## Decisions

| | Decision | Why |
|---|---|---|
| H1 | **General, in layers**: a hyperbolic view first, then an optional group layer, then specializations as scripts + data. | Navigation and precision are the same for every surface or group; only the data differs. |
| H2 | **The Poincaré disk first; the upper half-plane as a fast follow**, switchable between the two at run time. | The half-plane is the same machinery with a different map to the screen. |
| H3 | **The script still sees a complex number** (`z` in the unit disk), as on the sphere. | Existing scripts and tools keep working; the view only decides which point a pixel is. |
| H4 | **The view is an isometry**, s ↦ (a s + b)/(b̄ s + ā) with \|a\|² − \|b\|² = 1, plus a zoom of the disk on screen. | Composing isometries is exact enough and cheap; the pair (a, b) saves and restores. |
| H5 | **Keep the homogeneous form** [a s + b : b̄ s + ā] until the last step. | Phase 2 folds the view itself by the group, so points near the boundary stay precise (a plain `z` within 1e-12 of the circle has few digits left). |
| H6 | **`draw line` keeps its meaning** (a straight segment in `z`, drawn curved when the view moves it); `draw geodesic` may come later, or `draw line` may be revisited. | Drawing commands mean the same in every view. |
| H7 | **A plane viewer isn't switchable to the disk** (not needed); disk ↔ half-plane is. | Outside the unit disk there'd be nothing to see. |
| H8 | **Data stays in the project file**: no remote or loose sidecar data (see "Data", below). | Reproducibility: a researcher must be able to run a script decades later. |

## Phase 1: `view=disk`

- [x] Header: `@viewer "..." view=disk center=<disk point> pixelSize=...`
      (complex viewers only). `center=` is the disk point at the middle of
      the window; `pixelSize` is in units of the disk's radius, as on the
      sphere.
- [x] Geometry module (like `Actor.Viewer.Sphere`): isometries as (a, b),
      composition, inverse, the translation taking one point to another,
      normalization (renormalize after composing, so rounding can't drift
      off the group).
- [x] The wrapper (`inTheDisk`, like `onTheSphere`): off the disk, a
      background color; on it, `z` from the screen point through the
      homogeneous pair. Live values `[internal] disk view` / the isometry,
      as the sphere's rotation is passed.
- [x] Saved view: `wx-properties` `view: disk` and the isometry; the
      original view for "Original view".
- [x] wx navigation: drag = the hyperbolic translation taking the grabbed
      point to the cursor (live, one history step); scroll = magnify the
      disk on screen about the cursor; double-click = recenter on the
      clicked point; animation and history as for the other views.
- [x] Tools and overlays: points in disk coordinates; drawings mapped
      through the view by sampling (as the sphere's `visibleRuns`), in both
      wx and the headless overlay.
- [x] `fsrun`: set the disk view (center / isometry), and a navigation drag.
- [x] Tests: geometry (`Actor.DiskSpec`: isometries, inverses, drag,
      interpolation, headers); end to end (`DiskSpec` in the headless
      suite: center, drag, pixel size, background, drawings). Checked by
      hand: the Per_6 atlas on `view=disk` renders as its Recenter-tool
      version (byte-identical facing 0; elsewhere only last-bit rounding
      in chaotic regions), and the wx app restores a saved isometry.
- [x] `pixel_size` is the pixel's local model width in the disk (it varies
      with the pixel), so a pixel's hyperbolic size is
      `2 pixel_size / (1 - |z|²)` (needed for tile edges and similar).
- [ ] wx navigation checked by hand with the mouse (drag, click, scroll).
- [x] The web page: hyperbolic drag (`diskDrag`; the engine refuses the
      page's covered region once the view has moved), checked in headless
      Chromium (`check-tools.mjs disk`).
- [x] Tools' marker sizes: `pixel_size_at(p)`, a built-in function for
      tools (the model width of a screen pixel at the model point p, from
      hidden view values each tool gets), so markers look the same anywhere.
- [x] Docs: the language reference, the skill's reference, CLAUDE_NOTES.

## Phase 1b: the upper half-plane

Decided: a script's `view=` fixes its *model* (the disk or the half-plane:
where its `z` is); the *display* (how it's drawn) is switchable, keeping the
view. The kernel gets one Möbius map, screen → model.

- [x] `view=halfplane`; the same isometries, conjugated by the Cayley map.
- [x] Switching disk ↔ half-plane at run time (viewer menu), keeping the
      view.
- [x] The half-plane drawn with its real axis along the window's bottom
      edge (no dead space below it): `center=` 1/`pixelSize` pixels above
      it; zooms about the axis point below the pointer (an isometry, so the
      axis stays put); resizes keep it there.
- [ ] Maybe a preference for where the real axis goes (the bottom edge, as
      now, or with the view's center in the middle of the window).

## Phase 2: the group layer

Decided (2026-11-14): `group=` names a `Table 2 of C` of side pairings
(rows 1-4: a, b, c, d of each side's map, in the script's model, taking
the outside of the side back in; further rows the script's own), or
`modular`. The polygon is the one the pairings' bisectors bound (the
Dirichlet polygon about `groupCenter=`, by default the model's middle). The
script sees the folded `z` plus `side_distance`, `side_index`,
`fold_count`; tools get folded points, and their drawings are repeated in
each visible tile. Implemented in `Actor.Viewer.Group`.

- [x] `group=` names side-pairing data, in a table (or `modular`).
- [x] The renderer folds each pixel (generated code around the script, with
      the group's sides as hidden values, prepared once per version of the
      table) *and the view* (reduced after each move, in fsrun, wx and the
      web app; the old view by the same element, so animations don't
      change); the script gets the folded point, how many steps it took,
      and its distance to the polygon's sides.
- [x] Faster folding: in the Klein model the polygon is convex about its
      center, so a binary search of the sides' angles finds the side a
      point is beyond (Per_9: 2250 sides, about 11 comparisons a step).
      Per_9 renders in 25 s of CPU instead of 42.
- [x] Tile edges, from `side_distance` and `side_index` (the script draws
      them, in its own colors).
- [x] Overlays repeated across tiles: a breadth-first walk across sides,
      keeping tiles that cover at least 3% of the polygon's own area on the
      screen and are shown at most twice as large anywhere (corners and
      center), up to 2000.
- [x] Tests: `Actor.GroupSpec` (folding, reduction, tiles, headers);
      `GroupSpec` end to end (folding, tools, copies, a view moved 77 units
      along an axis by the group staying exact); `check-tools.mjs group`.
- [x] Examples: examples/j-invariant.fs (the modular group), and
      examples/hyperbolic-surface.fs (a genus-g surface, its table worked
      out by a tool).
- [ ] Maybe `draw geodesic from p to q` (see H6).
- [ ] Copies of a drawing keep its model size: a marker a tool sized with
      `pixel_size_at` for its own place is smaller or larger elsewhere. A
      way to say "this is a marker, keep its screen size" would help.
- [ ] In fsrun, an overlay's table writes take effect after the capture
      that ran it (the surface example's first capture is grey).
- [ ] A bad group table folds nothing, silently: say why somewhere.

Navigation note (2026-11-13): a pinch or scroll can't zoom about the
pointer by hyperbolic motions, since those that fix a point are rotations
about it; on the half-plane both zoom about the point of the real axis
below the pointer (an isometry), so the axis stays at the bottom.

## Phase 3: specializations

- [x] The Per_n(0) atlases (examples/pern-atlas, not in the repo: kept
      out of git for size) rewritten on phases 1–2 (`gen_group.py`:
      `group=pairings`), and checked against the earlier, verified renders:
      Per_6 identical but for rounding noise near the circle (a different
      but equivalent order of fold steps).

## Data (future work, not started)

Self-contained, documented, compact:

- [ ] An optional `.fsz` bundle: a zip of the YAML project and raw data
      files (no base64), still one file to archive.
- [ ] A 32-bit real table type (`R32`): float32 data stays float32.
- [ ] Compact encodings for grids (byte shuffling, differences along rows,
      as per_n-viewer's `.dxs`).
- [ ] Provenance with each data set: source, method (with citation),
      generator version, checksum.
- [ ] A careful written spec of every binary format (or standard formats),
      so that the data can be read without FractalStream, and so that other
      tools can write data for FractalStream scripts.

## Known limits

- Kleinian groups act on the sphere, not the hyperbolic plane: their limit
  sets aren't this view's business.
