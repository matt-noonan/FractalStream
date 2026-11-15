# FractalStream project files

A project is one file, in one of two forms:

- **`.fs`**: a YAML document (UTF-8) whose top-level `script:` key holds
  the project's script, with its other keys alongside: `metadata:`
  (title, description, preview), `scenarios:`, `colorizers:`,
  `autocolor:`, `values:` (configuration values that differ from the
  script's defaults), and the apps' own keys (`wx-properties:`,
  `web-properties:`: windows and views).
- **`.fsz`**: a zip archive of a `.fs` file, named `project.fs`, and the
  data files its `manifest:` key lists. A project is saved as a `.fsz`
  when it's saved with data that doesn't belong in YAML: today, what its
  tables hold. Everything a project needs is in the one file, so that it
  can be opened, as it was, years from now.

The apps open either form (they tell them apart by their first bytes, not
their names), and save a project with tables as a `.fsz`, without them as
a `.fs`. `fsrun` opens either, and ignores saved tables unless a scenario
loads them (`load saved table T`, `load all saved tables`: see
SCENARIO_LANGUAGE.md).

## The archive

A standard zip file, as `zip` and `unzip` make and read: each entry
stored or deflated (method 0 or 8), names in UTF-8, no zip64 (no entry,
nor the archive, past 4 GB) and no encryption. FractalStream writes every
entry deflated, with the timestamp 1980-01-01 00:00 (zip's first day),
in a fixed order (`project.fs`, then the data files by name), so that
the same project always gives the same bytes.

| Entry | What it is |
|---|---|
| `project.fs` | the project's YAML, as a `.fs` file, with a `manifest:` key |
| `tables/<name>.bin` | a table's elements (see below), one file per table |

Other entries are ignored, so later versions can add files (a preview
image, say) without breaking older ones that only read what they know.

## The manifest

`project.fs`'s `manifest:` key lists the archive's data files, by kind.
Today there's one kind, `tables:`, by the table's name in the script:

```yaml
manifest:
  tables:
    grids:
      file: tables/grids.bin
      type: "Table 3 of (ℂ × ℂ)"
      size: [160, 160, 318]
      origin: [0.0, 0.0]
      step: [1.0, 1.0]
      encoding: raw
      sha256: 9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08
```

| Field | Meaning |
|---|---|
| `file` | the entry in the archive holding the table's elements |
| `type` | the table's type, as the script declares it (written as FractalStream prints types: `ℝ`, `ℂ`, `ℤ`, `Boolean`, `Color`, and tuples as `(ℂ × ℤ)`) |
| `size` | its size along each dimension |
| `origin` | the point at the center of cell (1, 1) |
| `step` | a cell's width and height (rows go down the plane) |
| `encoding` | how the file holds the elements: `raw` (below); a version of FractalStream refuses one it doesn't know |
| `sha256` | the SHA-256 of the file's bytes (as they are in the archive, inflated), in hex; FractalStream checks it, and refuses a table whose data doesn't match |

A table is restored only into a table the script still declares with the
same name and type; the others are skipped, with a warning.

Fields may be added later (provenance, for instance: where the data came
from, how it was made, by which version of which program), and readers
should ignore ones they don't know.

## Raw table data

`encoding: raw` is the table's cells, one after another, the first index
varying fastest (cell `(j, k, m)` of a table of size `[w, h, d]` is cell
number `(j - 1) + w ((k - 1) + h (m - 1))`). Each cell is its element,
packed, with no padding, little-endian:

| Type | Bytes |
|---|---|
| `Boolean` | 1: 0 or 1 |
| `ℤ` (integer) | 4: a two's-complement 32-bit integer |
| `ℝ` (real) | 8: an IEEE 754 double |
| `ℂ` (complex) | 16: the real part, then the imaginary part, as doubles |
| `Color` | 3: red, green, blue, a byte each |
| a tuple `(A × B)` | A's bytes, then B's |

So a `Table 3 of (ℂ × ℂ)` of size `[160, 160, 318]` is 160 × 160 × 318
cells of 32 bytes: 260,505,600 bytes. (In memory, FractalStream pads a
tuple's parts to 8 bytes; on disk they're packed.)

## Changes

- 2026-11-14: tables moved out of the YAML (where a `tables:` key held
  each one's elements, zlib-compressed, in base64) into `.fsz` archives.
  A `.fs` file with a `tables:` key opens without its tables, with a
  warning; `dev-tools/fsz/inline-tables-to-fsz.py` converts one.
