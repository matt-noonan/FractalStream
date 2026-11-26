#!/usr/bin/env python3
"""Write a table of real or complex numbers in FractalStream's `fixed`
encoding: a load-only, lossy format, much smaller after compression than
`raw` (see `decodeFixed` in fractalstream-core/src/Actor/SavedTables.hs).

    fixed_encode.py TABLE.yaml --bits 16 [--out NEW]

reads a raw table's description, TABLE.yaml, and its file beside it, and
writes NEW.yaml and NEW.bin (by default, TABLE-fixed.yaml and .bin) in the
same folder. Each of a cell's components (its reals: a complex number is
two) is stored in `bits` bits, scaled to its range in its layer (a value
of the table's last index), so the largest error in a value is half of
(hi - lo) / (2^bits - 1) for its layer's range; the script prints those.
"""
import argparse, array, json, os, sys


def read_description(path):
    info = {}
    for line in open(path, encoding='utf-8'):
        line = line.rstrip('\n')
        if not line.strip() or ':' not in line:
            continue
        key, value = line.split(':', 1)
        value = value.strip()
        try:
            info[key.strip()] = json.loads(value)
        except json.JSONDecodeError:
            info[key.strip()] = value
    return info


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('table', help="the raw table's .yaml")
    ap.add_argument('--bits', type=int, default=16, help='bits for each value (1 to 32; default 16)')
    ap.add_argument('--out', help='the new files\' path, without .yaml or .bin')
    args = ap.parse_args()

    info = read_description(args.table)
    if info.get('encoding', 'raw') != 'raw':
        sys.exit(f"{args.table}: its encoding is {info.get('encoding')}, not raw")
    ty = info['type']
    if any(w in ty for w in ('ℤ', 'Z', 'Boolean', 'Color', 'Text', 'List')):
        sys.exit(f"{args.table}: a {ty} isn't made of real and complex numbers only")
    if not 1 <= args.bits <= 32:
        sys.exit("--bits should be from 1 to 32")

    folder = os.path.dirname(args.table)
    data = array.array('d')
    with open(os.path.join(folder, info['file']), 'rb') as f:
        data.frombytes(f.read())
    if sys.byteorder != 'little':
        data.byteswap()
    dims = info['size']
    cells = 1
    for d in dims:
        cells *= d
    if cells == 0 or len(data) % cells:
        sys.exit(f"{args.table}: its file doesn't hold {cells} cells of reals")
    components = len(data) // cells
    layers = dims[-1] if len(dims) >= 2 else 1
    per_layer = cells // layers
    width = (args.bits + 7) // 8
    top = 2 ** args.bits - 1
    modulus = 2 ** (8 * width)

    ranges, worst = [], 0.0
    values = [0] * (cells * components)        # the deltas, file order
    for g in range(layers):
        for c in range(components):
            column = [data[(g * per_layer + cell) * components + c] for cell in range(per_layer)]
            lo, hi = min(column), max(column)
            if hi == lo:
                hi = lo + 1.0
            ranges.append([lo, hi])
            worst = max(worst, (hi - lo) / top / 2)
            base = (g * components + c) * per_layer
            prev = 0
            for cell, x in enumerate(column):
                q = round((x - lo) / (hi - lo) * top)
                values[base + cell] = (q - prev) % modulus
                prev = q

    # byte-shuffled: every value's first byte, then every second byte, ...
    out = bytearray(len(values) * width)
    n = len(values)
    for k in range(width):
        shift = 8 * k
        out[k * n:(k + 1) * n] = bytes((v >> shift) & 0xff for v in values)

    stem = args.out or os.path.splitext(args.table)[0] + '-fixed'
    with open(stem + '.bin', 'wb') as f:
        f.write(out)
    fields = [('type', json.dumps(ty, ensure_ascii=False)), ('size', json.dumps(dims)),
              ('origin', json.dumps(info['origin'])), ('step', json.dumps(info['step'])),
              ('encoding', 'fixed'), ('file', os.path.basename(stem) + '.bin'),
              ('bits', str(args.bits)), ('components', str(components)),
              ('ranges', json.dumps(ranges))]
    with open(stem + '.yaml', 'w', encoding='utf-8') as f:
        for k, v in fields:
            f.write(f"{k}: {v}\n")
    print(f"wrote {stem}.yaml and .bin: {len(out)} bytes ({len(data) * 8} raw), "
          f"{layers} layer(s) x {components} component(s), largest error {worst:.3g}")


if __name__ == '__main__':
    main()
