#!/usr/bin/env python3
"""How alike two PNGs (as fsrun writes them: 8-bit RGB or RGBA, not
interlaced) are: the share of pixels that are the same, and that are within
2 levels in every channel. With only the standard library, for CI.

    dev-tools/ci/compare-png.py A.png B.png [--min-close PERCENT]

Prints the shares; with --min-close, exits 1 if fewer are within 2.
"""
import struct
import sys
import zlib


def read_png(path):
    with open(path, "rb") as f:
        data = f.read()
    if data[:8] != b"\x89PNG\r\n\x1a\n":
        raise ValueError(path + " isn't a PNG")
    pos, idat, header = 8, b"", None
    while pos < len(data):
        n, kind = struct.unpack(">I4s", data[pos:pos + 8])
        chunk = data[pos + 8:pos + 8 + n]
        if kind == b"IHDR":
            header = struct.unpack(">IIBBBBB", chunk)
        elif kind == b"IDAT":
            idat += chunk
        pos += 12 + n
    width, height, depth, color, _, _, interlace = header
    if depth != 8 or color not in (2, 6) or interlace:
        raise ValueError(path + ": only 8-bit RGB or RGBA, not interlaced")
    bpp = 3 if color == 2 else 4
    raw = zlib.decompress(idat)
    stride = width * bpp
    rows, prev = [], bytearray(stride)
    for y in range(height):
        kind = raw[y * (stride + 1)]
        line = bytearray(raw[y * (stride + 1) + 1:(y + 1) * (stride + 1)])
        for i in range(stride):
            a = line[i - bpp] if i >= bpp else 0
            b = prev[i]
            c = prev[i - bpp] if i >= bpp else 0
            if kind == 1:
                line[i] = (line[i] + a) & 255
            elif kind == 2:
                line[i] = (line[i] + b) & 255
            elif kind == 3:
                line[i] = (line[i] + (a + b) // 2) & 255
            elif kind == 4:
                p = a + b - c
                pa, pb, pc = abs(p - a), abs(p - b), abs(p - c)
                pred = a if pa <= pb and pa <= pc else (b if pb <= pc else c)
                line[i] = (line[i] + pred) & 255
        rows.append(line)
        prev = line
    pixels = [tuple(row[x * bpp:x * bpp + 3]) for row in rows for x in range(width)]
    return width, height, pixels


def main():
    args = sys.argv[1:]
    minimum = None
    if "--min-close" in args:
        k = args.index("--min-close")
        minimum = float(args[k + 1])
        del args[k:k + 2]
    wa, ha, pa = read_png(args[0])
    wb, hb, pb = read_png(args[1])
    if (wa, ha) != (wb, hb):
        print(f"sizes differ: {wa}x{ha} and {wb}x{hb}")
        sys.exit(1)
    same = sum(1 for p, q in zip(pa, pb) if p == q)
    close = sum(1 for p, q in zip(pa, pb) if all(abs(u - v) <= 2 for u, v in zip(p, q)))
    n = len(pa)
    print(f"{100 * same / n:.2f}% the same, {100 * close / n:.2f}% within 2")
    if minimum is not None and 100 * close / n < minimum:
        sys.exit(1)


main()
