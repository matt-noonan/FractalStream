#!/usr/bin/env python3
"""Convert a project file with its tables inline (the old `tables:` key:
each table's packed elements, zlib-compressed and base64-encoded) into a
.fsz archive (PROJECT_FILES.md): project.fs with a `manifest:`, and each
table as two files, its description in tables/<name>.yaml and its packed
elements beside it in tables/<name>.bin.

    dev-tools/fsz/inline-tables-to-fsz.py OLD.fs [NEW.fsz]

(default: OLD.fsz beside it). Only the standard library: the `tables:`
key is read as FractalStream wrote it, the last top-level key, each table
with `type`, `size`, `origin`, `step` and a literal-block `data`. The
archive is written as FractalStream writes one: entries deflated, every
timestamp 1980-01-01, so the same input always gives the same bytes.
"""
import base64
import hashlib
import json
import re
import sys
import zipfile
import zlib


def read_tables(lines):
    """The tables in the lines of a `tables:` key (after its first line)."""
    tables = {}
    name, field, entry = None, None, None
    for line in lines:
        m = re.match(r"^  (\S[^:]*):\s*$", line)
        if m:
            name = m.group(1)
            entry = tables[name] = {"size": [], "origin": [], "step": [], "data": []}
            field = None
            continue
        m = re.match(r"^    (\w+):\s*(.*)$", line)
        if m:
            field, value = m.group(1), m.group(2).strip()
            if field == "type":
                entry["type"] = value
            elif value and value not in ("|", "|-"):
                entry[field] = json.loads(value)   # (an inline list: [1, 2])
            continue
        m = re.match(r"^    - (.*)$", line)
        if m and field in ("size", "origin", "step"):
            entry[field].append(json.loads(m.group(1)))
            continue
        if field == "data" and line.strip():
            entry["data"].append(line.strip())
    return tables


def yaml_string(s):
    return json.dumps(s, ensure_ascii=False)


def main():
    old = sys.argv[1]
    new = sys.argv[2] if len(sys.argv) > 2 else re.sub(r"\.fs$", "", old) + ".fsz"
    text = open(old, encoding="utf-8").read()
    lines = text.split("\n")
    starts = [i for i, l in enumerate(lines) if l == "tables:"]
    if not starts:
        sys.exit(f"{old} has no tables: key")
    start = starts[0]
    rest = [l for l in lines[start + 1:] if l and not l.startswith(" ")]
    if rest:
        sys.exit(f"{old}: `tables:` isn't its last top-level key")
    tables = read_tables(lines[start + 1:])
    files = []
    manifest = ["manifest:", "  tables:"]
    for name, t in tables.items():
        packed = zlib.decompress(base64.b64decode("".join(t["data"])))
        # (each table's description, and its data beside it)
        description = "\n".join([
            f"type: {yaml_string(t['type'])}",
            f"size: {json.dumps(t['size'])}",
            f"origin: {json.dumps(t['origin'])}",
            f"step: {json.dumps(t['step'])}",
            "encoding: raw",
            f"file: {name}.bin",
            f"sha256: {hashlib.sha256(packed).hexdigest()}",
        ]) + "\n"
        files.append((f"tables/{name}.yaml", description.encode("utf-8")))
        files.append((f"tables/{name}.bin", packed))
        manifest.append(f"    {name}: tables/{name}.yaml")
    project = "\n".join(lines[:start] + manifest) + "\n"
    with zipfile.ZipFile(new, "w", zipfile.ZIP_DEFLATED) as z:
        for path, data in [("project.fs", project.encode("utf-8"))] + files:
            info = zipfile.ZipInfo(path, date_time=(1980, 1, 1, 0, 0, 0))
            info.compress_type = zipfile.ZIP_DEFLATED
            z.writestr(info, data)
    data = [d for path, d in files if path.endswith(".bin")]
    print(f"{new}: {len(data)} table(s), {sum(len(d) for d in data)} bytes of data")


main()
