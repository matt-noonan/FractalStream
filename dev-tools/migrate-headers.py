#!/usr/bin/env python3
"""Rewrite FractalStream scripts' stanza headers from `key=value` attributes
to header phrases and a `setup:` block (see LANGUAGE.md, "Viewers").

    migrate-headers.py FILE ...       rewrite the files in place
    migrate-headers.py --check FILE   say which files would change

    @viewer "P" var=C center=-0.5 pixelSize=1/200
  becomes
    @viewer "P" over C
    setup:
      center: -0.5
      pixel size: 1/200

A viewer's `coloring:` block, which came right after its header, moves into
its `setup:`. Works on project files, scripts inside Markdown and source
files (line by line), and on one-line scripts in escaped string literals
(`"@viewer \\"V\\" var=w\\npass\\n"`).
"""
import re
import sys

PHRASE = {"var", "xvar", "yvar", "view", "lattice", "latticeCenter", "group", "groupCenter", "for", "shortcut"}
SETTING = {
    "center": "center", "pixelSize": "pixel size", "aspect": "aspect", "tiled": "tiled",
    "colorizer": "colorizer", "show_if": "show if", "limit": "iteration limit",
    "escape": "escape radius", "vanish": "vanishing radius",
    "icon": "icon", "drawLayer": "draw layer", "refreshOnActivate": "refresh on activate",
    "refreshCanUpdate": "refresh can update", "description": "description",
}
SURFACE = {"plane": "the plane", "sphere": "the sphere", "disk": "the disk", "halfplane": "the half-plane"}
HEADER = re.compile(r'^(?P<lead>@(?:viewer|tool|colorizer)\s+(?:"(?:[^"\\]|\\.)*"|overlay))(?P<rest>.*)$')


def split_attrs(rest):
    """`key=value` pairs (a value quoted, parenthesized, or up to a space)."""
    attrs, i = [], 0
    while i < len(rest):
        if rest[i].isspace():
            i += 1
            continue
        m = re.match(r'([A-Za-z_][A-Za-z_0-9]*)=', rest[i:])
        if not m:
            return None
        key = m.group(1)
        i += m.end()
        if i < len(rest) and rest[i] == '"':
            j = rest.index('"', i + 1)
            value, quoted = rest[i + 1:j], True
            i = j + 1
        elif i < len(rest) and rest[i] == '(':
            depth, j = 0, i
            while True:
                depth += {"(": 1, ")": -1}.get(rest[j], 0)
                j += 1
                if depth == 0:
                    break
            value, quoted = rest[i:j], False
            i = j
        else:
            j = i
            while j < len(rest) and not rest[j].isspace():
                j += 1
            value, quoted = rest[i:j], False
            i = j
        attrs.append((key, value, quoted))
    return attrs


def atom(v):
    """A phrase's value: as it is if it's one name or number, else in parentheses."""
    v = v.strip()
    if re.fullmatch(r'-?[\w.]+', v) or (v.startswith("(") and v.endswith(")")):
        return v
    return "(" + v + ")"


def convert(lead, rest):
    """The new header line and its `setup:` lines (without indentation)."""
    attrs = split_attrs(rest)
    if attrs is None:
        raise ValueError("can't read the attributes of: " + lead + rest)
    a = {k: v for k, v, _ in attrs}
    phrases, setup = [], []
    if "for" in a:
        phrases.append('for "%s"' % a["for"])
    if "shortcut" in a:
        phrases.append("with shortcut " + a["shortcut"])
    if "var" in a:
        phrases.append("over " + a["var"])
    if "xvar" in a or "yvar" in a:
        phrases.append("over (%s, %s)" % (a.get("xvar", "x"), a.get("yvar", "y")))
    if "view" in a and a["view"] not in SURFACE:
        raise ValueError("unknown view=%s in: %s%s" % (a["view"], lead, rest))
    if "view" in a and a["view"] != "plane":
        phrases.append("on " + SURFACE[a["view"]])
    if "lattice" in a:
        phrases.append("modulo the lattice " + atom(a["lattice"]))
        if "latticeCenter" in a:
            phrases.append("centered at " + atom(a["latticeCenter"]))
    if "group" in a:
        phrases.append("modulo the modular group" if a["group"] == "modular" else "modulo the group " + a["group"])
        if "groupCenter" in a:
            phrases.append("centered at " + atom(a["groupCenter"]))
    # (a center without its lattice or group: kept, so that it's an error)
    for k in ("latticeCenter", "groupCenter"):
        if k in a and not ("lattice" in a or "group" in a):
            phrases.append("centered at " + atom(a[k]))
    for k, v, quoted in attrs:
        if k in PHRASE:
            continue
        if k == "pixelVar":
            raise ValueError("pixelVar= has no replacement: " + lead + rest)
        if k not in SETTING:
            raise ValueError("unknown attribute %s= in: %s%s" % (k, lead, rest))
        if k in ("icon", "description") and quoted:
            v = '"' + v + '"'
        setup.append("%s: %s" % (SETTING[k], v))
    return " ".join([lead] + phrases), setup


def migrate_lines(text):
    """Script lines: each header, its setup, and a coloring block moved in."""
    lines = text.split("\n")
    out, i = [], 0
    while i < len(lines):
        line = lines[i]
        indent = line[:len(line) - len(line.lstrip())]
        m = HEADER.match(line.strip())
        if not m or ("=" not in m.group("rest") and not coloring_after(lines, i + 1, indent)):
            out.append(line)
            i += 1
            continue
        try:
            header, setup = convert(m.group("lead"), m.group("rest"))
        except ValueError as err:
            print("left as it is:", err, file=sys.stderr)
            out.append(line)
            i += 1
            continue
        out.append(indent + header)
        i += 1
        # (a viewer's coloring: block, right after its header)
        coloring = []
        if coloring_after(lines, i, indent):
            coloring.append(lines[i])
            i += 1
            while i < len(lines) and (lines[i].strip() == "" or indent_of(lines[i]) > len(indent)):
                if lines[i].strip() == "" and not continues(lines, i, indent):
                    break
                coloring.append(lines[i])
                i += 1
        if setup or coloring:
            out.append(indent + "setup:")
            out.extend(indent + "  " + s for s in setup)
            out.extend(("  " + c) if c.strip() else c for c in coloring)
    return "\n".join(out)


def indent_of(line):
    return len(line) - len(line.lstrip())


def coloring_after(lines, i, indent):
    return i < len(lines) and lines[i].strip() == "coloring:" and lines[i][:len(indent)] == indent \
        and indent_of(lines[i]) == len(indent)


def continues(lines, i, indent):
    """Whether a blank line is inside an indented block (more of it follows)."""
    j = i
    while j < len(lines) and lines[j].strip() == "":
        j += 1
    return j < len(lines) and indent_of(lines[j]) > len(indent)


ESCAPED = re.compile(r'@(viewer|tool|colorizer) (\\"(?:[^"\\]|\\\\.)*?\\"|overlay)((?:[^"\\\n]|\\")*?)(\\n|")')


def migrate_escaped(text):
    """One-line scripts in escaped string literals."""
    def one(m):
        rest = m.group(3).replace('\\"', '"')
        if "=" not in rest:
            return m.group(0)
        lead = "@" + m.group(1) + " " + m.group(2).replace('\\"', '"')
        try:
            header, setup = convert(lead, rest)
        except ValueError as err:
            print("left as it is:", err, file=sys.stderr)
            return m.group(0)
        lines = [header] + (["setup:"] + ["  " + s for s in setup] if setup else [])
        new = "\n".join(lines).replace('"', '\\"').replace("\n", "\\n")
        return new + m.group(4)
    return ESCAPED.sub(one, text)


def migrate(text):
    return migrate_escaped(migrate_lines(text))


if __name__ == "__main__":
    check = sys.argv[1:2] == ["--check"]
    for path in sys.argv[2 if check else 1:]:
        try:
            with open(path, encoding="utf-8") as f:
                old = f.read()
        except UnicodeDecodeError:
            continue
        new = migrate(old)
        if new != old:
            if check:
                print("would change", path)
            else:
                with open(path, "w", encoding="utf-8") as f:
                    f.write(new)
                print("changed", path)
