#!/usr/bin/env python3
"""Check a FractalStream project headlessly, the way a person would try it.

    python3 check.py project.fs [output-dir]

Renders every viewer, then for each tool selects it on its viewer, clicks,
double-clicks, drags and refreshes at the viewer's center (so runtime errors
in its handlers show up), and captures the viewer with the tool's drawings.
Prints fsrun's output (errors, and warnings about magenta pixels) and the
PNG paths to inspect.

fsrun is found from $FSRUN, else via `stack exec which fsrun` in the repo's
fractalstream-ui-headless directory (build it there with `stack build`).
"""
import os
import re
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(os.path.join(HERE, "..", "..", ".."))


def find_fsrun():
    if os.environ.get("FSRUN"):
        return os.environ["FSRUN"]
    headless = os.path.join(REPO, "fractalstream-ui-headless")
    out = subprocess.run(["stack", "exec", "which", "fsrun"], cwd=headless,
                         capture_output=True, text=True)
    path = out.stdout.strip()
    if out.returncode != 0 or not path:
        sys.exit("couldn't find fsrun; build it with `stack build` in " + headless
                 + " or set $FSRUN")
    return path


def header_attr(line, name):
    m = re.search(name + r'=(?:"([^"]*)"|(\S+))', line)
    return (m.group(1) or m.group(2)) if m else None


def main():
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    project = os.path.abspath(sys.argv[1])
    outdir = os.path.abspath(sys.argv[2]) if len(sys.argv) > 2 else tempfile.mkdtemp(prefix="fscheck-")
    os.makedirs(outdir, exist_ok=True)
    src = open(project).read()

    viewers = {}  # title -> center expression
    for line in src.splitlines():
        m = re.match(r'\s*@viewer\s+"([^"]+)"(.*)', line)
        if m:
            viewers[m.group(1)] = header_attr(m.group(2), "center") or "0"
    tools = []  # (name, viewer)
    for line in src.splitlines():
        m = re.match(r'\s*@tool\s+"([^"]+)"(.*)', line)
        if m:
            target = header_attr(m.group(2), "for")
            if target is None and len(viewers) == 1:
                target = next(iter(viewers))
            tools.append((m.group(1), target))

    def q(s):
        return s.replace('"', '\\"')

    cmds = []
    pngs = []
    for i, v in enumerate(viewers):
        png = os.path.join(outdir, "viewer-%d.png" % i)
        cmds.append('capture viewer "%s" base to "%s"' % (q(v), png))
        pngs.append((png, 'viewer "%s"' % v))
    for j, (t, v) in enumerate(tools):
        if v not in viewers:
            print('tool "%s": target viewer %r not found' % (t, v))
            continue
        c = viewers[v]
        cmds += ['select viewer "%s"' % q(v),
                 'select tool "%s"' % q(t),
                 'trigger refresh',
                 'click at (%s) + 0.1 + 0.05i' % c,
                 'double-click at (%s) - 0.1' % c,
                 'drag from (%s) to (%s) + 0.2 + 0.1i' % (c, c),
                 'trigger refresh']
        png = os.path.join(outdir, "tool-%d.png" % j)
        cmds.append('capture viewer "%s" to "%s"' % (q(v), png))
        pngs.append((png, 'tool "%s" on viewer "%s"' % (t, v)))
        cmds.append('deselect tool')

    cmdfile = os.path.join(outdir, "check.cmds")
    with open(cmdfile, "w") as f:
        f.write("\n".join(cmds) + "\n")
    p = subprocess.run([find_fsrun(), project, "--commands", cmdfile],
                       capture_output=True, text=True)
    print((p.stdout + p.stderr).strip())
    print()
    print("viewers: %d, tools: %d" % (len(viewers), len(tools)))
    for png, what in pngs:
        if os.path.exists(png):
            print("  %s: %s" % (what, png))
    sys.exit(p.returncode)


if __name__ == "__main__":
    main()
