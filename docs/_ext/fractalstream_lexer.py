"""
A Pygments lexer for FractalStream scripts, and a Sphinx extension that
registers it.

Use it in the docs with::

    .. code-block:: fractalstream

       @viewer "Julia set"
       iterate z -> z^2 + C until z escapes

The lexer follows the app's own editor highlighting
(``fractalstream-core/src/Language/Editor/Highlight.hs``), so a snippet
looks in the docs the way it does in the app. Like the app, it works line
by line from tokens rather than from a parse, and decides what a name is
by what the snippet defines: statement words (``draw``, ``while``, ``at``,
...) aren't reserved by the language, so they're keywords only where the
snippet doesn't define them as something else, and names it does define
are marked as parameters (``@config`` and tool fields), variables,
functions, or code blocks. The word lists below are copied from the
Haskell sources named next to each; keep them in step.

How the app's classes become Pygments tokens:

==================  ============================================
App class           Pygments token
==================  ============================================
keyword             Keyword
builtin-function    Name.Builtin (also ``∂`` and ``ⅆ``)
builtin-value       Keyword.Constant (``pi``, ``i``, ``e``,
                    ``true``, ``false``, ``∞``, ``infinity``) or
                    Name.Builtin.Pseudo (``iterations``, ``stuck``,
                    ``color``)
type                Keyword.Type
color               Name.Constant
number              Number.Integer / Number.Float
string              String.Double (escapes: String.Escape)
comment             Comment.Single
operator            Operator
stanza-header       Name.Decorator (``@viewer``), String (title),
                    Name.Attribute (``var``, ``center``, ...)
parameter           Name.Variable.Global
variable            Name.Variable
function            Name.Function
code-block          Name.Function
undefined           Name
==================  ============================================

Brackets, commas, and colons are Punctuation, and a ``...`` or ``…`` line
continuation is too. A character the language has no use for is an Error
token, which Sphinx reports as a warning when it builds the docs.
"""

import re
import unicodedata

from pygments.lexer import Lexer
from pygments.token import (
    Comment,
    Error,
    Keyword,
    Name,
    Number,
    Operator,
    Punctuation,
    String,
    Text,
    Whitespace,
)

__all__ = ["FractalStreamLexer"]


# ----------------------------------------------------------------------
# Words (keep in step with the Haskell sources named)

# Language.Parser.Tokenizer.wordlikeTokens: words that can't name
# anything, by the class Language.Editor.Highlight.tokenClass gives them.
_RESERVED_KEYWORDS = {
    "if", "then", "else", "or", "and", "not",
    "escapes", "escaped", "vanishes", "vanished", "stops", "stopped",
    "previous", "times", "to",
}
_RESERVED_CONSTANTS = {"e", "𝑒", "pi", "π", "i", "𝑖", "true", "false"}
# (`click`, `drag start` and `drag end` are a tool handler's points)
_RESERVED_PSEUDO = {"iterations", "stuck", "click", "drag"}
_RESERVED_FUNCTIONS = {
    "text", "append", "prepend", "join", "remove", "find", "transform",
    "range", "length", "diff",
}
_RESERVED = (_RESERVED_KEYWORDS | _RESERVED_CONSTANTS | _RESERVED_PSEUDO
             | _RESERVED_FUNCTIONS)

# Language.Editor.Highlight.keywords: statement words, which are keywords
# unless the script defines them.
_KEYWORDS = {
    "while", "until", "repeat", "iterate", "up", "to", "for", "each", "in",
    "with", "first", "matching", "define", "function", "run", "pass", "draw",
    "use",
    "erase", "write", "point", "at", "circle", "filled", "radius", "pixels",
    "rectangle", "from", "line", "stroke", "fill", "on", "as",
    "choice", "button", "row", "tab", "coloring", "reset", "resize",
}

# Language.Editor.Highlight.builtinFunctions, from the tables in
# Language.Value.Builtins (realFunctions, commonFunctions,
# complexFunctions, roundingFunctions), the standard library's functions
# (Language.Stdlib.stdlibFunctionNames: fractalstream-core/stdlib/standard.fs,
# less its `internal_` names), and the extras listed there.
_BUILTIN_FUNCTIONS = {
    # realFunctions
    "arccos", "arcsin", "arctan", "acos", "asin", "atan",
    "arccosh", "arcsinh", "arctanh", "acosh", "asinh", "atanh",
    # commonFunctions
    "exp", "log", "sqrt", "cos", "sin", "tan", "cosh", "sinh", "tanh",
    # complexFunctions
    "re", "Re", "im", "Im", "arg", "Arg", "conj", "Conj", "bar", "Bar",
    # roundingFunctions
    "round", "floor", "ceiling", "ceil",
    # the standard library
    "rainbow", "arenberg", "romao", "bamo", "broco", "corko", "viko",
    "ice", "fire", "wheat", "rose", "forest", "ocean",
    "winter", "spring", "summer", "fall",
    "rgb255", "cycle_at", "wheel2_at", "wheel4_at", "wheel8_at", "wheel8",
    "domain_color", "mobius_stretch", "hyperbolic_stretch_at",
    "view_stretch_at", "pixel_size_at",
    "min", "max", "clamp", "compose", "newton_root", "full_newton_root", "S",
    "find_cycle", "riemann_sum", "residue", "winding_number",
    "theta1", "theta2", "theta3", "theta4", "lattice_basis",
    "lattice_reduce_by", "lattice_reduce", "wp", "wp_prime", "g2", "g3",
    "klein_j",
    # the rest
    "mod", "rgb", "hsv", "hsl", "blend", "cycle", "dark", "light", "invert",
    "autocolor", "nearest_autocolor", "get_autocolor",
    "table_size", "table_cell", "table_point",
    "partial", "chart", "affine", "chordal",
}

# Language.Value.Builtins.colors
_COLORS = {
    "red", "green", "blue", "black", "white", "grey", "gray", "orange",
    "yellow", "purple", "violet", "pink",
}

# Language.Value.Builtins.types
# (ℙ¹ is also `ℙ¹`, a superscript after `ℙ`: see _classify, and `P^1`,
# which is left as it is: Language.Value.Parser.typeGrammar)
_TYPES = {
    "R", "ℝ", "Real", "Z", "ℤ", "Int", "Integer", "C", "ℂ", "Complex",
    "Bool", "Boolean", "Color", "Text",
    "ℙ", "P1", "Ĉ", "ℂ\u0302", "Cinf",
}
_UNICODE_TYPES = {t for t in _TYPES if not t.isascii()}

# FractalStream's own settings, which every script sees.
_SETTINGS = ("max_iters", "escape_radius", "vanishing_radius")


# ----------------------------------------------------------------------
# Tokens of one line (Language.Parser.Tokenizer.tokenize)

# Language.Parser.Tokenizer.opTokens; brackets and the like are
# punctuation here, the rest operators. Longest first, then in string
# order, as the tokenizer tries them (so `<-` wins over `<=` in `x<-1`).
_PUNCTUATION = ["(", ")", "[", "]", "{", "}", ",", ":", ";"]
_OPERATORS = [
    "+", "-", "*", "×", "//", "/", "^", "|", ":=", "≔", "'", "′",
    ">", "<", "===", "≡", "==", "=", ">=", "<=", "≥", "≤",
    "!=", "=/=", "≠", "->", "<-", "⭢", "⭠", "→", "←", "⟼", "↦", "⟶", "⟵",
    "@", "@@", "∘",
]
# (∂ and ⅆ start a derivative written as a fraction, ∂f/∂z and ⅆf/ⅆz,
# and are classed as built-in functions; ∞ is a built-in value)
_DERIVATIVE_SIGNS = ["∂", "ⅆ"]
_INFINITY = "∞"
_SYMBOLS = sorted(_PUNCTUATION + _OPERATORS + _DERIVATIVE_SIGNS + [_INFINITY],
                  key=lambda s: (-len(s), s))

_SUPERSCRIPTS = "⁰¹²³⁴⁵⁶⁷⁸⁹"
# Combining marks (Language.Parser.Tokenizer.identChar allows any).
_MARKS = "̀-ͯ᪰-᫿᷀-᷿⃐-⃿︠-︯"

_TOKEN_RE = re.compile(
    r"""
      (?P<space>\s+)
    | (?P<string>"(?:\\.|[^"\\])*"?)
    | (?P<sci>\d+(?:\.\d+)?[eE][+-]?\d+)
    | (?P<float>\d+\.\d+)
    | (?P<int>\d+)
    | (?P<super>⁻?[{sup}]+)
    | (?P<symbol>{symbols})
    | (?P<ident>(?![{sup}ⅆ])[^\W\d_](?:(?![{sup}ⅆ])\w|[{marks}])*)
    | (?P<junk>.)
    """.format(
        sup=_SUPERSCRIPTS,
        marks=_MARKS,
        symbols="|".join(re.escape(s) for s in _SYMBOLS),
    ),
    re.VERBOSE,
)

_ESCAPE_RE = re.compile(r'\\["\\nt]')
_CONTINUATION_RE = re.compile(r"(?:\.\.\.|…)\s*$")


class _Tok:
    """A token of a line: where it is, its text, and what kind it is.

    ``kind`` is one of ``ident`` (a name, which ``name`` holds in NFC),
    ``reserved`` (a word in _RESERVED), ``int``, ``float``, ``sci``,
    ``super``, ``string``, ``symbol``, ``junk``, or ``continuation``.
    """

    __slots__ = ("start", "end", "text", "kind", "name")

    def __init__(self, start, end, text, kind):
        self.start, self.end, self.text, self.kind = start, end, text, kind
        self.name = None
        if kind == "ident":
            self.name = unicodedata.normalize("NFC", text)
            if self.name in _RESERVED:
                self.kind = "reserved"

    def is_ident(self, name=None):
        return self.kind == "ident" and (name is None or self.name == name)

    def is_symbol(self, *symbols):
        return self.kind == "symbol" and self.text in symbols


def _symbol_key(text):
    """Symbols the tokenizer treats as the same token compare equal."""
    return {
        "≔": ":=", "′": "'", "≡": "===", "×": "*",
        "≥": ">=", "≤": "<=", "!=": "≠", "=/=": "≠", "==": "=",
        "⭢": "->", "→": "->", "⟼": "->", "↦": "->", "⟶": "->",
        "⭠": "<-", "←": "<-", "⟵": "<-",
    }.get(text, text)


def _comment_start(line):
    """Where a line's comment starts: its first ``#`` not in quotes."""
    quoted = False
    k = 0
    while k < len(line):
        c = line[k]
        if c == "#" and not quoted:
            return k
        if c == "\\" and quoted:
            k += 2
            continue
        if c == '"':
            quoted = not quoted
        k += 1
    return None


class _Line:
    """A line of the script: its text, where it starts, its tokens,
    where its comment starts (if it has one), and its indentation."""

    def __init__(self, text, start):
        self.text = text
        self.start = start
        self.comment = _comment_start(text)
        code = text if self.comment is None else text[: self.comment]
        self.continuation = None
        m = _CONTINUATION_RE.search(code)
        if m:
            self.continuation = m.start()
            code = code[: m.start()]
        self.tokens = []
        for m in _TOKEN_RE.finditer(code):
            kind = m.lastgroup
            if kind != "space":
                self.tokens.append(_Tok(m.start(), m.end(), m.group(), kind))
        if self.continuation is not None:
            end = len(text) if self.comment is None else self.comment
            marker = text[self.continuation:end].rstrip()
            self.tokens.append(_Tok(self.continuation,
                                    self.continuation + len(marker),
                                    marker, "continuation"))
        self.indent = len(text) - len(text.lstrip(" "))

    def is_header(self):
        return self.text.startswith("@")

    def first_word(self):
        if self.tokens and self.tokens[0].kind == "ident":
            return self.tokens[0].name
        return None


# ----------------------------------------------------------------------
# Stanzas and what they define (Language.Editor.Highlight)

PARAMETER, VARIABLE, FUNCTION, CODE_BLOCK = (
    "parameter", "variable", "function", "code-block")


class _Stanza:
    """A stanza: its kind ("viewer", "tool", ...; "" for code before any
    header), its title, its attributes, and its lines (header included)."""

    def __init__(self, header, lines):
        self.header = header
        self.lines = lines
        self.kind = ""
        self.title = None
        self.attrs = {}
        if header is not None:
            m = re.match(r"@(\w*)", header.text)
            self.kind = m.group(1)
            toks = header.tokens
            titles = [t for t in toks if t.kind == "string"]
            if titles:
                self.title = _unquote(titles[0].text)
            k = 0
            while k < len(toks):
                if (k + 2 < len(toks) and toks[k].is_ident()
                        and toks[k + 1].is_symbol("=", "==")):
                    v = toks[k + 2]
                    value = None
                    if v.kind in ("ident", "reserved"):
                        value = v.name
                    elif v.kind == "string":
                        value = _unquote(v.text)
                    elif v.kind == "int":
                        value = v.text
                    if value is not None:
                        self.attrs.setdefault(toks[k].name, value)
                        k += 3
                        continue
                k += 1


def _unquote(s):
    body = s[1:-1] if len(s) >= 2 and s.endswith('"') else s[1:]
    return re.sub(r'\\(["\\nt])',
                  lambda m: {"n": "\n", "t": "\t"}.get(m.group(1), m.group(1)),
                  body)


def _split_stanzas(lines):
    stanzas = []
    k = 0
    while k < len(lines) and not lines[k].is_header():
        k += 1
    if k > 0:
        stanzas.append(_Stanza(None, lines[:k]))
    while k < len(lines):
        header = lines[k]
        j = k + 1
        while j < len(lines) and not lines[j].is_header():
            j += 1
        stanzas.append(_Stanza(header, lines[k:j]))
        k = j
    return stanzas


def _viewer_names(stanza):
    """The names a viewer gives its code (and its tools')."""
    attrs = stanza.attrs
    if "xvar" in attrs and "yvar" in attrs:
        return ([(attrs["xvar"], VARIABLE), (attrs["yvar"], VARIABLE),
                 ("pixel_width", PARAMETER), ("pixel_height", PARAMETER)]
                + [(n, PARAMETER) for n in
                   ("viewer_width", "viewer_height",
                    "viewer_center_x", "viewer_center_y")])
    return ([(attrs.get("var", "z"), VARIABLE)]
            + [(n, PARAMETER) for n in
               ("viewer_width", "viewer_height", "viewer_center")])


def _first_wins(pairs):
    defs = {}
    for name, what in pairs:
        defs.setdefault(name, what)
    return defs


def _union(*maps):
    """Merge maps, the earlier ones winning (as Haskell's Map.unions)."""
    out = {}
    for m in maps:
        for k, v in m.items():
            out.setdefault(k, v)
    return out


def _is_handler(line):
    return line.indent == 0 and line.first_word() == "on"


def _definitions(stanza):
    """The names a stanza defines, from the shapes of its lines."""
    body = [l for l in (stanza.lines[1:] if stanza.header else stanza.lines)
            if l.tokens]
    first_handler = next((k for k, l in enumerate(body) if _is_handler(l)),
                         None)

    def is_field(k):
        if stanza.kind in ("config", "colorizer"):
            return True
        if stanza.kind == "tool":
            return first_handler is None or k < first_handler
        return False

    def enclosing(k):
        line = body[k]
        for p in reversed(body[:k]):
            if p.indent < line.indent:
                return p
        return None

    pairs = []
    for k, line in enumerate(body):
        toks = line.tokens
        # `name : Type ...` (not `row:` or `coloring:`, which have no type)
        if (len(toks) >= 3 and toks[0].is_ident()
                and toks[1].is_symbol(":")):
            pairs.append((toks[0].name, PARAMETER if is_field(k) else VARIABLE))
        pairs.extend(_code_definitions(toks))
        # settings changed in a viewer's `coloring:` block
        parent = enclosing(k)
        if (parent is not None and parent.first_word() == "coloring"
                and toks[0].is_ident()):
            pairs.append((toks[0].name, PARAMETER))
    return _first_wins(pairs)


def _function_definition(toks):
    """Whether tokens (after a name) are ``(x, y) :=``: a function's
    parameters and the start of its body."""
    if not toks or not toks[0].is_symbol("("):
        return False
    k = 1
    while k + 1 < len(toks) and toks[k].is_ident():
        nxt = toks[k + 1]
        if nxt.is_symbol(")"):
            return k + 2 < len(toks) and _symbol_key(toks[k + 2].text) == ":="
        if not nxt.is_symbol(","):
            return False
        k += 2
    return False


def _code_definitions(toks):
    """Definitions that can appear in any code."""

    def params(ts):
        prev = None
        out = []
        for t in ts:
            if t.is_symbol(")"):
                break
            if t.is_ident() and (prev is None or prev.is_symbol("(", ",")):
                out.append((t.name, VARIABLE))
            prev = t
        return out

    def everywhere(ts):
        out = []
        for a, b in zip(ts, ts[1:]):
            if a.is_ident() and b.kind == "symbol" and _symbol_key(b.text) in (":=", "->"):
                out.append((a.name, VARIABLE))
            elif (a.is_ident("each") or a.is_ident("first")) and b.is_ident():
                out.append((b.name, VARIABLE))
        return out

    if not toks:
        return []
    t0 = toks[0]
    # `f(x, y) := ...` (before tuple declarations, which it also looks like)
    if t0.is_ident() and _function_definition(toks[1:]):
        return [(t0.name, FUNCTION)] + params(toks[2:])
    if (t0.is_ident("function") and len(toks) > 2 and toks[1].is_ident()
            and toks[2].is_symbol("(")):
        return [(toks[1].name, FUNCTION)] + params(toks[3:])
    if t0.is_ident("define") and len(toks) > 1 and toks[1].is_ident():
        if len(toks) > 2 and toks[2].is_symbol("("):
            return [(toks[1].name, CODE_BLOCK)] + params(toks[3:])
        return [(toks[1].name, CODE_BLOCK)]
    # homogeneous coordinates, `[a : b] := p`
    if t0.is_symbol("["):
        close = next((k for k, t in enumerate(toks) if t.is_symbol("]")), None)
        if (close is not None and close + 1 < len(toks)
                and toks[close + 1].kind == "symbol"
                and _symbol_key(toks[close + 1].text) == ":="
                and all(t.is_ident() or t.is_symbol(":") for t in toks[1:close])):
            return ([(t.name, VARIABLE) for t in toks[1:close] if t.is_ident()]
                    + everywhere(toks))
    # a tuple declaration, `x, y := ...` or `(x, y) := ...`
    k = next((k for k, t in enumerate(toks)
              if t.kind == "symbol" and _symbol_key(t.text) == ":="), None)
    if k:
        lhs = toks[:k]
        if (any(t.is_symbol(",") for t in lhs)
                and all(t.is_ident() or t.is_symbol(",", "(", ")") for t in lhs)):
            return [(t.name, VARIABLE) for t in lhs if t.is_ident()] + everywhere(toks)
    return everywhere(toks)


def _all_definitions(stanzas):
    """What each stanza's names refer to."""
    viewer_vars = {s.title: _viewer_names(s) for s in stanzas
                   if s.kind == "viewer" and s.title is not None}
    own = [_definitions(s) for s in stanzas]
    common = _union(*[d for s, d in zip(stanzas, own)
                      if s.kind in ("config", "common")],
                    {n: PARAMETER for n in _SETTINGS})

    def stanza_defs(s, d):
        implicit = []
        if s.kind == "viewer":
            implicit = _viewer_names(s) + [
                (s.attrs.get("pixelVar", "pixel_size"), PARAMETER)]
        elif s.kind == "tool":
            names = None
            if "for" in s.attrs:
                names = viewer_vars.get(s.attrs["for"])
            elif len(viewer_vars) == 1:
                names = next(iter(viewer_vars.values()))
            # (a tool's handlers name their points `click`, `drag start` and
            # `drag end`: they see the viewer's pixel and geometry values,
            # not its coordinate)
            implicit = ([nw for nw in (names or []) if nw[1] == PARAMETER]
                        + [("pixel_size", PARAMETER)])
        elif s.kind == "colorizer":
            implicit = [("pixel_size", PARAMETER)]
        elif s.kind == "":
            implicit = [("z", VARIABLE), ("pixel_size", PARAMETER)]
        return _union(d, _first_wins(implicit))

    per_stanza = [stanza_defs(s, d) for s, d in zip(stanzas, own)]
    every = _union(*per_stanza)
    return [_union(d, common, every if s.kind in ("common", "colorizer") else {})
            for s, d in zip(stanzas, per_stanza)]


# ----------------------------------------------------------------------
# Classifying

_DEFINED_TOKENS = {
    PARAMETER: Name.Variable.Global,
    VARIABLE: Name.Variable,
    FUNCTION: Name.Function,
    CODE_BLOCK: Name.Function,
}


def _in_type_position(tok):
    """What a type comes after: ``x : R``, ``List of C``, ``R x R``."""
    return tok is not None and (tok.is_symbol(":") or tok.is_ident("of")
                                or tok.is_ident("x"))


def _classify_identifier(tok, before, before2, defs):
    n = tok.name
    if (n in _TYPES or n in ("List", "Table")) and _in_type_position(before):
        return Keyword.Type
    if (n in _TYPES and before is not None and before.is_symbol("(")
            and _in_type_position(before2)):
        return Keyword.Type
    if n == "of" and before is not None and before.is_ident("List"):
        return Keyword.Type
    if (n == "of" and before is not None and before.kind == "int"
            and before2 is not None and before2.is_ident("Table")):
        return Keyword.Type
    if n == "x" and before is not None and before.is_ident() and before.name in _TYPES:
        return Keyword.Type
    # (`drag start` and `drag end`, a tool handler's points)
    if (n in ("start", "end") and before is not None
            and before.kind == "reserved" and before.name == "drag"):
        return Name.Builtin.Pseudo
    # (a name the script defines is its own, even a built-in's)
    if n in defs:
        return _DEFINED_TOKENS[defs[n]]
    if n in _BUILTIN_FUNCTIONS:
        return Name.Builtin
    if n in _COLORS:
        return Name.Constant
    if n in _KEYWORDS:
        return Keyword
    if n == "color":
        return Name.Builtin.Pseudo
    if n == "infinity":
        return Keyword.Constant
    if n in _UNICODE_TYPES:
        return Keyword.Type
    return Name


def _classify(tok, before, before2, defs):
    kind = tok.kind
    if kind == "ident":
        return _classify_identifier(tok, before, before2, defs)
    if kind == "reserved":
        n = tok.name
        if n in _RESERVED_KEYWORDS:
            return Keyword
        if n in _RESERVED_CONSTANTS:
            return Keyword.Constant
        if n in _RESERVED_PSEUDO:
            return Name.Builtin.Pseudo
        return Name.Builtin
    if kind == "int":
        # (the number of dimensions in `Table 2 of C`)
        if (before is not None and before.is_ident("Table")
                and _in_type_position(before2)):
            return Keyword.Type
        return Number.Integer
    if kind in ("float", "sci"):
        return Number.Float
    if kind == "super":
        # (`ℙ¹`, in a type)
        if (tok.text == "¹" and before is not None and before.is_ident("ℙ")
                and _in_type_position(before2)):
            return Keyword.Type
        return Number.Integer
    if kind == "string":
        return String.Double
    if kind == "symbol":
        if tok.text in _DERIVATIVE_SIGNS:
            return Name.Builtin
        if tok.text == _INFINITY:
            return Keyword.Constant
        return Punctuation if tok.text in _PUNCTUATION else Operator
    if kind == "continuation":
        return Punctuation
    return Error


def _header_token(tok, nxt):
    """A token of a stanza's header line."""
    if tok.kind == "symbol" and tok.text == "@":
        return Name.Decorator
    if tok.kind in ("ident", "reserved") and nxt is not None and nxt.is_symbol("=", "=="):
        return Name.Attribute
    if tok.kind in ("ident", "reserved"):
        return Name
    return _classify(tok, None, None, {})


def _string_tokens(start, text):
    """A quoted string's pieces: its text, and its escapes."""
    pos = 0
    for m in _ESCAPE_RE.finditer(text):
        if m.start() > pos:
            yield start + pos, String.Double, text[pos:m.start()]
        yield start + m.start(), String.Escape, m.group()
        pos = m.end()
    if pos < len(text):
        yield start + pos, String.Double, text[pos:]


# ----------------------------------------------------------------------
# The lexer


class FractalStreamLexer(Lexer):
    """Lexer for FractalStream scripts: the code in a project's ``script:``
    block, or any part of it."""

    name = "FractalStream"
    aliases = ["fractalstream", "fs"]
    filenames = []
    mimetypes = ["text/x-fractalstream"]
    url = "https://github.com/matt-noonan/FractalStream"

    def __init__(self, **options):
        options.setdefault("stripnl", False)
        super().__init__(**options)

    def get_tokens_unprocessed(self, text):
        lines = []
        pos = 0
        for raw in text.splitlines(keepends=True):
            body = raw.rstrip("\r\n")
            lines.append((_Line(body, pos), raw[len(body):]))
            pos += len(raw)

        stanzas = _split_stanzas([l for l, _ in lines])
        defs_of = {}
        for stanza, defs in zip(stanzas, _all_definitions(stanzas)):
            for l in stanza.lines:
                defs_of[id(l)] = (stanza, defs)

        for line, newline in lines:
            yield from self._line_tokens(line, *defs_of[id(line)])
            if newline:
                yield line.start + len(line.text), Whitespace, newline

    def _line_tokens(self, line, stanza, defs):
        toks = line.tokens
        is_header = stanza.header is line

        # the event words of a handler's first line, up to its ':'
        event_words = set()
        if _is_handler(line):
            for t in toks[1:]:
                if t.is_symbol(":"):
                    break
                event_words.add(t.start)

        pos = 0
        if is_header and len(toks) >= 2 and toks[1].start == toks[0].end \
                and toks[1].kind in ("ident", "reserved"):
            # `@viewer`, as one token
            yield line.start, Name.Decorator, line.text[:toks[1].end]
            pos = toks[1].end
            toks = toks[2:]
        for k, tok in enumerate(toks):
            if tok.start > pos:
                yield line.start + pos, Whitespace, line.text[pos:tok.start]
            if is_header:
                ttype = _header_token(tok, toks[k + 1] if k + 1 < len(toks) else None)
            elif tok.start in event_words:
                ttype = Keyword
            else:
                before = toks[k - 1] if k >= 1 else None
                before2 = toks[k - 2] if k >= 2 else None
                ttype = _classify(tok, before, before2, defs)

            if ttype is String.Double:
                yield from _string_tokens(line.start + tok.start, tok.text)
            else:
                yield line.start + tok.start, ttype, tok.text
            pos = tok.end

        if line.comment is not None:
            if line.comment > pos:
                yield line.start + pos, Whitespace, line.text[pos:line.comment]
            yield line.start + line.comment, Comment.Single, line.text[line.comment:]
        elif pos < len(line.text):
            yield line.start + pos, Whitespace, line.text[pos:]


def setup(app):
    """Register the lexer with Sphinx, as ``fractalstream`` (and ``fs``)."""
    app.add_lexer("fractalstream", FractalStreamLexer)
    app.add_lexer("fs", FractalStreamLexer)
    return {"parallel_read_safe": True, "parallel_write_safe": True}
