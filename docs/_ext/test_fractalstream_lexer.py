"""Tests for the FractalStream Pygments lexer: ``pytest docs/_ext``."""

import os
import sys

import pytest
from pygments.token import (
    Comment, Error, Keyword, Name, Number, Operator, Punctuation, String,
    Whitespace,
)

sys.path.insert(0, os.path.dirname(__file__))
from fractalstream_lexer import FractalStreamLexer  # noqa: E402

LEXER = FractalStreamLexer()

EXAMPLE = '''\
@config "Parameters"
C : ℂ as "C =" := -0.12 + 0.75i
T : Table 2 of C
radius : R := 2   # a field named like a keyword

@common
f(z) := z² + C

define shade:
  if z escaped:
    color ⟵ blend (cos (pi m / 20)^2, dark blue, light yellow)

@viewer "Parameter plane" var=C center="-0.5" pixelSize=1/150
coloring:
  period <- 100
z := 0
iterate z -> f(z) until z escapes
run shade
x := 3.5e-2 + "a\\"b"

@tool "Trace" for="Parameter plane" shortcut=t
on click-or-drag at w:
  draw circle at w with radius 4 pixels
  for each p in [1, 2, ...
    3]:
    write text("z = ", z) at p
'''


def tokens(src):
    return list(LEXER.get_tokens_unprocessed(src))


def classes(src):
    """Each non-blank token's text and type, in order."""
    return [(v, t) for _, t, v in tokens(src) if t is not Whitespace]


def class_of(src, text, nth=0):
    """The type of the nth token with this text."""
    return [t for v, t in classes(src) if v == text][nth]


def test_round_trip():
    toks = tokens(EXAMPLE)
    assert "".join(v for _, _, v in toks) == EXAMPLE
    pos = 0
    for i, _, v in toks:
        assert i == pos
        pos += len(v)


def test_no_errors():
    assert not [v for v, t in classes(EXAMPLE) if t is Error]


def test_junk_is_an_error():
    assert class_of("x := 1 $ 2", "$") is Error


def test_stanza_headers():
    assert classes('@viewer "J" var=C center="-0.5"') == [
        ("@viewer", Name.Decorator),
        ('"J"', String.Double),
        ("var", Name.Attribute), ("=", Operator), ("C", Name),
        ("center", Name.Attribute), ("=", Operator), ('"-0.5"', String.Double),
    ]


@pytest.mark.parametrize("text, ttype", [
    ("if", Keyword), ("escaped", Keyword), ("define", Keyword),
    ("iterate", Keyword), ("until", Keyword), ("run", Keyword),
    ("coloring", Keyword), ("draw", Keyword), ("pixels", Keyword),
    ("pi", Keyword.Constant), ("i", Keyword.Constant),
    ("color", Name.Builtin.Pseudo),
    ("blend", Name.Builtin), ("cos", Name.Builtin), ("dark", Name.Builtin),
    ("text", Name.Builtin),
    ("blue", Name.Constant),
    ("ℂ", Keyword.Type), ("Table", Keyword.Type), ("of", Keyword.Type),
    ("0.12", Number.Float), ("3.5e-2", Number.Float),
    ("100", Number.Integer), ("²", Number.Integer),
    ("⟵", Operator), ("->", Operator), (":=", Operator),
    ("...", Punctuation),
    ("# a field named like a keyword", Comment.Single),
    ("\\\"", String.Escape),
])
def test_classes(text, ttype):
    assert class_of(EXAMPLE, text) is ttype


def test_definitions():
    # @config fields, and settings in a coloring: block, are parameters
    assert class_of(EXAMPLE, "C") is Name.Variable.Global
    assert class_of(EXAMPLE, "period") is Name.Variable.Global
    # functions and code blocks, where defined and where used
    assert class_of(EXAMPLE, "f", 0) is Name.Function
    assert class_of(EXAMPLE, "f", 1) is Name.Function
    assert class_of(EXAMPLE, "shade", 1) is Name.Function
    # variables, including a handler's point
    assert class_of(EXAMPLE, "x") is Name.Variable
    assert class_of(EXAMPLE, "w") is Name.Variable
    # an undefined name
    assert class_of(EXAMPLE, "m") is Name


def test_defined_statement_word_is_not_a_keyword():
    # as in the app: `radius` is a field here, so not a keyword
    assert class_of(EXAMPLE, "radius", 1) is Name.Variable.Global
    assert class_of("draw circle at 0 with radius 1", "radius") is Keyword


def test_handler_event_words():
    assert [t for v, t in classes("on click-or-drag at w:")][:6] == [Keyword] * 6


def test_types_only_in_type_position():
    assert class_of("C : C := 0", "C", 1) is Keyword.Type
    assert class_of("z := C", "C") is Name
    assert class_of("p : R x R := (0, 0)", "x") is Keyword.Type
    assert class_of("xs : List of Z := []", "of") is Keyword.Type


def test_hash_in_string_is_not_a_comment():
    assert classes('write "#1" at 0') == [
        ("write", Keyword), ('"#1"', String.Double), ("at", Keyword),
        ("0", Number.Integer),
    ]


def test_combining_marks_in_names():
    # x with a combining hat is one name
    assert classes("x̂ := 1")[0] == ("x̂", Name.Variable)
