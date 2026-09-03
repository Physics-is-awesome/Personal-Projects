"""Syntax highlighting: pygments tokenization mapped to ``theme.colors.syntax``
roles, rendered as Pango markup that ``MarkupText`` can consume directly.

This is deliberately *not* "animated syntax highlighting" — that's Phase 3's
job (revealing highlighted code progressively as it's "typed"). This module
only answers "what color is this token", which ``CodeBuffer`` and
``OrgDocument``'s embedded code blocks both need as a static capability.

Using real pygments rather than a hand-rolled regex tokenizer means every
language pygments supports works here for free, and the tokenization itself
is correct.
"""
from __future__ import annotations

from xml.sax.saxutils import escape

from pygments import lex
from pygments.lexers import get_lexer_by_name
from pygments.token import Token
from pygments.util import ClassNotFound

from ..theme import Theme

# Ordered most-specific-first: pygments token types form a hierarchy
# (Name.Function is a subtype of Name), so the first matching prefix wins.
_TOKEN_ROLE_MAP: tuple[tuple, str] = (
    (Token.Keyword, "keyword"),
    (Token.Name.Builtin.Pseudo, "constant"),
    (Token.Name.Builtin, "builtin"),
    (Token.Name.Function, "function"),
    (Token.Name.Class, "type"),
    (Token.Name.Decorator, "decorator"),
    (Token.Name.Tag, "tag"),
    (Token.Name.Attribute, "parameter"),
    (Token.Name.Namespace, "type"),
    (Token.Name, "variable"),
    (Token.Literal.String.Doc, "docstring"),
    (Token.Literal.String, "string"),
    (Token.Literal.Number, "number"),
    (Token.Comment, "comment"),
    (Token.Operator, "operator"),
    (Token.Punctuation, "punctuation"),
    (Token.Generic.Error, "error"),
    (Token.Error, "error"),
)


def _role_for_token(token_type) -> str:
    for prefix, role in _TOKEN_ROLE_MAP:
        if token_type in prefix:
            return role
    return "variable"


def highlight_code_lines(code: str, language: str, theme: Theme) -> list[str]:
    """Tokenize *code* and return one Pango-markup string per source line.

    Always returns exactly ``len(code.splitlines()) or 1`` entries, so
    callers can zip the result against line numbers without special-casing
    trailing newlines (pygments lexers are inconsistent about whether they
    emit a token for a final trailing newline).
    """
    try:
        lexer = get_lexer_by_name(language, stripnl=False, stripall=False)
    except ClassNotFound:
        lexer = get_lexer_by_name("text", stripnl=False, stripall=False)

    lines: list[str] = []
    current: list[str] = []
    for token_type, text in lex(code, lexer):
        role = _role_for_token(token_type)
        color = getattr(theme.colors.syntax, role, theme.colors.foreground.primary)
        parts = text.split("\n")
        for i, part in enumerate(parts):
            if part:
                current.append(f'<span foreground="{color}">{escape(part)}</span>')
            if i < len(parts) - 1:
                lines.append("".join(current))
                current = []
    lines.append("".join(current))

    expected = len(code.splitlines()) or 1
    lines = lines[:expected]
    while len(lines) < expected:
        lines.append("")
    return lines
