"""A scratch buffer is just a ``CodeBuffer`` with scratch-buffer
conventions (name, default comment) — a factory function rather than a
subclass, since there's no behavioral difference once constructed."""
from __future__ import annotations

from ..theme import Theme
from .code_buffer import CodeBuffer

SCRATCH_BUFFER_NAME = "*scratch*"
_DEFAULT_COMMENT = "# This buffer is for text that is not saved.\n\n"


def scratch_buffer(theme: Theme, code: str = "", *, language: str = "python", width: float | None = None) -> CodeBuffer:
    return CodeBuffer(theme, _DEFAULT_COMMENT + code, language=language, width=width)
