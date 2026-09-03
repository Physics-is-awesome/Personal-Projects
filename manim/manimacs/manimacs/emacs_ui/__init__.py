"""manimacs.emacs_ui — Emacs-inspired UI components (Phase 2).

    from manimacs.emacs_ui import EmacsWindow, CodeBuffer, OrgDocument

Everything here is a Manim ``VGroup`` subclass, so it composes with
ordinary Manim idioms: ``self.play(FadeIn(window))``, ``window.shift(...)``,
``self.add(buffer)``, etc.

What's *not* here on purpose: typing/scrolling/fold *animation*, cursor
blinking/movement, selection/deletion/undo animation, and search — all of
that is Phase 3 ("typing, cursor, scrolling, and editing animations"),
built as a layer over what these components expose (``get_line()``,
``set_code()``, the ``folded`` flag, ...).
"""
from __future__ import annotations

from .code_buffer import CodeBuffer
from .compile_buffer import CompileBuffer
from .cursor import EmacsCursor
from .file_tree import FileTree, FileTreeEntry
from .minibuffer import MiniBuffer
from .modeline import ModeLine
from .org_document import OrgDocument, OrgHeading
from .popup import PopupWindow
from .scratch_buffer import SCRATCH_BUFFER_NAME, scratch_buffer
from .status import StatusIndicator
from .syntax import highlight_code_lines
from .window import EmacsWindow

__all__ = [
    "EmacsWindow",
    "CodeBuffer",
    "OrgDocument", "OrgHeading",
    "ModeLine",
    "MiniBuffer",
    "FileTree", "FileTreeEntry",
    "PopupWindow",
    "scratch_buffer", "SCRATCH_BUFFER_NAME",
    "EmacsCursor",
    "CompileBuffer",
    "StatusIndicator",
    "highlight_code_lines",
]
