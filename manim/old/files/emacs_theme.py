"""
emacs_theme.py
================================================================================
Shared visual language for "Can You Destroy a Universe With a Force?"

The whole video is staged as though the audience is looking over someone's
shoulder at a dark-themed Emacs buffer (palette modeled on the popular
"Doom One" theme) editing `universe-force.tex`. Every scene reuses:

  * one color palette (BG / FG / syntax colors)
  * one monospace font
  * a `BufferChrome` widget: a thin tab/header bar + a mode-line footer
    that is updated per-scene to show which "section" is being narrated
  * a blinking-cursor mobject
  * `code_line()` for hand-highlighted fake LaTeX source (keyword / brace /
    argument coloring, the way font-lock-mode would color it)
  * `checklist_item()` for org-mode style "- [ ] / - [X]" bullets
  * `scale_bar()` for the big log-scale force comparison at the end

Import everything from here into `universe_force.py`.
================================================================================
"""

import numpy as np
from manim import *

# ---------------------------------------------------------------------------
# Palette -- "Doom One" flavored dark Emacs theme
# ---------------------------------------------------------------------------
BG          = "#282C34"   # buffer background
FG          = "#BBC2CF"   # default prose text
FG_DIM      = "#5B6268"   # comments / punctuation / de-emphasis
BLUE        = "#51AFEF"   # keywords            (\section, \frac, \nabla ...)
GREEN       = "#98BE65"   # confirmed / "good"   results, strings
ORANGE      = "#DA8548"   # numbers / constants
RED         = "#FF6C6B"   # warnings, errors, danger call-outs
PURPLE      = "#C678DD"   # section / titles
TEAL        = "#46D9FF"   # cursor + accent highlights
YELLOW      = "#ECBE7B"   # variables / emphasis
MODELINE_BG = "#1C1F24"
MODELINE_FG = "#9CA0A4"
REGION      = "#3E4451"   # selection / chip background

FONT = "DejaVu Sans Mono"   # widely available monospace; swap for
                            # "JetBrains Mono" / "Fira Code" / "Iosevka"
                            # if you have one of those installed.

config.background_color = BG

# Safe horizontal space for body content (keeps text off the frame edges).
CONTENT_WIDTH = config.frame_width - 1.6


def fit_width(mobj, max_width=None):
    """Scale `mobj` down (never up) so it never overflows the safe content
    width. Call this on any text block whose length isn't hand-verified --
    it's a cheap insurance policy against off-frame text."""
    max_width = max_width if max_width is not None else CONTENT_WIDTH
    if mobj.width > max_width:
        mobj.scale_to_fit_width(max_width)
    return mobj


# ---------------------------------------------------------------------------
# Small helpers
# ---------------------------------------------------------------------------
def code_line(parts, font_size=32, font=FONT, buff=0.03, aligned_edge=DOWN):
    """
    Build one line of hand-syntax-highlighted "code" out of (string, color)
    pairs, e.g.

        code_line([("\\section", BLUE), ("{", FG_DIM),
                    ("Planck Force", PURPLE), ("}", FG_DIM)])

    Avoids Text(t2c=...) footguns (which recolor *every* occurrence of a
    substring) by building each run as its own Text mobject and arranging
    them in a row.
    """
    mobs = [Text(s, font=font, font_size=font_size, color=c) for s, c in parts]
    return VGroup(*mobs).arrange(RIGHT, buff=buff, aligned_edge=aligned_edge)


def prose(lines, font_size=30, color=FG, font=FONT, line_spacing=0.55, aligned_edge=LEFT):
    """A left-aligned multi-line paragraph, revealed as one VGroup."""
    mobs = [Text(l, font=font, font_size=font_size, color=color) for l in lines]
    return VGroup(*mobs).arrange(DOWN, buff=line_spacing, aligned_edge=aligned_edge)


def checklist_item(text, checked=False, font_size=30, font=FONT):
    box = "[x]" if checked else "[ ]"
    box_color = GREEN if checked else FG_DIM
    return code_line(
        [("- ", FG_DIM), (box, box_color), (" " + text, FG)],
        font_size=font_size, font=font,
    )


def comment_line(text, font_size=26, font=FONT):
    """A '%% ...' LaTeX-comment styled aside, in the muted comment color."""
    return code_line([("%%", FG_DIM), (text, FG_DIM)], font_size=font_size, font=font, buff=0.12)


class Cursor(Rectangle):
    """A blinking block cursor, Emacs-style."""

    def __init__(self, height=0.42, width=0.16, color=TEAL, **kwargs):
        super().__init__(
            height=height, width=width,
            fill_color=color, fill_opacity=1, stroke_width=0, **kwargs
        )


def blink(cursor, cycles=2, on_time=0.35, off_time=0.25):
    """Return a Succession of opacity toggles for `cursor` (pass to self.play)."""
    anims = []
    for _ in range(cycles):
        anims.append(cursor.animate.set_opacity(0.15))
        anims.append(cursor.animate.set_opacity(1))
    return Succession(*anims, run_time=cycles * (on_time + off_time))


class BufferChrome(VGroup):
    """
    The Emacs "chrome": a tab bar pinned to the top edge of the frame and a
    mode-line pinned to the bottom edge. Call `.set_status(...)` to update
    the mode-line text when a scene moves into a new section.
    """

    def __init__(self, filename="universe-force.tex", mode="LaTeX/Preview",
                 position="Top", tab="universe-force.tex", **kwargs):
        super().__init__(**kwargs)
        w = config.frame_width
        fh = config.frame_height

        # top tab bar
        self.top_bar = Rectangle(width=w, height=0.5, fill_color=MODELINE_BG,
                                  fill_opacity=1, stroke_width=0)
        self.top_bar.move_to(UP * (fh / 2 - 0.25))
        tab_txt = Text(f" {tab} ", font=FONT, font_size=18, color=FG)
        tab_bg = SurroundingRectangle(tab_txt, color=BLUE, fill_color=REGION,
                                       fill_opacity=1, buff=0.08, stroke_width=1.5)
        self.tab = VGroup(tab_bg, tab_txt)
        self.tab.next_to(self.top_bar.get_left(), RIGHT, buff=0.25)
        self.tab.move_to([self.tab.get_center()[0], self.top_bar.get_center()[1], 0])

        # bottom mode-line
        self.bottom_bar = Rectangle(width=w, height=0.4, fill_color=MODELINE_BG,
                                     fill_opacity=1, stroke_width=0)
        self.bottom_bar.move_to(DOWN * (fh / 2 - 0.2))
        self.left_txt = Text(f"-:**-  {filename}", font=FONT, font_size=16, color=MODELINE_FG)
        self.right_txt = Text(f"{position}   ({mode})", font=FONT, font_size=16, color=MODELINE_FG)
        self.left_txt.next_to(self.bottom_bar.get_left(), RIGHT, buff=0.25)
        self.left_txt.move_to([self.left_txt.get_center()[0], self.bottom_bar.get_center()[1], 0])
        self.right_txt.next_to(self.bottom_bar.get_right(), LEFT, buff=0.25)
        self.right_txt.move_to([self.right_txt.get_center()[0], self.bottom_bar.get_center()[1], 0])

        self.add(self.top_bar, self.tab, self.bottom_bar, self.left_txt, self.right_txt)
        # subtle left-edge fringe, sits behind the tab/mode-line (added last
        # here only so it's easy to find; visually it reads as "behind" since
        # it's a thin 2px rule the bars simply overlap at top and bottom).
        self.fringe_line = fringe()
        self.add_to_back(self.fringe_line)

    def status_text(self, position="Top", mode="LaTeX/Preview"):
        """Return a *new* Text mobject matching the right-hand mode-line slot
        so callers can Transform into it (keeps position anchored)."""
        new_right = Text(f"{position}   ({mode})", font=FONT, font_size=16, color=MODELINE_FG)
        new_right.move_to(self.right_txt)
        new_right.align_to(self.bottom_bar.get_right() + LEFT * 0.25, RIGHT)
        return new_right


# ---------------------------------------------------------------------------
# Log-scale comparison bar (used in the Results scene)
# ---------------------------------------------------------------------------
def scale_bar(min_exp, max_exp, width=11, **kwargs):
    """
    A NumberLine labeled in powers of ten (exponents from min_exp to max_exp),
    meant to plot wildly different force magnitudes on one readable axis.
    """
    line = NumberLine(
        x_range=[min_exp, max_exp, 5],
        length=width,
        color=FG_DIM,
        include_numbers=False,
        include_tip=True,
        tip_length=0.2,
        **kwargs,
    )
    return line


def scale_marker(line, exponent, label, color=ORANGE, font_size=22, direction=UP, dot_radius=0.07):
    """A dot + label placed at `10**exponent` along a scale_bar NumberLine."""
    point = line.n2p(exponent)
    dot = Dot(point, radius=dot_radius, color=color)
    txt = Text(label, font=FONT, font_size=font_size, color=color)
    txt.next_to(dot, direction, buff=0.18)
    return VGroup(dot, txt)


# ---------------------------------------------------------------------------
# Atmosphere: a faint starfield for the more "cosmic" scenes, and a thin
# fringe rule down the left edge of the buffer (subtle Emacs fringe/gutter
# nod, sits behind the content and never competes with it).
# ---------------------------------------------------------------------------
def starfield(n=80, seed=0, opacity_range=(0.12, 0.55), radius_range=(0.008, 0.03)):
    rng = np.random.default_rng(seed)
    stars = VGroup()
    
    return stars


def fringe():
    x = -config.frame_width / 2 + 0.06
    return Line(
        [x, -config.frame_height / 2 + 0.2, 0], [x, config.frame_height / 2 - 0.2, 0],
        color=REGION, stroke_width=2, stroke_opacity=0.8,
    )
