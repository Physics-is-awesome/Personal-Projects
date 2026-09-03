"""Concrete theme presets.

DOOM is the primary preset: a dark, Doom-Emacs-*inspired* palette (close
in spirit to doom-one / atom-one-dark — dark blue-gray background, cyan
accents, magenta keywords, green strings, yellow/orange numbers, red
warnings) adapted for physics content rather than copied pixel-for-pixel,
per the brief.

PAPER, COSMOLOGY, and PRESENTATION exist to prove the token schema itself
is theme-agnostic — swapping any one of these into a scene changes its
entire visual identity without touching scene code. Their specific colors
are a reasonable starting point, not gospel; nudge them freely via
``.derive(...)``.
"""
from __future__ import annotations

from .colors import (
    AccentColors,
    ColorPalette,
    ForegroundColors,
    MathColors,
    PlotColors,
    StatusColors,
    SurfaceColors,
    SyntaxColors,
    UIColors,
)
from .components import CodeStyle, EquationStyle, PanelStyle, PlotStyle, TitleStyle
from .geometry import Borders, CursorStyle, Opacity, Radii, Shadow, Spacing, WindowGeometry
from .motion import Motion, PauseDurations, RateFuncs, TransitionDurations
from .theme import Theme
from .typography import FontSizes, FontWeights, Typography

# ---------------------------------------------------------------------------
# DOOM — dark, Doom-Emacs-inspired. The primary/default theme.
# ---------------------------------------------------------------------------
DOOM = Theme(
    name="doom",
    mode="dark",
    colors=ColorPalette(
        surface=SurfaceColors(
            base="#1e2127", primary="#282c34", elevated="#2c313a",
            overlay="#333842", sunken="#21252b", selection="#3e4451",
            line_highlight="#2c313c",
        ),
        foreground=ForegroundColors(
            primary="#bbc2cf", secondary="#a0a8b7", muted="#5c6370",
            faint="#4b5263", inverse="#282c34",
        ),
        accent=AccentColors(primary="#61afef", secondary="#c678dd", tertiary="#56b6c2"),
        syntax=SyntaxColors(
            keyword="#c678dd", string="#98c379", comment="#5c6370",
            function="#61afef", type="#e5c07b", number="#e0af68",
            constant="#d19a66", operator="#56b6c2", variable="#abb2bf",
            parameter="#d7dae0", builtin="#56b6c2", decorator="#e5c07b",
            punctuation="#7f848e", tag="#e06c75", error="#ff6c6b",
            docstring="#7c9169",
        ),
        math=MathColors(
            scalar="#bbc2cf", vector="#61afef", tensor="#c678dd",
            matrix="#a9a1e1", constant="#e0af68", variable="#bbc2cf",
            index="#56b6c2", operator="#82b1c1", differential="#56b6c2",
            integral="#56b6c2", function="#61afef", highlight="#f9c859",
            substitution="#61afef", new_term="#98c379", unchanged="#5c6370",
            warning="#e5c07b", error="#ff6c6b", annotation="#9ca3af",
        ),
        status=StatusColors(success="#98c379", warning="#e5c07b", error="#ff6c6b", info="#61afef"),
        ui=UIColors(
            border_default="#3b4048", border_subtle="#2c313a", border_focus="#61afef",
            cursor="#61afef", line_number="#495162", line_number_current="#abb2bf",
            modeline_active_bg="#21252b", modeline_active_fg="#bbc2cf",
            modeline_inactive_bg="#1c1f24", modeline_inactive_fg="#5c6370",
            minibuffer_bg="#21252b", minibuffer_fg="#bbc2cf",
            popup_bg="#2c313a", popup_border="#3b4048",
            file_tree_bg="#21252b", file_tree_fg="#9ca3af",
            file_tree_selected_bg="#2c313a", file_tree_selected_fg="#bbc2cf",
            file_tree_icon_dir="#61afef", file_tree_icon_file="#5c6370",
            shadow="#000000",
        ),
        plot=PlotColors(
            axis="#5c6370", gridline="#3b4048",
            series=("#61afef", "#c678dd", "#98c379", "#e0af68", "#e06c75", "#56b6c2"),
            error_band="#61afef", reference_line="#9ca3af",
        ),
    ),
    typography=Typography(
        code_font=("Iosevka", "Fira Code", "JetBrains Mono", "Cascadia Code", "Menlo", "Consolas", "DejaVu Sans Mono"),
        ui_font=("Iosevka Aile", "Inter", "SF Pro Display", "Segoe UI", "Helvetica Neue", "Arial", "DejaVu Sans"),
        math_font_package=None,
        sizes=FontSizes(
            modeline=18, code_small=20, caption=22, label=24, code=28, body=32,
            equation=40, equation_emphasis=48, subtitle=40, heading=56, title=72,
        ),
        weights=FontWeights(light="LIGHT", regular="NORMAL", medium="MEDIUM", bold="BOLD"),
    ),
    spacing=Spacing(xxs=0.05, xs=0.1, sm=0.2, md=0.35, lg=0.6, xl=1.0, xxl=1.6),
    radii=Radii(none=0.0, sm=0.05, md=0.1, lg=0.2, xl=0.35),
    borders=Borders(thin=1.0, default=2.0, thick=3.5, heavy=6.0),
    shadow=Shadow(opacity=0.35, offset_x=0.05, offset_y=-0.08, blur=0.12, layers=3),
    opacity=Opacity(disabled=0.4, dim=0.3, subtle=0.6, hover=0.85, scrim=0.5, full=1.0),
    window=WindowGeometry(default_width=12.0, default_height=7.0, title_bar_height=0.5,
                           modeline_height=0.35, padding=0.3, corner_radius=0.15),
    motion=Motion(
        typing_chars_per_second=22, typing_jitter=0.15,
        pauses=PauseDurations(beat=0.15, short=0.3, medium=0.75, long=1.5),
        transitions=TransitionDurations(fast=0.25, base=0.5, slow=0.9, camera=1.2),
        rate_funcs=RateFuncs(default="smooth", entrance="smooth", exit="smooth",
                              emphasis="there_and_back", camera="smooth", warning_shake="wiggle"),
    ),
    cursor=CursorStyle(shape="bar", width=0.04, blink=True, blink_period=1.0),
    code=CodeStyle(tab_width=2, show_line_numbers=True, show_indent_guides=True,
                    active_line_highlight=True, gutter_width=0.6, line_spacing=0.15),
    equation=EquationStyle(highlight_box_corner_radius=0.08, highlight_box_stroke_width=2.0,
                            highlight_box_fill_opacity=0.12, term_spacing=0.25,
                            row_spacing=0.5, arrow_stroke_width=3.0),
    plot=PlotStyle(axis_stroke_width=2.0, gridline_stroke_width=1.0, gridline_opacity=0.25,
                    line_width=3.0, point_radius=0.05, error_band_opacity=0.2,
                    error_bar_cap_size=0.08),
    title=TitleStyle(show_accent_bar=True, accent_bar_height=0.05, accent_bar_buffer=0.15),
    panel=PanelStyle(padding=0.3, corner_radius=0.12, border_width=2.0, show_shadow=True),
)

# ---------------------------------------------------------------------------
# PAPER — light academic theme: warm paper background, restrained ink colors.
# ---------------------------------------------------------------------------
PAPER = Theme(
    name="paper",
    mode="light",
    colors=ColorPalette(
        surface=SurfaceColors(
            base="#eee7d6", primary="#faf6ee", elevated="#f0e9d8",
            overlay="#ffffff", sunken="#e5ddc8", selection="#d9cba8",
            line_highlight="#f2ecdd",
        ),
        foreground=ForegroundColors(
            primary="#2b2622", secondary="#4a443c", muted="#6b6255",
            faint="#948b7a", inverse="#faf6ee",
        ),
        accent=AccentColors(primary="#1d6fa5", secondary="#7a4fa3", tertiary="#2f7d82"),
        syntax=SyntaxColors(
            keyword="#7a4fa3", string="#3e7a3e", comment="#948b7a",
            function="#1d6fa5", type="#a6740e", number="#b5651d",
            constant="#a6580e", operator="#2f7d82", variable="#463f37",
            parameter="#5a5248", builtin="#2f7d82", decorator="#a6740e",
            punctuation="#7a7266", tag="#9c3d54", error="#b3261e",
            docstring="#5f8f5f",
        ),
        math=MathColors(
            scalar="#2b2622", vector="#1d6fa5", tensor="#7a4fa3",
            matrix="#5b3f8f", constant="#b5651d", variable="#2b2622",
            index="#2f7d82", operator="#3c6e73", differential="#2f7d82",
            integral="#2f7d82", function="#1d6fa5", highlight="#c2410c",
            substitution="#1d6fa5", new_term="#3e7a3e", unchanged="#a39c8c",
            warning="#a6740e", error="#b3261e", annotation="#6b6255",
        ),
        status=StatusColors(success="#3e7a3e", warning="#a6740e", error="#b3261e", info="#1d6fa5"),
        ui=UIColors(
            border_default="#d9cfb8", border_subtle="#e5ddc8", border_focus="#1d6fa5",
            cursor="#1d6fa5", line_number="#b9ae95", line_number_current="#6b6255",
            modeline_active_bg="#e5ddc8", modeline_active_fg="#2b2622",
            modeline_inactive_bg="#eee7d6", modeline_inactive_fg="#948b7a",
            minibuffer_bg="#e5ddc8", minibuffer_fg="#2b2622",
            popup_bg="#ffffff", popup_border="#d9cfb8",
            file_tree_bg="#f0e9d8", file_tree_fg="#4a443c",
            file_tree_selected_bg="#d9cba8", file_tree_selected_fg="#2b2622",
            file_tree_icon_dir="#1d6fa5", file_tree_icon_file="#948b7a",
            shadow="#3a3226",
        ),
        plot=PlotColors(
            axis="#6b6255", gridline="#d9cfb8",
            series=("#1d6fa5", "#7a4fa3", "#3e7a3e", "#b5651d", "#9c3d54", "#2f7d82"),
            error_band="#1d6fa5", reference_line="#948b7a",
        ),
    ),
    typography=Typography(
        code_font=("Iosevka", "Fira Code", "JetBrains Mono", "Menlo", "Consolas", "DejaVu Sans Mono"),
        ui_font=("Iosevka Aile", "Lato", "Georgia", "Palatino", "DejaVu Serif"),
        math_font_package="newtxmath",
        sizes=FontSizes(
            modeline=18, code_small=20, caption=22, label=24, code=28, body=32,
            equation=40, equation_emphasis=48, subtitle=38, heading=52, title=68,
        ),
        weights=FontWeights(light="LIGHT", regular="NORMAL", medium="MEDIUM", bold="BOLD"),
    ),
    spacing=Spacing(xxs=0.06, xs=0.12, sm=0.22, md=0.4, lg=0.65, xl=1.05, xxl=1.7),
    radii=Radii(none=0.0, sm=0.03, md=0.06, lg=0.12, xl=0.2),
    borders=Borders(thin=1.0, default=1.5, thick=2.5, heavy=4.0),
    shadow=Shadow(opacity=0.18, offset_x=0.04, offset_y=-0.06, blur=0.1, layers=2),
    opacity=Opacity(disabled=0.4, dim=0.35, subtle=0.6, hover=0.85, scrim=0.4, full=1.0),
    window=WindowGeometry(default_width=12.0, default_height=7.0, title_bar_height=0.5,
                           modeline_height=0.35, padding=0.32, corner_radius=0.08),
    motion=Motion(
        typing_chars_per_second=20, typing_jitter=0.12,
        pauses=PauseDurations(beat=0.15, short=0.35, medium=0.85, long=1.6),
        transitions=TransitionDurations(fast=0.28, base=0.55, slow=0.95, camera=1.3),
        rate_funcs=RateFuncs(default="smooth", entrance="smooth", exit="smooth",
                              emphasis="there_and_back", camera="smooth", warning_shake="wiggle"),
    ),
    cursor=CursorStyle(shape="bar", width=0.035, blink=True, blink_period=1.0),
    code=CodeStyle(tab_width=2, show_line_numbers=True, show_indent_guides=False,
                    active_line_highlight=True, gutter_width=0.6, line_spacing=0.16),
    equation=EquationStyle(highlight_box_corner_radius=0.05, highlight_box_stroke_width=1.5,
                            highlight_box_fill_opacity=0.1, term_spacing=0.25,
                            row_spacing=0.52, arrow_stroke_width=2.5),
    plot=PlotStyle(axis_stroke_width=1.5, gridline_stroke_width=0.75, gridline_opacity=0.35,
                    line_width=2.5, point_radius=0.045, error_band_opacity=0.15,
                    error_bar_cap_size=0.07),
    title=TitleStyle(show_accent_bar=True, accent_bar_height=0.04, accent_bar_buffer=0.15),
    panel=PanelStyle(padding=0.32, corner_radius=0.06, border_width=1.5, show_shadow=True),
)

# ---------------------------------------------------------------------------
# COSMOLOGY — spacetime/cosmology theme: deep-space blacks, starlight, glow.
# ---------------------------------------------------------------------------
COSMOLOGY = Theme(
    name="cosmology",
    mode="dark",
    colors=ColorPalette(
        surface=SurfaceColors(
            base="#05060a", primary="#0b0e1a", elevated="#121629",
            overlay="#1a1f38", sunken="#060810", selection="#232a4d",
            line_highlight="#11152a",
        ),
        foreground=ForegroundColors(
            primary="#e8ecf7", secondary="#aab2cc", muted="#5f6785",
            faint="#3d4463", inverse="#0b0e1a",
        ),
        accent=AccentColors(primary="#7dd3fc", secondary="#a78bfa", tertiary="#34d399"),
        syntax=SyntaxColors(
            keyword="#a78bfa", string="#34d399", comment="#5f6785",
            function="#7dd3fc", type="#fbbf24", number="#fb923c",
            constant="#fbbf24", operator="#38bdf8", variable="#c7cdea",
            parameter="#dfe3f5", builtin="#38bdf8", decorator="#fbbf24",
            punctuation="#7b83a8", tag="#fb7185", error="#f43f5e",
            docstring="#5fb894",
        ),
        math=MathColors(
            scalar="#e8ecf7", vector="#7dd3fc", tensor="#a78bfa",
            matrix="#c4b5fd", constant="#fbbf24", variable="#e8ecf7",
            index="#38bdf8", operator="#67c9e8", differential="#38bdf8",
            integral="#38bdf8", function="#7dd3fc", highlight="#fde047",
            substitution="#7dd3fc", new_term="#34d399", unchanged="#5f6785",
            warning="#fbbf24", error="#fb7185", annotation="#aab2cc",
        ),
        status=StatusColors(success="#34d399", warning="#fbbf24", error="#fb7185", info="#7dd3fc"),
        ui=UIColors(
            border_default="#262c4d", border_subtle="#171b33", border_focus="#7dd3fc",
            cursor="#7dd3fc", line_number="#3d4463", line_number_current="#aab2cc",
            modeline_active_bg="#0f1226", modeline_active_fg="#e8ecf7",
            modeline_inactive_bg="#08091a", modeline_inactive_fg="#5f6785",
            minibuffer_bg="#0f1226", minibuffer_fg="#e8ecf7",
            popup_bg="#151a33", popup_border="#262c4d",
            file_tree_bg="#0f1226", file_tree_fg="#aab2cc",
            file_tree_selected_bg="#1a1f38", file_tree_selected_fg="#e8ecf7",
            file_tree_icon_dir="#7dd3fc", file_tree_icon_file="#5f6785",
            shadow="#000000",
        ),
        plot=PlotColors(
            axis="#5f6785", gridline="#1e2340",
            series=("#7dd3fc", "#a78bfa", "#34d399", "#fbbf24", "#fb7185", "#38bdf8"),
            error_band="#7dd3fc", reference_line="#aab2cc",
        ),
    ),
    typography=Typography(
        code_font=("Iosevka", "JetBrains Mono", "Fira Code", "Menlo", "Consolas", "DejaVu Sans Mono"),
        ui_font=("Iosevka Aile", "Inter", "Space Grotesk", "Segoe UI", "DejaVu Sans"),
        math_font_package=None,
        sizes=FontSizes(
            modeline=18, code_small=20, caption=22, label=24, code=28, body=32,
            equation=42, equation_emphasis=50, subtitle=40, heading=56, title=72,
        ),
        weights=FontWeights(light="LIGHT", regular="NORMAL", medium="MEDIUM", bold="BOLD"),
    ),
    spacing=Spacing(xxs=0.05, xs=0.1, sm=0.2, md=0.35, lg=0.6, xl=1.0, xxl=1.6),
    radii=Radii(none=0.0, sm=0.05, md=0.1, lg=0.22, xl=0.4),
    borders=Borders(thin=1.0, default=1.75, thick=3.0, heavy=5.0),
    shadow=Shadow(opacity=0.45, offset_x=0.0, offset_y=-0.1, blur=0.2, layers=4),
    opacity=Opacity(disabled=0.4, dim=0.28, subtle=0.55, hover=0.85, scrim=0.55, full=1.0),
    window=WindowGeometry(default_width=12.0, default_height=7.0, title_bar_height=0.5,
                           modeline_height=0.35, padding=0.3, corner_radius=0.18),
    motion=Motion(
        typing_chars_per_second=20, typing_jitter=0.18,
        pauses=PauseDurations(beat=0.18, short=0.4, medium=0.9, long=1.8),
        transitions=TransitionDurations(fast=0.3, base=0.6, slow=1.1, camera=1.6),
        rate_funcs=RateFuncs(default="smooth", entrance="smooth", exit="smooth",
                              emphasis="there_and_back", camera="smooth", warning_shake="wiggle"),
    ),
    cursor=CursorStyle(shape="block", width=0.05, blink=True, blink_period=1.2),
    code=CodeStyle(tab_width=2, show_line_numbers=True, show_indent_guides=True,
                    active_line_highlight=True, gutter_width=0.6, line_spacing=0.16),
    equation=EquationStyle(highlight_box_corner_radius=0.1, highlight_box_stroke_width=2.0,
                            highlight_box_fill_opacity=0.14, term_spacing=0.28,
                            row_spacing=0.55, arrow_stroke_width=3.0),
    plot=PlotStyle(axis_stroke_width=2.0, gridline_stroke_width=1.0, gridline_opacity=0.2,
                    line_width=3.0, point_radius=0.055, error_band_opacity=0.22,
                    error_bar_cap_size=0.08),
    title=TitleStyle(show_accent_bar=True, accent_bar_height=0.05, accent_bar_buffer=0.18),
    panel=PanelStyle(padding=0.32, corner_radius=0.16, border_width=1.75, show_shadow=True),
)

# ---------------------------------------------------------------------------
# PRESENTATION — clean, high-contrast, light. Bigger type, minimal effects.
# ---------------------------------------------------------------------------
PRESENTATION = Theme(
    name="presentation",
    mode="light",
    colors=ColorPalette(
        surface=SurfaceColors(
            base="#f2f2f4", primary="#ffffff", elevated="#f0f0f2",
            overlay="#ffffff", sunken="#e7e7ea", selection="#cfe0fc",
            line_highlight="#f5f7fb",
        ),
        foreground=ForegroundColors(
            primary="#16181d", secondary="#3f434c", muted="#6b7280",
            faint="#9aa0ab", inverse="#ffffff",
        ),
        accent=AccentColors(primary="#2563eb", secondary="#9333ea", tertiary="#0d9488"),
        syntax=SyntaxColors(
            keyword="#9333ea", string="#16a34a", comment="#6b7280",
            function="#2563eb", type="#b45309", number="#c2410c",
            constant="#b45309", operator="#0891b2", variable="#1f2430",
            parameter="#374151", builtin="#0891b2", decorator="#b45309",
            punctuation="#6b7280", tag="#be185d", error="#dc2626",
            docstring="#3f8f52",
        ),
        math=MathColors(
            scalar="#16181d", vector="#2563eb", tensor="#9333ea",
            matrix="#7c3aed", constant="#c2410c", variable="#16181d",
            index="#0891b2", operator="#0e7c8f", differential="#0891b2",
            integral="#0891b2", function="#2563eb", highlight="#dc2626",
            substitution="#2563eb", new_term="#16a34a", unchanged="#9aa0ab",
            warning="#b45309", error="#dc2626", annotation="#6b7280",
        ),
        status=StatusColors(success="#16a34a", warning="#b45309", error="#dc2626", info="#2563eb"),
        ui=UIColors(
            border_default="#d8dbe0", border_subtle="#e7e7ea", border_focus="#2563eb",
            cursor="#2563eb", line_number="#b7bcc6", line_number_current="#3f434c",
            modeline_active_bg="#e7e7ea", modeline_active_fg="#16181d",
            modeline_inactive_bg="#f2f2f4", modeline_inactive_fg="#9aa0ab",
            minibuffer_bg="#e7e7ea", minibuffer_fg="#16181d",
            popup_bg="#ffffff", popup_border="#d8dbe0",
            file_tree_bg="#f0f0f2", file_tree_fg="#3f434c",
            file_tree_selected_bg="#cfe0fc", file_tree_selected_fg="#16181d",
            file_tree_icon_dir="#2563eb", file_tree_icon_file="#9aa0ab",
            shadow="#1f2430",
        ),
        plot=PlotColors(
            axis="#3f434c", gridline="#d8dbe0",
            series=("#2563eb", "#9333ea", "#16a34a", "#c2410c", "#be185d", "#0891b2"),
            error_band="#2563eb", reference_line="#9aa0ab",
        ),
    ),
    typography=Typography(
        code_font=("JetBrains Mono", "Iosevka", "Fira Code", "Menlo", "Consolas", "DejaVu Sans Mono"),
        ui_font=("Inter", "Iosevka Aile", "Helvetica Neue", "Arial", "DejaVu Sans"),
        math_font_package=None,
        sizes=FontSizes(
            modeline=22, code_small=26, caption=28, label=30, code=34, body=38,
            equation=48, equation_emphasis=58, subtitle=46, heading=64, title=84,
        ),
        weights=FontWeights(light="LIGHT", regular="NORMAL", medium="MEDIUM", bold="BOLD"),
    ),
    spacing=Spacing(xxs=0.06, xs=0.12, sm=0.24, md=0.42, lg=0.7, xl=1.15, xxl=1.85),
    radii=Radii(none=0.0, sm=0.04, md=0.09, lg=0.16, xl=0.28),
    borders=Borders(thin=1.5, default=2.5, thick=4.0, heavy=7.0),
    shadow=Shadow(opacity=0.12, offset_x=0.03, offset_y=-0.05, blur=0.08, layers=2),
    opacity=Opacity(disabled=0.4, dim=0.35, subtle=0.65, hover=0.9, scrim=0.35, full=1.0),
    window=WindowGeometry(default_width=12.5, default_height=7.2, title_bar_height=0.55,
                           modeline_height=0.4, padding=0.35, corner_radius=0.1),
    motion=Motion(
        typing_chars_per_second=24, typing_jitter=0.1,
        pauses=PauseDurations(beat=0.2, short=0.45, medium=1.0, long=1.9),
        transitions=TransitionDurations(fast=0.3, base=0.6, slow=1.0, camera=1.3),
        rate_funcs=RateFuncs(default="smooth", entrance="smooth", exit="smooth",
                              emphasis="there_and_back", camera="smooth", warning_shake="wiggle"),
    ),
    cursor=CursorStyle(shape="bar", width=0.045, blink=True, blink_period=1.0),
    code=CodeStyle(tab_width=2, show_line_numbers=True, show_indent_guides=False,
                    active_line_highlight=True, gutter_width=0.65, line_spacing=0.18),
    equation=EquationStyle(highlight_box_corner_radius=0.07, highlight_box_stroke_width=2.5,
                            highlight_box_fill_opacity=0.1, term_spacing=0.3,
                            row_spacing=0.58, arrow_stroke_width=3.5),
    plot=PlotStyle(axis_stroke_width=2.5, gridline_stroke_width=1.0, gridline_opacity=0.3,
                    line_width=3.5, point_radius=0.06, error_band_opacity=0.16,
                    error_bar_cap_size=0.09),
    title=TitleStyle(show_accent_bar=True, accent_bar_height=0.06, accent_bar_buffer=0.18),
    panel=PanelStyle(padding=0.35, corner_radius=0.1, border_width=2.0, show_shadow=True),
)
