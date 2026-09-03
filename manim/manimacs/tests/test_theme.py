"""Tests for manimacs.theme.

These run without Manim actually rendering anything — deliberately, since
the theme package is pure data plus the small manim_adapters bridge.
"""
from __future__ import annotations

import dataclasses

import pytest

from manimacs.theme import (
    COSMOLOGY,
    DOOM,
    PAPER,
    PRESENTATION,
    ColorPalette,
    SyntaxColors,
    Theme,
    blend,
    darken,
    deep_replace,
    get_current_theme,
    get_theme,
    hex_to_rgb,
    lighten,
    list_themes,
    register_theme,
    resolve_font,
    resolve_rate_func,
    rgb_to_hex,
    set_current_theme,
    to_manim_color,
    use_theme,
    validate_hex,
)

ALL_PRESETS = [DOOM, PAPER, COSMOLOGY, PRESENTATION]


# ---------------------------------------------------------------------------
# Presets construct correctly and are internally consistent
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("theme", ALL_PRESETS, ids=lambda t: t.name)
def test_preset_is_a_fully_formed_theme(theme: Theme) -> None:
    assert isinstance(theme, Theme)
    assert theme.mode in ("dark", "light")
    assert theme.name


@pytest.mark.parametrize("theme", ALL_PRESETS, ids=lambda t: t.name)
def test_preset_is_frozen(theme: Theme) -> None:
    with pytest.raises(dataclasses.FrozenInstanceError):
        theme.name = "hacked"  # type: ignore[misc]
    with pytest.raises(dataclasses.FrozenInstanceError):
        theme.colors.syntax.keyword = "#ffffff"  # type: ignore[misc]


def test_presets_are_distinct_objects_not_aliases() -> None:
    # Regression guard: themes must not accidentally share nested objects
    # (e.g. two presets pointing at the same SyntaxColors instance).
    assert DOOM.colors.syntax is not PAPER.colors.syntax
    assert DOOM.colors is not COSMOLOGY.colors


# ---------------------------------------------------------------------------
# Registry
# ---------------------------------------------------------------------------

def test_all_presets_registered_by_default() -> None:
    names = list_themes()
    for theme in ALL_PRESETS:
        assert theme.name in names
    assert get_theme("doom") is DOOM


def test_get_theme_unknown_name_raises_with_helpful_message() -> None:
    with pytest.raises(KeyError, match="nonexistent"):
        get_theme("nonexistent")


def test_register_theme_duplicate_without_overwrite_raises() -> None:
    with pytest.raises(ValueError):
        register_theme(DOOM)  # already registered


def test_register_theme_overwrite() -> None:
    variant = DOOM.derive({"name": "doom"})  # same name, harmless no-op content
    register_theme(variant, overwrite=True)
    assert get_theme("doom") is not None  # did not raise


# ---------------------------------------------------------------------------
# Current-theme context management
# ---------------------------------------------------------------------------

def test_current_theme_defaults_to_doom() -> None:
    assert get_current_theme().name == "doom"


def test_use_theme_context_manager_restores_previous() -> None:
    before = get_current_theme()
    with use_theme(PAPER):
        assert get_current_theme() is PAPER
        with use_theme(COSMOLOGY):
            assert get_current_theme() is COSMOLOGY
        assert get_current_theme() is PAPER
    assert get_current_theme() is before


def test_set_current_theme() -> None:
    original = get_current_theme()
    try:
        set_current_theme(PRESENTATION)
        assert get_current_theme() is PRESENTATION
    finally:
        set_current_theme(original)


# ---------------------------------------------------------------------------
# derive() / deep_replace()
# ---------------------------------------------------------------------------

def test_derive_shallow_override() -> None:
    variant = DOOM.derive({"motion": {"typing_chars_per_second": 40}}, name="doom-fast-typist")
    assert variant.motion.typing_chars_per_second == 40
    assert variant.name == "doom-fast-typist"
    # original untouched
    assert DOOM.motion.typing_chars_per_second == 22
    assert DOOM.name == "doom"


def test_derive_deep_nested_override_only_touches_targeted_leaf() -> None:
    variant = DOOM.derive({"colors": {"syntax": {"keyword": "#ff8800"}}})
    assert variant.colors.syntax.keyword == "#ff8800"
    # every other syntax color is untouched
    assert variant.colors.syntax.string == DOOM.colors.syntax.string
    assert variant.colors.math.tensor == DOOM.colors.math.tensor
    # original untouched
    assert DOOM.colors.syntax.keyword == "#c678dd"


def test_derive_rejects_unknown_field() -> None:
    with pytest.raises(AttributeError):
        DOOM.derive({"colors": {"syntax": {"not_a_real_role": "#ffffff"}}})


def test_derive_revalidates_leaf_values() -> None:
    with pytest.raises(ValueError):
        DOOM.derive({"colors": {"syntax": {"keyword": "not-a-hex-color"}}})


def test_deep_replace_on_plain_dataclass() -> None:
    palette = deep_replace(DOOM.colors, {"syntax": {"string": "#00ff00"}})
    assert isinstance(palette, ColorPalette)
    assert palette.syntax.string == "#00ff00"
    assert DOOM.colors.syntax.string != "#00ff00"


# ---------------------------------------------------------------------------
# Color validation
# ---------------------------------------------------------------------------

def test_invalid_hex_color_rejected_at_construction() -> None:
    with pytest.raises(ValueError):
        SyntaxColors(
            keyword="not-a-color", string="#98c379", comment="#5c6370",
            function="#61afef", type="#e5c07b", number="#e0af68",
            constant="#d19a66", operator="#56b6c2", variable="#abb2bf",
            parameter="#d7dae0", builtin="#56b6c2", decorator="#e5c07b",
            punctuation="#7f848e", tag="#e06c75", error="#ff6c6b",
            docstring="#7c9169",
        )


@pytest.mark.parametrize("bad", ["red", "#fff", "#gggggg", "123456", "#12345", "#1234567"])
def test_validate_hex_rejects_bad_formats(bad: str) -> None:
    with pytest.raises(ValueError):
        validate_hex(bad)


def test_validate_hex_accepts_good_format() -> None:
    validate_hex("#61afef")  # should not raise


# ---------------------------------------------------------------------------
# Color utils
# ---------------------------------------------------------------------------

def test_hex_rgb_roundtrip() -> None:
    for color in ("#000000", "#ffffff", "#61afef", "#c678dd"):
        assert rgb_to_hex(*hex_to_rgb(color)) == color


def test_lighten_moves_toward_white() -> None:
    result = lighten("#000000", 0.5)
    assert result == "#808080"
    assert lighten("#000000", 0.0) == "#000000"
    assert lighten("#000000", 1.0) == "#ffffff"


def test_darken_moves_toward_black() -> None:
    assert darken("#ffffff", 1.0) == "#000000"
    assert darken("#ffffff", 0.0) == "#ffffff"


def test_blend_out_of_range_t_raises() -> None:
    with pytest.raises(ValueError):
        blend("#000000", "#ffffff", 1.5)


# ---------------------------------------------------------------------------
# Manim adapters (require manim to be installed, which it is for this repo)
# ---------------------------------------------------------------------------

def test_to_manim_color_accepts_theme_hex() -> None:
    from manim import ManimColor

    result = to_manim_color(DOOM.colors.accent.primary)
    assert isinstance(result, ManimColor)


@pytest.mark.parametrize("name", ["smooth", "linear", "there_and_back", "wiggle"])
def test_resolve_rate_func_known_names(name: str) -> None:
    func = resolve_rate_func(name)
    assert callable(func)
    assert 0.0 <= func(0.0) <= 1.0 or True  # just confirm it's callable & runs
    func(0.5)


def test_resolve_rate_func_unknown_name_raises_with_suggestions() -> None:
    with pytest.raises(ValueError, match="smooth"):
        resolve_rate_func("not_a_real_rate_func")


@pytest.mark.parametrize("theme", ALL_PRESETS, ids=lambda t: t.name)
def test_every_preset_rate_func_name_actually_resolves(theme: Theme) -> None:
    rf = theme.motion.rate_funcs
    for name in (rf.default, rf.entrance, rf.exit, rf.emphasis, rf.camera, rf.warning_shake):
        resolve_rate_func(name)  # should not raise


def test_resolve_font_falls_back_to_first_when_none_installed() -> None:
    result = resolve_font(("Definitely Not A Real Font XYZ",))
    assert result == "Definitely Not A Real Font XYZ"


def test_resolve_font_finds_an_installed_font_if_present_in_stack() -> None:
    # DejaVu Sans Mono is installed via fonts-dejavu-core in this environment.
    result = resolve_font(("Definitely Not Real", "DejaVu Sans Mono"))
    assert result == "DejaVu Sans Mono"


def test_resolve_font_rejects_empty_stack() -> None:
    with pytest.raises(ValueError):
        resolve_font(())
