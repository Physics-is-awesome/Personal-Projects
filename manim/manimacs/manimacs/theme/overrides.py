"""Generic deep-override support for immutable, nested dataclasses.

This has nothing Theme-specific about it on purpose: it operates on any
dataclass tree, so it keeps working unchanged as later phases add more
nested token types.
"""
from __future__ import annotations

import dataclasses
from typing import Any, TypeVar

T = TypeVar("T")


def deep_replace(obj: T, overrides: dict[str, Any]) -> T:
    """Return a copy of the dataclass instance *obj* with *overrides* applied.

    ``overrides`` is a (possibly nested) dict keyed by field name. A value
    that is itself a dict *and* the corresponding current field is a
    dataclass gets merged recursively; any other value replaces the field
    outright. Raises ``AttributeError`` on an unknown field name, so a
    typo in an override dict fails loudly instead of silently no-op'ing.
    """
    if not dataclasses.is_dataclass(obj):
        raise TypeError(f"deep_replace() target must be a dataclass instance, got {type(obj)!r}")

    field_names = {f.name for f in dataclasses.fields(obj)}
    changes: dict[str, Any] = {}
    for key, value in overrides.items():
        if key not in field_names:
            raise AttributeError(f"{type(obj).__name__} has no field named {key!r}")
        current = getattr(obj, key)
        if isinstance(value, dict) and dataclasses.is_dataclass(current):
            changes[key] = deep_replace(current, value)
        else:
            changes[key] = value
    return dataclasses.replace(obj, **changes)
