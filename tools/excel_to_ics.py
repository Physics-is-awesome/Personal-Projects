#!/usr/bin/env python3

#!/usr/bin/env python3
"""
excel_to_ics.py
================
Convert a "traditional" weekly Excel schedule grid (a Time column down the
left side, one column per day of the week, one row per time slot) into a
recurring .ics calendar file that any calendar app can import.

WHAT IT ASSUMES ABOUT YOUR SPREADSHEET
---------------------------------------
- One sheet contains the grid (default: the first sheet, or pass --sheet).
- Row 1 (or whichever --header-row you specify) has headers: a time column
  ("Time" by default) and day-of-week columns. Day columns are matched by
  name (Monday/Mon/monday/etc. all work) in ANY order and ANY subset of days
  is fine (e.g. just Mon-Fri).
- Column A (or whichever the time column is) holds actual time values for
  each row -- either real Excel time cells, or text like "6:00 AM"/"18:30".
- Each cell under a day column is the activity happening in that time slot.

WHAT IT DOES
------------
1. Reads the grid into memory (time-of-day per row, activity per day/row).
2. For each day, merges consecutive rows with the IDENTICAL activity text
   into a single block (so "Homework" at 9:00, 9:30, 10:00 becomes one
   9:00-10:30 event, not three).
3. Detects overnight activities that wrap past midnight: if a day's last
   block has the same activity as the very next day's first block (checked
   cyclically, Sunday -> Monday), it's treated as ONE continuous overnight
   event and emitted as a single daily-recurring VEVENT instead of being
   duplicated on every day. This is exactly what happens with things like
   "Sleep" that start at night and end the next morning.
4. Writes a valid RFC 5545 .ics file: every remaining block becomes a
   weekly-recurring VEVENT (RRULE FREQ=WEEKLY;BYDAY=..), anchored to a
   chosen starting Monday (default: the coming Monday), recurring
   indefinitely unless you pass --until.

USAGE
-----
    python excel_to_ics.py schedule.xlsx -o my_schedule.ics

    # Common options
    python excel_to_ics.py schedule.xlsx \\
        --sheet "Mock Schedule" \\
        --time-column "Time" \\
        --start-date 2026-07-27 \\
        --until 2026-12-19 \\
        --no-merge-overnight \\
        -o my_schedule.ics

Run `python excel_to_ics.py --help` for the full option list.

CUSTOMIZING TEXT CLEANUP
-------------------------
Real-world sheets often have typos or truncated entries (a stray trailing
"/", inconsistent capitalization, etc.). Edit the NORMALIZE dict below to
map any raw cell text to a cleaned-up label before merging/emitting -- this
is the one part of the script that's inherently specific to your data, so
it's kept as a plain, easy-to-edit dictionary near the top of the file.

REMINDERS / ALARMS
-------------------
Three ways to add a reminder (VALARM) to events, checked in this order of
precedence for each event:
  1. The REMINDERS dict below, keyed by exact activity name -- e.g.
     REMINDERS = {"Physics 2 Lab": 15} rings 15 minutes before any event
     titled "Physics 2 Lab", no matter which day it lands on.
  2. --reminder-column "Reminder": add a column to your SHEET (any header
     name you like, passed via this flag) holding a number of minutes for
     that time slot's row; it's read alongside the activity and applied to
     whichever block starts on that row. Leave cells blank for "no
     sheet-driven reminder here."
  3. --reminder-minutes N: a single global default applied to every event
     that isn't covered by #1 or #2 above.
All three can be combined -- e.g. a global 10-minute default via the CLI,
with per-class exceptions in REMINDERS for anything that needs more
lead time.
"""

import argparse
import datetime as dt
import re
import sys
import uuid
from typing import Dict, List, Optional, Tuple

import openpyxl

# ---------------------------------------------------------------------------
# EDIT ME: map raw (messy) cell text -> cleaned-up label.
# Leave empty ({}) to keep every cell exactly as written in the spreadsheet.
# ---------------------------------------------------------------------------
NORMALIZE: Dict[str, str] = {
    # "Hygine": "Hygiene",
    # "ReadReview": "Read/Review",
}

# ---------------------------------------------------------------------------
# EDIT ME: per-activity reminder overrides, in minutes before the event
# starts (0 = remind right at start time). This takes precedence over both
# --reminder-minutes and a --reminder-column value for any activity listed
# here. Leave empty ({}) to rely entirely on the CLI flags instead.
# ---------------------------------------------------------------------------
REMINDERS: Dict[str, int] = {
    # "Physics 2 Lab": 15,
    # "Physics Comp": 10,
}

WEEKDAY_NAMES = [
    "Monday", "Tuesday", "Wednesday", "Thursday",
    "Friday", "Saturday", "Sunday",
]
BYDAY_CODE = {"Monday": "MO", "Tuesday": "TU", "Wednesday": "WE",
              "Thursday": "TH", "Friday": "FR", "Saturday": "SA", "Sunday": "SU"}

# Accepted aliases per weekday, used to match header cells case-insensitively.
WEEKDAY_ALIASES = {
    "Monday": {"monday", "mon", "m"},
    "Tuesday": {"tuesday", "tue", "tues", "t"},
    "Wednesday": {"wednesday", "wed", "w"},
    "Thursday": {"thursday", "thu", "thur", "thurs", "th"},
    "Friday": {"friday", "fri", "f"},
    "Saturday": {"saturday", "sat", "sa"},
    "Sunday": {"sunday", "sun", "su"},
}


# ---------------------------------------------------------------------------
# Parsing helpers
# ---------------------------------------------------------------------------

def parse_time_cell(value) -> Optional[dt.time]:
    """Coerce a cell value into a datetime.time, or None if it isn't one."""
    if value is None:
        return None
    if isinstance(value, dt.time):
        return value
    if isinstance(value, dt.datetime):
        return value.time()
    if isinstance(value, str):
        text = value.strip()
        for fmt in ("%H:%M:%S", "%H:%M", "%I:%M %p", "%I:%M%p", "%I %p"):
            try:
                return dt.datetime.strptime(text, fmt).time()
            except ValueError:
                continue
    return None


def normalize(value) -> Optional[str]:
    """Clean a raw activity cell: strip whitespace, apply NORMALIZE map,
    treat blanks as unscheduled (None)."""
    if value is None:
        return None
    text = str(value).strip()
    if text == "":
        return None
    return NORMALIZE.get(text, text)


def find_header_row_and_columns(ws, header_row: int, time_col_name: str,
                                 reminder_col_name: Optional[str] = None):
    """Locate the time column, each weekday's column, and (optionally) a
    reminder-minutes column on the header row."""
    time_col = None
    reminder_col = None
    day_cols: Dict[str, int] = {}
    for cell in ws[header_row]:
        if cell.value is None:
            continue
        text = str(cell.value).strip().lower()
        if text == time_col_name.strip().lower():
            time_col = cell.column
            continue
        if reminder_col_name and text == reminder_col_name.strip().lower():
            reminder_col = cell.column
            continue
        for day, aliases in WEEKDAY_ALIASES.items():
            if text in aliases and day not in day_cols:
                day_cols[day] = cell.column
    if time_col is None:
        raise ValueError(
            f'Could not find a time column named "{time_col_name}" on header '
            f"row {header_row}. Pass --time-column to match your sheet, or "
            f"--header-row if headers aren't on row {header_row}."
        )
    if not day_cols:
        raise ValueError(
            f"Could not find any weekday columns on header row {header_row}. "
            "Expected headers like Monday/Tuesday/... (abbreviations OK)."
        )
    if reminder_col_name and reminder_col is None:
        raise ValueError(
            f'Could not find a reminder column named "{reminder_col_name}" on '
            f"header row {header_row}."
        )
    return time_col, day_cols, reminder_col


def parse_reminder_cell(value) -> Optional[int]:
    """Coerce a reminder-column cell into whole minutes, or None if blank/
    unparsable (meaning: no sheet-driven reminder for this row)."""
    if value is None:
        return None
    if isinstance(value, (int, float)):
        return int(value)
    text = str(value).strip()
    if text == "":
        return None
    match = re.search(r"-?\d+", text)
    return int(match.group()) if match else None


def read_grid(path: str, sheet: Optional[str], header_row: int,
              time_col_name: str, reminder_col_name: Optional[str] = None,
              data_only: bool = True):
    """Read the schedule grid. Returns (day_order, rows, reminders) where
    rows is a list of (time, {day: activity_or_None}) sorted by time, and
    reminders is a parallel list of (minutes_before_or_None) read from
    --reminder-column, or None if no such column was requested."""
    wb = openpyxl.load_workbook(path, data_only=data_only)
    ws = wb[sheet] if sheet else wb.worksheets[0]

    time_col, day_cols, reminder_col = find_header_row_and_columns(
        ws, header_row, time_col_name, reminder_col_name)
    day_order = [d for d in WEEKDAY_NAMES if d in day_cols]

    rows: List[Tuple[dt.time, Dict[str, Optional[str]]]] = []
    reminders: List[Optional[int]] = []
    for r in range(header_row + 1, ws.max_row + 1):
        t = parse_time_cell(ws.cell(row=r, column=time_col).value)
        if t is None:
            continue  # skip blank / non-time rows (spacer rows, notes, etc.)
        activities = {
            day: normalize(ws.cell(row=r, column=col).value)
            for day, col in day_cols.items()
        }
        rows.append((t, activities))
        if reminder_col is not None:
            reminders.append(parse_reminder_cell(ws.cell(row=r, column=reminder_col).value))

    order = sorted(range(len(rows)), key=lambda i: rows[i][0])
    rows = [rows[i] for i in order]
    if reminder_col is not None:
        reminders = [reminders[i] for i in order]
    else:
        reminders = None

    if not rows:
        raise ValueError("No rows with a recognizable time value were found.")
    return day_order, rows, reminders


# ---------------------------------------------------------------------------
# Block building
# ---------------------------------------------------------------------------

def slot_duration(rows: List[Tuple[dt.time, dict]], idx: int) -> dt.timedelta:
    """Duration of the slot starting at rows[idx]: the gap to the next row,
    or (if it's the last row) the same gap as the previous slot, or 30
    minutes as a last resort."""
    if idx + 1 < len(rows):
        t0, t1 = rows[idx][0], rows[idx + 1][0]
        d0 = dt.datetime.combine(dt.date(2000, 1, 1), t0)
        d1 = dt.datetime.combine(dt.date(2000, 1, 1), t1)
        if d1 <= d0:
            d1 += dt.timedelta(days=1)  # shouldn't normally happen pre-sort
        return d1 - d0
    if idx > 0:
        return slot_duration(rows, idx - 1)
    return dt.timedelta(minutes=30)


def build_day_blocks(day: str, rows: List[Tuple[dt.time, dict]],
                     reminders: Optional[List[Optional[int]]] = None):
    """Merge consecutive identical activities for one day into blocks:
    list of (start_time, end_time, activity, reminder_minutes_or_None).
    reminder_minutes comes from the --reminder-column value on the block's
    FIRST row, if such a column was supplied. Skips unscheduled (None)
    cells -- they simply produce no event."""
    blocks = []
    cur_val, cur_start_idx = None, None
    for i, (t, activities) in enumerate(rows):
        val = activities.get(day)
        if val != cur_val:
            if cur_val is not None:
                start_t = rows[cur_start_idx][0]
                end_dt = (dt.datetime.combine(dt.date(2000, 1, 1), rows[i - 1][0])
                          + slot_duration(rows, i - 1))
                rem = reminders[cur_start_idx] if reminders else None
                blocks.append((start_t, end_dt.time(), cur_val, rem))
            cur_val, cur_start_idx = val, i
    if cur_val is not None:
        start_t = rows[cur_start_idx][0]
        end_dt = (dt.datetime.combine(dt.date(2000, 1, 1), rows[-1][0])
                  + slot_duration(rows, len(rows) - 1))
        rem = reminders[cur_start_idx] if reminders else None
        blocks.append((start_t, end_dt.time(), cur_val, rem))
    return blocks


# ---------------------------------------------------------------------------
# ICS writing
# ---------------------------------------------------------------------------

def esc(text: str) -> str:
    return text.replace("\\", "\\\\").replace(";", "\\;").replace(",", "\\,")


def fmt_local(d: dt.date, t: dt.time) -> str:
    return f"{d.strftime('%Y%m%d')}T{t.strftime('%H%M%S')}"


def next_weekday_on_or_after(base: dt.date, target_weekday: int) -> dt.date:
    """target_weekday: Monday=0 ... Sunday=6"""
    days_ahead = (target_weekday - base.weekday()) % 7
    return base + dt.timedelta(days=days_ahead)


def valarm_lines(minutes: int) -> List[str]:
    """A DISPLAY alarm firing `minutes` before the event's DTSTART
    (minutes=0 means "at start time")."""
    return [
        "BEGIN:VALARM",
        "ACTION:DISPLAY",
        "DESCRIPTION:Reminder",
        f"TRIGGER:-PT{minutes}M",
        "END:VALARM",
    ]


def effective_reminder(summary: str, block_reminder: Optional[int],
                        default_reminder: Optional[int]) -> Optional[int]:
    """Precedence: REMINDERS dict (by activity name) > per-row sheet value
    (--reminder-column) > --reminder-minutes default > none."""
    if summary in REMINDERS:
        return REMINDERS[summary]
    if block_reminder is not None:
        return block_reminder
    return default_reminder


def build_calendar(day_order, rows, start_date: dt.date,
                    until: Optional[dt.date], merge_overnight: bool,
                    reminders: Optional[List[Optional[int]]] = None,
                    default_reminder: Optional[int] = None):
    day_blocks = {day: build_day_blocks(day, rows, reminders) for day in day_order}

    # Overnight-wrap detection using RAW first/last row per day (the most
    # reliable signal): if day D's last row and day D+1's first row share
    # the same activity, that activity is a single continuous event that
    # crosses midnight -- pull it out of both days and emit once, as a
    # daily-recurring event, instead of duplicating it on every weekday.
    #
    # Note this has to strip BOTH ends: the trailing block on day D (e.g.
    # "Sleep" starting at 21:30) AND the leading block on day D+1 (e.g. that
    # same "Sleep" still showing at 06:00 before the person wakes at 06:30)
    # -- otherwise the leading block survives as a spurious extra weekly
    # event on top of the daily-recurring one.
    overnight_activity = None
    if merge_overnight and len(day_order) == 7:
        last_vals = {day: rows[-1][1].get(day) for day in day_order}
        first_vals = {day: rows[0][1].get(day) for day in day_order}
        candidate = last_vals[day_order[0]]
        if (candidate is not None
                and all(last_vals[d] == candidate for d in day_order)
                and all(first_vals[d] == candidate for d in day_order)):
            overnight_activity = candidate
            last_block_start = day_blocks[day_order[0]][-1][0]  # e.g. 21:30
            # End time = where the leading block (same activity) stops,
            # i.e. its END time, not just the first row's start time.
            leading_block = day_blocks[day_order[0]][0]
            leading_block_end = (leading_block[1] if leading_block[2] == candidate
                                  else rows[0][0])
            overnight_reminder = leading_block[3] if leading_block[2] == candidate else None
            for day in day_order:
                if day_blocks[day] and day_blocks[day][-1][2] == candidate:
                    day_blocks[day] = day_blocks[day][:-1]
                if day_blocks[day] and day_blocks[day][0][2] == candidate:
                    day_blocks[day] = day_blocks[day][1:]
            start_dt = dt.datetime.combine(dt.date(2000, 1, 1), last_block_start)
            end_dt = dt.datetime.combine(dt.date(2000, 1, 1), leading_block_end)
            if end_dt <= start_dt:
                end_dt += dt.timedelta(days=1)
            overnight_duration = end_dt - start_dt

    anchor = {day: next_weekday_on_or_after(start_date, i)
              for i, day in enumerate(WEEKDAY_NAMES)}

    now_stamp = dt.datetime.now(dt.timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    until_str = f";UNTIL={until.strftime('%Y%m%d')}T235959Z" if until else ""

    lines = [
        "BEGIN:VCALENDAR",
        "VERSION:2.0",
        "PRODID:-//excel_to_ics.py//Weekly Schedule//EN",
        "CALSCALE:GREGORIAN",
        "METHOD:PUBLISH",
    ]

    event_count = 0
    for day in day_order:
        d0 = anchor[day]
        for start_t, end_t, summary, block_reminder in day_blocks[day]:
            lines += [
                "BEGIN:VEVENT",
                f"UID:{uuid.uuid4()}@excel-to-ics",
                f"DTSTAMP:{now_stamp}",
                f"DTSTART:{fmt_local(d0, start_t)}",
                f"DTEND:{fmt_local(d0, end_t)}",
                f"RRULE:FREQ=WEEKLY;BYDAY={BYDAY_CODE[day]}{until_str}",
                f"SUMMARY:{esc(summary)}",
            ]
            rem = effective_reminder(summary, block_reminder, default_reminder)
            if rem is not None:
                lines += valarm_lines(rem)
            lines.append("END:VEVENT")
            event_count += 1

    if overnight_activity is not None:
        d0 = anchor[day_order[0]]
        start_dt = dt.datetime.combine(d0, last_block_start)
        end_dt = start_dt + overnight_duration
        lines += [
            "BEGIN:VEVENT",
            f"UID:{uuid.uuid4()}@excel-to-ics",
            f"DTSTAMP:{now_stamp}",
            f"DTSTART:{start_dt.strftime('%Y%m%dT%H%M%S')}",
            f"DTEND:{end_dt.strftime('%Y%m%dT%H%M%S')}",
            f"RRULE:FREQ=DAILY{until_str}",
            f"SUMMARY:{esc(overnight_activity)}",
        ]
        rem = effective_reminder(overnight_activity, overnight_reminder, default_reminder)
        if rem is not None:
            lines += valarm_lines(rem)
        lines.append("END:VEVENT")
        event_count += 1

    lines.append("END:VCALENDAR")
    return "\r\n".join(lines) + "\r\n", event_count


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    p = argparse.ArgumentParser(
        description="Convert a weekly Excel schedule grid into a recurring .ics file.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    p.add_argument("excel_file", help="Path to the .xlsx file")
    p.add_argument("-o", "--output", default="schedule.ics", help="Output .ics path")
    p.add_argument("--sheet", default=None, help="Sheet name (default: first sheet)")
    p.add_argument("--header-row", type=int, default=1, help="Row number with headers (default 1)")
    p.add_argument("--time-column", default="Time", help='Header text of the time column (default "Time")')
    p.add_argument("--start-date", default=None,
                   help="YYYY-MM-DD for the anchor Monday-of-week. Default: the coming Monday.")
    p.add_argument("--until", default=None,
                   help="YYYY-MM-DD after which recurrences stop. Default: repeats forever.")
    p.add_argument("--no-merge-overnight", action="store_true",
                   help="Disable automatic detection of an activity that wraps past midnight "
                        "(e.g. Sleep). Without this flag, if a day's last block and the next "
                        "day's first block share the same label, they're combined into one "
                        "daily-recurring overnight event instead of duplicated per weekday.")
    p.add_argument("--reminder-minutes", type=int, default=None,
                   help="Add a reminder/alarm this many minutes before EVERY event "
                        "(0 = at start time). Overridden per-activity by the REMINDERS "
                        "dict in the script, and per-row by --reminder-column.")
    p.add_argument("--reminder-column", default=None,
                   help='Header text of an optional column (e.g. "Reminder") holding '
                        "minutes-before-event for that row's time slot, applied across "
                        "all days. Blank cells fall back to --reminder-minutes.")
    args = p.parse_args()

    start_date = (dt.datetime.strptime(args.start_date, "%Y-%m-%d").date()
                  if args.start_date else dt.date.today())
    # Always anchor to the Monday of that date's week or after today, so
    # BYDAY recurrences line up correctly regardless of what date was passed.
    until_date = (dt.datetime.strptime(args.until, "%Y-%m-%d").date()
                  if args.until else None)

    try:
        day_order, rows, reminders = read_grid(
            args.excel_file, args.sheet, args.header_row, args.time_column,
            args.reminder_column,
        )
    except Exception as e:
        print(f"Error reading {args.excel_file}: {e}", file=sys.stderr)
        sys.exit(1)

    ics_text, count = build_calendar(
        day_order, rows, start_date, until_date,
        merge_overnight=not args.no_merge_overnight,
        reminders=reminders,
        default_reminder=args.reminder_minutes,
    )

    with open(args.output, "w", newline="") as f:
        f.write(ics_text)

    print(f"Wrote {count} events to {args.output}")
    print(f"Days found: {', '.join(day_order)}")


if __name__ == "__main__":
    main()
