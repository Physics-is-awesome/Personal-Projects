#!/usr/bin/env bash
# Render every scene in universe_force.py, in order, and (optionally)
# concatenate them into one final video.
#
# Usage:
#   ./render_all.sh            # low quality, fast draft pass (480p15)
#   ./render_all.sh -qh        # high quality (1080p60)
#   ./render_all.sh -qk        # 4K
#   ./render_all.sh -qh concat # high quality + stitch into final_video.mp4
#
# Requires: manim, a LaTeX install (for MathTex), ffmpeg, dvisvgm.
# See README.md for details and for how to swap the font / retime scenes.

set -euo pipefail
cd "$(dirname "$0")"

QUALITY="${1:--ql}"
DO_CONCAT="${2:-}"

SCENES=(
  S00_Title
  S00b_Outline
  S01_Introduction
  S02_PlanckForce
  S03_StrikingEarth
  S04_Assumptions
  S05_Modeling
  S06_Results
  S07_Interpreting
  S08_Questions
  S09_Outro
)

for s in "${SCENES[@]}"; do
  echo "=== rendering $s ($QUALITY) ==="
  manim "$QUALITY" --disable_caching -o "${s}_out" universe_force.py "$s"
done

if [ "$DO_CONCAT" = "concat" ]; then
  # Figure out which resolution folder manim used for this quality flag.
  case "$QUALITY" in
    -ql) RES_DIR="480p15" ;;
    -qm) RES_DIR="720p30" ;;
    -qh) RES_DIR="1080p60" ;;
    -qp) RES_DIR="1440p60" ;;
    -qk) RES_DIR="2160p60" ;;
    *)   RES_DIR="480p15" ;;
  esac
  LIST_FILE="$(mktemp)"
  for s in "${SCENES[@]}"; do
    echo "file '$(pwd)/media/videos/universe_force/${RES_DIR}/${s}_out.mp4'" >> "$LIST_FILE"
  done
  ffmpeg -y -f concat -safe 0 -i "$LIST_FILE" -c copy final_video.mp4
  rm -f "$LIST_FILE"
  echo "=== wrote final_video.mp4 ==="
fi
