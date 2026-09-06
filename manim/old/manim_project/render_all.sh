#!/bin/bash
# Render every scene in rmhd_manim.py and concatenate into one final video.
#
# Usage:
#   bash render_all.sh l     # low quality, fast preview (480p15)
#   bash render_all.sh m     # medium (720p30)
#   bash render_all.sh h     # high, final (1080p60)
#   bash render_all.sh k     # 4K (2160p60)
#
# Requires: manim, ffmpeg (both must be on PATH).

set -e

QUALITY_FLAG="${1:-l}"

case "$QUALITY_FLAG" in
  l) QUALITY_DIR="480p15" ;;
  m) QUALITY_DIR="720p30" ;;
  h) QUALITY_DIR="1080p60" ;;
  k) QUALITY_DIR="2160p60" ;;
  *) echo "Unknown quality flag '$QUALITY_FLAG' (use l/m/h/k)"; exit 1 ;;
esac

SCENES=(
  OpeningScene
  IntroductionScene
  VariationalIntegratorsScene
  RMHDScene
  FormalLagrangianScene
  ConservationLawsScene
  ElectronInertiaScene
  SymmetrisationScene
  DiscreteActionPrincipleScene
  DiscreteConservationLawsScene
  NumericalMethodsScene
  ValidationScene
  RandomTestScene
)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

for scene in "${SCENES[@]}"; do
  echo "=== Rendering $scene ==="
  manim -q"$QUALITY_FLAG" rmhd_manim.py "$scene"
done

MEDIA_DIR="media/videos/rmhd_manim/${QUALITY_DIR}"
LIST_FILE="concat_list.txt"
rm -f "$LIST_FILE"

for scene in "${SCENES[@]}"; do
  mp4="${MEDIA_DIR}/${scene}.mp4"
  if [ ! -f "$mp4" ]; then
    echo "WARNING: expected file not found: $mp4"
    echo "         (check the QUALITY_DIR name against your installed Manim's"
    echo "          actual output folder under media/videos/rmhd_manim/)"
    exit 1
  fi
  echo "file '$mp4'" >> "$LIST_FILE"
done

echo "=== Concatenating into full_video_${QUALITY_FLAG}.mp4 ==="
ffmpeg -y -f concat -safe 0 -i "$LIST_FILE" -c copy "full_video_${QUALITY_FLAG}.mp4"

echo "Done: full_video_${QUALITY_FLAG}.mp4"
