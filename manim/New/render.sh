#!/usr/bin/env bash
# Render every scene and stitch them in narration order.
#   ./render_all.sh          -> 1080p60-ish "high" quality (-qh)
#   Q=l ./render_all.sh      -> fast 480p preview
# Put kerr_bssn.srt next to this file (or set KERR_SRT=/path/to/kerr_bssn.srt).
set -euo pipefail
cd "$(dirname "$0")"
Q="${Q:-h}"
SCENES=(S0 S1 S2 S2_1 S2_2 S2_3 S3 S4 S4_1 S5 S6 S6_1 S7 S8 S9 S9_1 S9_2 S10 S10_1 S11 S12)
python3 -m manim -q"$Q" --disable_caching kerr_bssn.py "${SCENES[@]}"
case "$Q" in l) RES=480p15;; m) RES=720p30;; h) RES=1080p60;; k) RES=2160p60;; *) RES=1080p60;; esac
DIR="media/videos/kerr_bssn_scenes/$RES"
: > concat.txt
for s in "${SCENES[@]}"; do echo "file '$DIR/$s.mp4'" >> concat.txt; done
ffmpeg -y -f concat -safe 0 -i concat.txt -c copy kerr_bssn_silent.mp4
echo "Silent video: kerr_bssn_silent.mp4"
echo "Add narration with:  ffmpeg -i kerr_bssn_silent.mp4 -i narration.mp3 -c:v copy -c:a aac -shortest kerr_bssn_final.mp4"
