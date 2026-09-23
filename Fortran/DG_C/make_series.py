import re
import json
from pathlib import Path

DATA_DIR = Path(".")
VTI_DIR = Path("vti")

pattern = re.compile(r"^expl3d_sp_(\d+)_30\.dat$")

def get_time(filename):
    with open(filename) as f:
        for line in f:
            if line.startswith("# time"):
                return float(line.split("=")[1])
    raise ValueError(f"No time found in {filename}")

files = [
    f for f in DATA_DIR.glob("expl3d_sp_*.dat")
    if pattern.match(f.name)
]

files.sort(key=get_time)

series = []

for i, dat_file in enumerate(files):
    time = get_time(dat_file)

    vti_file = f"expl3d_sp_{i}_30.vti"

    if not (VTI_DIR / vti_file).exists():
        print(f"WARNING: missing {VTI_DIR / vti_file}")
        continue

    series.append({
        "name": vti_file,
        "time": time
    })

output = {
    "file-series-version": "1.0",
    "files": series
}

with open(VTI_DIR / "expl3d_sp.series", "w") as f:
    json.dump(output, f, indent=2)

print(f"Created {VTI_DIR / 'expl3d_sp.series'}")
print(f"Number of frames: {len(series)}")
