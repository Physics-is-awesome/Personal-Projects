import re
from pathlib import Path
import xml.etree.ElementTree as ET

DATA_DIR = Path(".")
VTI_DIR = Path("vti")

pattern = re.compile(r"^expl3d_sp_(\d+)_30\.dat$")


def get_time(filename):
    with open(filename) as f:
        for line in f:
            if line.startswith("# time"):
                return float(line.split("=")[1])

    raise ValueError(f"No time found in {filename}")


# Find all matching DAT files
files = [
    f for f in DATA_DIR.glob("expl3d_sp_*30.dat")
    if pattern.match(f.name)
]

# Sort by physical simulation time
files.sort(key=get_time)


# Create PVD XML structure
vtkfile = ET.Element(
    "VTKFile",
    type="Collection",
    version="0.1",
    byte_order="LittleEndian"
)

collection = ET.SubElement(vtkfile, "Collection")


for dat_file in files:

    # Extract timestep number from DAT filename
    match = pattern.match(dat_file.name)
    step = match.group(1)

    time = get_time(dat_file)

    # Corresponding VTI file
    vti_file = f"expl3d_sp_{step}_30.vti"
    vti_path = VTI_DIR / vti_file

    if not vti_path.exists():
        print(f"WARNING: missing {vti_path}")
        continue

    # Add timestep to PVD
    ET.SubElement(
        collection,
        "DataSet",
        timestep=str(time),
        group="",
        part="0",
        file=vti_file
    )


# Write PVD
tree = ET.ElementTree(vtkfile)

try:
    ET.indent(tree, space="  ")
except AttributeError:
    pass

pvd_file = VTI_DIR / "expl3d_sp.pvd"

tree.write(
    pvd_file,
    encoding="UTF-8",
    xml_declaration=True
)

print(f"Created {pvd_file}")
print(f"Number of frames: {len(collection)}")
