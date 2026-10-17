# Documentation

`bsssn-kerr-axi.tex` — a complete description of the axisymmetric vacuum Kerr
BSSN code: the mathematics, the discretisation, the boundaries, the
diagnostics, and the failure modes.

## Build

```sh
cd docs
pdflatex bsssn-kerr-axi.tex   # run twice for cross-references
```

Requires `pdflatex` with `book`, `amsmath`, `longtable`, `listings`, `url`.
Produces `bsssn-kerr-axi.pdf` (44 pages).

## Contents

| Chapter | Subject |
|---|---|
| 1 | Orientation, quick start, repository map |
| 2 | The physics: ADM, BSSN, constraints, gauge |
| 3 | The discretisation: grid, Cartoon, stencils, fallbacks |
| 4 | Boundaries: excision fill, Sommerfeld, RK4, projection |
| 5 | Initial data: Kerr–Schild |
| 6 | Constraints and diagnostics, including the margin trap |
| 7 | Build and test system |
| 8 | A tour of every file |
| 9 | Findings from the failure investigation |
| 10 | Writing your own code |
| 11 | Deriving BSSN, and why each piece exists |
| 12 | Worked example: the Cartoon method, start to finish |
| 13 | Debugging numerical relativity code |
| 14 | Where the numbers come from |
| A | Traps, collected |
| B | Reproduction commands |

## Conventions

The `.pdf` is not committed. Build it locally.
