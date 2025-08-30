# Makefile - build both Galerkin and Matrix-assembled metriplectic programs
FC ?= gfortran
FFLAGS ?= -O2 -fdefault-real-8 -Wall
LDFLAGS ?=
# Optionally link LAPACK/BLAS if you need eigenvalue/linear-solvers:
USE_LAPACK ?= 0
ifeq ($(USE_LAPACK),1)
  LDFLAGS += -llapack -lblas
endif

GAL_SRCS = metriplectic_galerkin.f90
MAT_SRCS = metriplectic_matrix_fixed.f90

GAL_BIN = metriplectic_galerkin
MAT_BIN = metriplectic_matrix_fixed

.PHONY: all galerkin metriplectic run_gal run_mat run clean

all: $(GAL_BIN) $(MAT_BIN)

# build Galerkin program
$(GAL_BIN): $(GAL_SRCS)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

# build Matrix-assembled program
$(MAT_BIN): $(MAT_SRCS)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

# convenience targets to build only one of them
galerkin: $(GAL_BIN)
metriplectic: $(MAT_BIN)

# run targets
run_gal: $(GAL_BIN)
	./$(GAL_BIN)

run_mat: $(MAT_BIN)
	./$(MAT_BIN)

# run both in sequence
run: run_gal run_mat

clean:
	rm -f $(GAL_BIN) $(MAT_BIN) *.o *.mod *.dat *.png
