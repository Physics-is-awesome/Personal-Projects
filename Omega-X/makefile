# Compiler and flags
FC := gfortran
FFLAGS := -O2 -Wall -fimplicit-none

SRC_DIR := src
OBJ_DIR := obj
MOD_DIR := mod
BIN_DIR := bin
EXEC := $(BIN_DIR)/thermal_sim

# Explicit module order
MODULES := \
    Core/mesh/mesh_1d.f90 \
    Core/initialize/states_1d.f90 \
    Core/functionals/mass_1d.f90 \
    Core/functionals/momentum_1d.f90 \
    Core/functionals/entropy_1d.f90 \
    Core/functionals/projection_matrix_1d.f90 \
    Core/time_integrator/time_integrator.f90 \
    Core/brackets.f90 \
    Core/EOS/EOS_1d_thermal.f90 \
    Core/io/io_1d.f90

MODULE_OBJS := $(patsubst %,$(OBJ_DIR)/%,$(MODULES:.f90=.o))

DRIVER := driver/1D_Thermal_driver.f90
DRIVER_OBJ := $(patsubst %,$(OBJ_DIR)/%,$(DRIVER:.f90=.o))

# Default target
all: $(EXEC)

# Build executable
$(EXEC): $(MODULE_OBJS) $(DRIVER_OBJ) | $(BIN_DIR)
	$(FC) $(FFLAGS) -J$(MOD_DIR) -o $@ $^

# Compile modules and driver
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90 | $(MOD_DIR)
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $< -J$(MOD_DIR) -o $@

# Create directories
$(BIN_DIR) $(MOD_DIR):
	mkdir -p $@

# Clean
clean:
	rm -rf $(OBJ_DIR) $(MOD_DIR) $(BIN_DIR)

.PHONY: all clean


	rm -rf $(OBJ_DIR) $(MOD_DIR) $(BIN_DIR)

.PHONY: all clean

