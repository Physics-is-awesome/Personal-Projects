# ========================
# Compiler and flags
# ========================
FC := gfortran
FFLAGS := -O2 -Wall -fimplicit-none

# ========================
# Directories
# ========================
SRC_DIR := src
OBJ_DIR := obj
MOD_DIR := mod
BIN_DIR := bin
EXEC := $(BIN_DIR)/thermal_sim

# ========================
# Source files
# ========================
# List all modules explicitly or use a wildcard per folder
MODULE_SRCS := \
    $(SRC_DIR)/Core/EOS/EOS_1d_thermal.f90 \
    $(SRC_DIR)/Core/functionals/entropy_1d.f90 \
    $(SRC_DIR)/Core/functionals/mass_1d.f90 \
    $(SRC_DIR)/Core/functionals/momentum_1d.f90 \
    $(SRC_DIR)/Core/functionals/projection_matrix_1d.f90 \
    $(SRC_DIR)/Core/initialize/states_1d.f90 \
    $(SRC_DIR)/Core/io/io_1d.f90 \
    $(SRC_DIR)/Core/mesh/mesh_1d.f90 \
    $(SRC_DIR)/Core/time_integrator/time_integrator.f90

DRIVER_SRC := $(SRC_DIR)/driver/1D_Thermal_driver.f90

# Object files
MODULE_OBJS := $(patsubst $(SRC_DIR)/%.f90,$(OBJ_DIR)/%.o,$(MODULE_SRCS))
DRIVER_OBJ := $(patsubst $(SRC_DIR)/%.f90,$(OBJ_DIR)/%.o,$(DRIVER_SRC))

# ========================
# Default target
# ========================
all: $(EXEC)

# ========================
# Build executable
# ========================
$(EXEC): $(MODULE_OBJS) $(DRIVER_OBJ) | $(BIN_DIR)
	$(FC) $(FFLAGS) -J$(MOD_DIR) -o $@ $^

# ========================
# Compile modules and driver
# ========================
# General rule: compile any .f90 source to corresponding obj/ folder
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90 | $(MOD_DIR)
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $< -J$(MOD_DIR) -o $@

# ========================
# Directories
# ========================
$(BIN_DIR) $(MOD_DIR):
	mkdir -p $@

# ========================
# Clean
# ========================
clean:
	rm -rf $(OBJ_DIR) $(MOD_DIR) $(BIN_DIR)

.PHONY: all clean

