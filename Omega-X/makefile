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
# Module source files in correct order
# ========================
MODULE_SRCS := \
    $(SRC_DIR)/Core/mesh/mesh_1d.f90 \
    $(SRC_DIR)/Core/initialize/states_1d.f90 \
    $(SRC_DIR)/Core/functionals/mass_1d.f90 \
    $(SRC_DIR)/Core/functionals/momentum_1d.f90 \
    $(SRC_DIR)/Core/functionals/entropy_1d.f90 \
    $(SRC_DIR)/Core/functionals/projection_matrix_1d.f90 \
    $(SRC_DIR)/Core/time_integrator/time_integrator.f90 \
    $(SRC_DIR)/Core/brackets.f90 \
    $(SRC_DIR)/Core/EOS/EOS_1d_thermal.f90 \
    $(SRC_DIR)/Core/io/io_1d.f90

# Map source files to object files
MODULE_OBJS := $(patsubst $(SRC_DIR)/%,$(OBJ_DIR)/%,$(MODULE_SRCS:.f90=.o))

# Driver
DRIVER_SRC := $(SRC_DIR)/driver/1D_Thermal_driver.f90
DRIVER_OBJ := $(patsubst $(SRC_DIR)/%,$(OBJ_DIR)/%,$(DRIVER_SRC:.f90=.o))

# ========================
# Default target
# ========================
all: $(EXEC)

# ========================
# Link executable
# ========================
$(EXEC): $(MODULE_OBJS) $(DRIVER_OBJ) | $(BIN_DIR)
	$(FC) $(FFLAGS) -J$(MOD_DIR) -o $@ $^

# ========================
# Compile any module or driver
# ========================
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90 | $(MOD_DIR)
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $< -J$(MOD_DIR) -o $@

# ========================
# Ensure directories exist
# ========================
$(BIN_DIR) $(MOD_DIR):
	mkdir -p $@

# ========================
# Clean build artifacts
# ========================
clean:
	rm -rf $(OBJ_DIR) $(MOD_DIR) $(BIN_DIR)

.PHONY: all clean

	rm -rf $(OBJ_DIR) $(MOD_DIR) $(BIN_DIR)

.PHONY: all clean

