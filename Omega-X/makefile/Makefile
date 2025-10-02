# Compiler and flags
FC = gfortran
FFLAGS = -O2 -Wall -fimplicit-none
MODULE_DIR = mod
OBJ_DIR = obj
SRC_DIR = src
BIN_DIR = bin
EXEC = $(BIN_DIR)/thermal_sim

# Source files
CORE_SRCS = $(wildcard $(SRC_DIR)/Core/**/*.f90)
DRIVER_SRCS = $(SRC_DIR)/driver/1D_Thermal_driver.f90
ALL_SRCS = $(CORE_SRCS) $(DRIVER_SRCS)

# Object files: preserve folder hierarchy
OBJ_FILES = $(patsubst $(SRC_DIR)/%.f90,$(OBJ_DIR)/%.o,$(ALL_SRCS))

# Default target
all: $(EXEC)

# Build executable
$(EXEC): $(OBJ_FILES) | $(BIN_DIR)
	$(FC) $(FFLAGS) -J$(MODULE_DIR) -o $@ $^

# Build object files
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90 | $(OBJ_DIR) $(MODULE_DIR)
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $< -J$(MODULE_DIR) -o $@

# Create directories if they don't exist
$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

$(MODULE_DIR):
	mkdir -p $(MODULE_DIR)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

# Clean
clean:
	rm -rf $(OBJ_DIR) $(MODULE_DIR) $(BIN_DIR)

.PHONY: all clean

	rm -rf $(OBJ_DIR) $(MODULE_DIR) $(BIN_DIR)

.PHONY: all clean

