# Compiler and flags
FC = gfortran
FFLAGS = -O2 -Wall -fimplicit-none

# Directories
SRC_DIR := src
OBJ_DIR := obj
MOD_DIR := mod
BIN_DIR := bin
EXEC := $(BIN_DIR)/thermal_sim

# Get all .f90 source files recursively
SRC_FILES := $(shell find $(SRC_DIR) -name '*.f90')

# Create matching object files in obj/ (preserve path)
OBJ_FILES := $(patsubst $(SRC_DIR)/%.f90,$(OBJ_DIR)/%.o,$(SRC_FILES))

# Default target
all: $(EXEC)

# Link the executable
$(EXEC): $(OBJ_FILES) | $(BIN_DIR)
	$(FC) $(FFLAGS) -J$(MOD_DIR) -o $@ $^

# Compile each object file
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90 | $(MOD_DIR)
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $< -J$(MOD_DIR) -o $@

# Ensure bin and mod directories exist
$(BIN_DIR) $(MOD_DIR):
	mkdir -p $@

# Clean build artifacts
clean:
	rm -rf $(OBJ_DIR) $(MOD_DIR) $(BIN_DIR)

.PHONY: all clean


