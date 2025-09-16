#===============================
# Metriplectic Project Makefile
#===============================

# Compiler and flags
FC = gfortran
FFLAGS = -O2 -Wall -std=f2008

# Executable name
EXEC = run_metriplectic

# Directories
SRC_DIR := ../src
OBJ_DIR := obj
MOD_DIR := mod

# Create build directories
$(shell mkdir -p $(OBJ_DIR) $(MOD_DIR))

# Source files from Core/ and driver/
SRC := $(wildcard $(SRC_DIR)/Core/*.f90) $(wildcard $(SRC_DIR)/driver/*.f90)

# Convert paths like src/Core/mesh.f90 → obj/mesh.o
OBJS := $(patsubst $(SRC_DIR)/%, $(OBJ_DIR)/%, $(SRC:.f90=.o))

# Default target
all: $(EXEC)

# Link object files into final executable
$(EXEC): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS)

# Compile .f90 files into .o files with module output in mod/
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $< -J$(MOD_DIR) -I$(MOD_DIR) -o $@

# Clean up build artifacts
clean:
	rm -rf $(OBJ_DIR)/*.o $(MOD_DIR)/*.mod $(EXEC)

# Run program
run: $(EXEC)
	./$(EXEC)

# Print found sources
print-src:
	@echo "SRC = $(SRC)"
	@echo "OBJS = $(OBJS)"

