#===============================
# Metriplectic Project Makefile
#===============================

# Compiler and flags
FC = gfortran
FFLAGS = -O2 -Wall -std=f2008

# Output and paths
EXEC = run_metriplectic
SRC_DIR = ../src
OBJ_DIR = obj
MOD_DIR = mod

# Create needed folders
$(shell mkdir -p $(OBJ_DIR) $(MOD_DIR))

# Find all .f90 files in Core/ and driver/
SRC = $(wildcard $(SRC_DIR)/Core/*.f90) $(wildcard $(SRC_DIR)/driver/*.f90)

# Convert source filenames to object file names
OBJS = $(patsubst $(SRC_DIR)/%.f90, $(OBJ_DIR)/%.o, $(subst /,.,$(SRC)))

# Default target
all: $(EXEC)

# Link executable
$(EXEC): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS)

# Compile rule
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $< -J$(MOD_DIR) -I$(MOD_DIR) -o $@

# Clean build artifacts
clean:
	rm -f $(OBJ_DIR)/*.o $(MOD_DIR)/*.mod $(EXEC)

# Run program
run: $(EXEC)
	./$(EXEC)

# Print found source files (for debugging)
print-src:
	@echo "Found sources:"
	@echo $(SRC)

