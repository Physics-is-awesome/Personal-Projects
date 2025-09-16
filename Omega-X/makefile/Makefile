#==================================================
# Makefile for modular metriplectic project layout
#==================================================

# Compiler and flags
FC = gfortran
FFLAGS = -O2 -Wall -std=f2008

# Output executable name
EXEC = run_metriplectic

# Directories
SRC_DIR = src
OBJ_DIR = obj
MOD_DIR = mod

# Create object and module directories if not present
$(shell mkdir -p $(OBJ_DIR) $(MOD_DIR))

# All Fortran source files
SRC = $(wildcard $(SRC_DIR)/*/*.f90)

# Object files with path
OBJS = $(patsubst $(SRC_DIR)/%.f90, $(OBJ_DIR)/%.o, $(subst /,.,$(SRC)))

# Build target
all: $(EXEC)

# Linking rule
$(EXEC): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS)

# Compilation rule
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -J$(MOD_DIR) -I$(MOD_DIR) -c $< -o $@

# Clean target
clean:
	rm -rf $(OBJ_DIR) $(MOD_DIR) $(EXEC)

# Run target
run: $(EXEC)
	./$(EXEC)
