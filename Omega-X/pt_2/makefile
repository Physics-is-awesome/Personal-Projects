# Directories
SRC_DIR := src
OBJ_DIR := obj
MOD_DIR := mod
BIN_DIR := bin
EXEC := $(BIN_DIR)/run

FC := gfortran
FFLAGS := -O2 -Wall -fimplicit-none

# Source files and objects
SRC = init/read.f90 init/states.F90 driver/driver_init.F90 driver/driver_main.F90
OBJ = $(SRC:.f90=.o)
OBJ := $(OBJ:.F90=.o)

# Executable name
EXE = states

# Default target
all: $(EXE)

# Link step
$(EXE): $(OBJ)
    $(FC) $(OBJ) -o $(EXE)

# Compile step for .f90
%.o: %.f90
    $(FC) $(FFLAGS) -c $< -o $@

# Compile step for .F90 (preprocessed)
%.o: %.F90
    $(FC) $(FFLAGS) -c $< -o $@

# Clean up
clean:
    rm -f $(OBJ) $(EXE) *.mod
