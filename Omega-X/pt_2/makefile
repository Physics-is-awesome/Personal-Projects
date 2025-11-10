# Directories
SRC_DIR := src
OBJ_DIR := obj
MOD_DIR := mod
BIN_DIR := bin
EXEC := $(BIN_DIR)/run

.PHONY: a

FC := gfortran
CXX = g++

FFLAGS := -O2 -Wall -fimplicit-none
CXXFLAGS = -O2 -Wall
# Source files and objects
FORTRAN_SRC = init/interface.F90 init/read.F90 init/declare_vars.F90 init/states.F90 driver/driver_init.F90 # projection/quad.F90 projection/basis.F90 evolution/mass_matrix.F90  driver/driver_evol.F90 driver/driver_main.F90
CPP_SRC = init/gen_declare_2.cpp

FORTRAN_OBJ = $(FORTRAN_SRC:.F90=.o) 

CPP_OBJ = $(CPP_SRC:.cpp=.o)

OBJ = $(FORTRAN_OBJ) $(CPP_OBJ)

# Executable name
EXE = program

# Default target
all: $(EXE)

# Link step
$(EXE): $(OBJ)
	$(FC) $(OBJ) -lstdc++ -o $(EXE)

# Compile step for .f90(not using currently)
#%.o: %.f90
	#$(FC) $(FFLAGS) -c $< -o $@

# Compile step for .F90 (preprocessed)
%.o: %.F90
	$(FC) $(FFLAGS) -c $< -o $@
# compile steps for C++
%.o: %.CPP
	$(CXX) $(CXXFLAGS) -c $< -o $@
# Run

run: $(EXE)
	./$(EXE)

# All: clean, run, run python(not yet), clean temporary files(not yet)
all: $(EXE)
	./$(EXE)
	rm -f *.mod *.o */*.o */*/*.o */*.mod
	#add pythin run ($(PYTHON) evolution/generation.py)
	# delete temp files (rm -f evolution/fortran_modules/*.f90
# Clean up
clean:
	rm -f *.mod *.o */*.o */*/*.o */*.mod
