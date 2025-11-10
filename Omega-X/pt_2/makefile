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
SHARED_SRC = init/interface.F90 init/read.F90 
FORTRAN_SRC_init = driver/driver_init.F90 
FORTRAN_SRC_MAIN = init/declare_vars.F90 init/states.F90 generated_code.F90 projection/quad.F90 projection/basis.F90 evolution/mass_matrix.F90  driver/driver_evol.F90 driver/driver_main.F90
CPP_SRC = init/gen_declare_2.cpp

FORTRAN_OBJ_init = $(FORTRAN_SRC_init:.F90=.o) 
FORTRAN_OBJ_shared = $(SHARED_SRC:.F90=.o) 
CPP_OBJ = $(CPP_SRC:.cpp=.o)
OBJ_init = $(FORTRAN_OBJ_shared) $(FORTRAN_OBJ_init) $(CPP_OBJ) 

OBJ_MAIN = $(FORTRAN_SRC_MAIN:.F90=.o) $(FORTRAN_OBJ_shared) $(CPP_OBJ)

# Executable name
init = program_init
EXE = program_main
# Default target
all: $(EXE)

# Link step
$(EXE): $(OBJ_MAIN) 
	$(FC) $(OBJ_MAIN) -lstdc++ -o $(EXE)
$(OBJ_MAIN): run_init
run_init: $(init)
	./$(init)
	touch run_init
	@echo "CODE BLUE"
$(init): $(OBJ_init)
	$(FC) $(OBJ_init) -lstdc++ -o $(init)
	@echo "Hi, this works!!!!!!!!!!!!!!!!"


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
