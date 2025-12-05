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
Tui = tui.cpp
#FORTRAN_SRC_pre = pre-init/interface.F90 pre-init/preread.F90 driver/predriver.F90
FORTRAN_SRC_MAIN = init/read.F90 init/types.F90 init/states.F90 generated_code.F90 init/calculated_vals.F90 projection/quad.F90 projection/basis.F90 evolution/mass_matrix.F90 evolution/fortran_modules/m_h_module.F90 driver/driver_init.F90 driver/driver_evol.F90 driver/driver_main.F90
#CPP_SRC = pre-init/gen_declare_2.cpp

#FORTRAN_OBJ_pre = $(FORTRAN_SRC_pre:.F90=.o) 
#CPP_OBJ = $(CPP_SRC:.cpp=.o)
#OBJ_pre = $(FORTRAN_OBJ_pre) $(CPP_OBJ) 
TUI_OBJ = $(Tui:.cpp=.o)
OBJ_MAIN = $(FORTRAN_SRC_MAIN:.F90=.o) 

# Executable name
#pre = program_init
EXE = program_main
# Default target
all: $(EXE)

# Link step
$(EXE): $(OBJ_MAIN) 
	$(FC) $(OBJ_MAIN) -lstdc++ -o $(EXE)
#$(OBJ_MAIN): run_init
#run_init: $(pre)
#	./$(pre)
#	touch run_init

#$(pre): $(OBJ_pre)
#	$(FC) $(OBJ_pre) -lstdc++ -o $(pre)



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
# tui
tui: 
	g++ -o tui tui.cpp -lncurses
	./tui
# All: clean, run, run python(not yet), clean temporary files(not yet)
all: $(EXE)
	./$(EXE)
	rm -f *.mod *.o */*.o */*/*.o */*.mod $(EXE) $(init) $(pre) generated*
	#add pythin run ($(PYTHON) evolution/generation.py)
	# delete temp files (rm -f evolution/fortran_modules/*.f90
# Clean up
clean:
	rm -f *.mod *.o */*.o */*/*.o */*.mod generated* $(EXE) $(init) $(pre)
