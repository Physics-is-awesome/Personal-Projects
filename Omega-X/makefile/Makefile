#===============================
# Metriplectic Project Makefile
#===============================

# Compiler
FC = gfortran
FFLAGS = -O2 -Wall -std=f2008
LDFLAGS = -O2 -Wall -lc  

# Output
EXEC = run_metriplectic
OBJ_DIR = obj
MOD_DIR = mod

# Create directories
$(shell mkdir -p $(OBJ_DIR) $(MOD_DIR))

# Source files
SRC_DIR = ../src/Core
DRIVER_DIR = ../src/driver

# Object files
OBJ_MESH = $(OBJ_DIR)/mesh.o
OBJ_STATES = $(OBJ_DIR)/states.o
OBJ_FUNCTIONALS = $(OBJ_DIR)/functionals.o
OBJ_ASSEMBLY = $(OBJ_DIR)/assembly.o
OBJ_BRACKETS = $(OBJ_DIR)/brackets.o
OBJ_IO = $(OBJ_DIR)/io.o
OBJ_TIME = $(OBJ_DIR)/time_integrator.o
OBJ_DRIVER = $(OBJ_DIR)/1D_Thermal_driver.o

OBJS = $(OBJ_MESH) $(OBJ_STATES) $(OBJ_FUNCTIONALS) $(OBJ_ASSEMBLY) \
       $(OBJ_BRACKETS) $(OBJ_IO) $(OBJ_TIME) $(OBJ_DRIVER)

# Default target
all: $(EXEC)

# Link
$(EXEC): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS)
$(EXEC): $(OBJS)
	$(FC) $(LDFLAGS) -o $@ $(OBJS)

# order of compiling 

obj/states.o: obj/mesh.o
obj/functionals.o: obj/states.o
obj/brackets.o: obj/functionals.o obj/states.o
obj/time_integrator.o: obj/brackets.o obj/states.o
obj/io.o: obj/states.o obj/functionals.o
obj/1D_Thermal_driver.o: obj/time_integrator.o obj/io.o

# Compile rules with module dependencies
$(OBJ_MESH): $(SRC_DIR)/mesh.f90
	$(FC) $(FFLAGS) -c $< -J$(MOD_DIR) -I$(MOD_DIR) -o $@

$(OBJ_STATES): $(SRC_DIR)/states.f90 $(OBJ_MESH)
	$(FC) $(FFLAGS) -c $< -J$(MOD_DIR) -I$(MOD_DIR) -o $@

$(OBJ_FUNCTIONALS): $(SRC_DIR)/functionals.f90 $(OBJ_STATES)
	$(FC) $(FFLAGS) -c $< -J$(MOD_DIR) -I$(MOD_DIR) -o $@

$(OBJ_ASSEMBLY): $(SRC_DIR)/assembly.f90 $(OBJ_MESH)
	$(FC) $(FFLAGS) -c $< -J$(MOD_DIR) -I$(MOD_DIR) -o $@

$(OBJ_BRACKETS): $(SRC_DIR)/brackets.f90 $(OBJ_FUNCTIONALS) $(OBJ_ASSEMBLY) $(OBJ_STATES)
	$(FC) $(FFLAGS) -c $< -J$(MOD_DIR) -I$(MOD_DIR) -o $@

$(OBJ_IO): $(SRC_DIR)/io.f90 $(OBJ_FUNCTIONALS) $(OBJ_STATES)
	$(FC) $(FFLAGS) -c $< -J$(MOD_DIR) -I$(MOD_DIR) -o $@

$(OBJ_TIME): $(SRC_DIR)/time_integrator.f90 $(OBJ_BRACKETS) $(OBJ_STATES)
	$(FC) $(FFLAGS) -c $< -J$(MOD_DIR) -I$(MOD_DIR) -o $@

$(OBJ_DRIVER): $(DRIVER_DIR)/1D_Thermal_driver.f90 $(OBJ_TIME) $(OBJ_IO)
	$(FC) $(FFLAGS) -c $< -J$(MOD_DIR) -I$(MOD_DIR) -o $@

# Clean
clean:
	rm -rf $(OBJ_DIR)/*.o $(MOD_DIR)/*.mod $(EXEC)
print-src:
	@echo "SRC = $(SRC)"
	@echo "OBJS = $(OBJS)"

