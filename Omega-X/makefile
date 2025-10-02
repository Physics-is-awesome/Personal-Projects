# Directories
SRC_DIR := src
OBJ_DIR := obj
MOD_DIR := mod
BIN_DIR := bin
EXEC := $(BIN_DIR)/thermal_sim

FC := gfortran
FFLAGS := -O2 -Wall -fimplicit-none

# Make directories
$(shell mkdir -p $(OBJ_DIR) $(MOD_DIR) $(BIN_DIR))

# Compile modules in correct order
OBJ_CORE := \
    $(OBJ_DIR)/Core/mesh/mesh_1d.o \
    $(OBJ_DIR)/Core/initialize/states_1d.o \
	$(OBJ_DIR)/Core/functionals/projection_matrix_1d.o \
    $(OBJ_DIR)/Core/functionals/mass_1d.o \
	$(OBJ_DIR)/Core/functionals/velocity_1d.o \
    $(OBJ_DIR)/Core/functionals/momentum_1d.o \
    $(OBJ_DIR)/Core/functionals/entropy_1d.o \
	$(OBJ_DIR)/Core/EOS/EOS_1d_thermal.o \
    $(OBJ_DIR)/Core/time_integrator/time_integrator.o \
    $(OBJ_DIR)/Core/EOS/EOS_1d_thermal.o \
    $(OBJ_DIR)/Core/io/io_1d.o

DRIVER_OBJ := $(OBJ_DIR)/driver/1D_Thermal_driver.o

all: $(EXEC)

# Link executable
$(EXEC): $(OBJ_CORE) $(DRIVER_OBJ)
	$(FC) $(FFLAGS) -J$(MOD_DIR) -o $@ $^

# Compile each module individually
$(OBJ_DIR)/Core/mesh/mesh_1d.o:
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $(SRC_DIR)/Core/mesh/mesh_1d.f90 -J$(MOD_DIR) -o $@

$(OBJ_DIR)/Core/initialize/states_1d.o: $(OBJ_DIR)/Core/mesh/mesh_1d.o
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $(SRC_DIR)/Core/initialize/states_1d.f90 -J$(MOD_DIR) -o $@

$(OBJ_DIR)/Core/functionals/mass_1d.o: $(OBJ_DIR)/Core/initialize/states_1d.o
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $(SRC_DIR)/Core/functionals/mass_1d.f90 -J$(MOD_DIR) -o $@


$(OBJ_DIR)/Core/functionals/projection_matrix_1d.o: $(OBJ_DIR)/Core/initialize/states_1d.o
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $(SRC_DIR)/Core/functionals/projection_matrix_1d.f90 -J$(MOD_DIR) -o $@

$(OBJ_DIR)/Core/functionals/momentum_1d.o: $(OBJ_DIR)/Core/initialize/states_1d.o
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $(SRC_DIR)/Core/functionals/momentum_1d.f90 -J$(MOD_DIR) -o $@

$(OBJ_DIR)/Core/functionals/velocity_1d.o: $(OBJ_DIR)/Core/initialize/states_1d.o
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $(SRC_DIR)/Core/functionals/velocity_1d.f90 -J$(MOD_DIR) -o $@

$(OBJ_DIR)/Core/functionals/entropy_1d.o: $(OBJ_DIR)/Core/initialize/states_1d.o
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $(SRC_DIR)/Core/functionals/entropy_1d.f90 -J$(MOD_DIR) -o $@
$(OBJ_DIR)/Core/EOS/EOS_1d_thermal.o: $(OBJ_DIR)/Core/initialize/states_1d.o
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $(SRC_DIR)/Core/EOS/EOS_1d_thermal.f90 -J$(MOD_DIR) -o $@

$(OBJ_DIR)/Core/time_integrator/time_integrator.o: $(OBJ_DIR)/Core/functionals/mass_1d.o $(OBJ_DIR)/Core/functionals/momentum_1d.o $(OBJ_DIR)/Core/functionals/entropy_1d.o $(OBJ_DIR)/Core/functionals/projection_matrix_1d.o
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $(SRC_DIR)/Core/time_integrator/time_integrator.f90 -J$(MOD_DIR) -o $@





$(OBJ_DIR)/Core/io/io_1d.o: $(OBJ_DIR)/Core/initialize/states_1d.o
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $(SRC_DIR)/Core/io/io_1d.f90 -J$(MOD_DIR) -o $@

# Driver depends on everything
$(OBJ_DIR)/driver/1D_Thermal_driver.o: $(OBJ_CORE)
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $(SRC_DIR)/driver/1D_Thermal_driver.f90 -J$(MOD_DIR) -o $@

clean:
	rm -rf $(OBJ_DIR) $(MOD_DIR) $(BIN_DIR)

.PHONY: all clean
