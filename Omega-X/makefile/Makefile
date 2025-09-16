#============================================
# Makefile for Metriplectic 1D Thermal-Fluid
#============================================

# Fortran compiler and flags
FC = gfortran
FFLAGS = -O2 -Wall -Wno-unused-variable -std=f2008

# Executable name
EXEC = run_metriplectic

# Object files
OBJS = \
    mesh.o \
    state.o \
    functionals.o \
    brackets.o \
    assembly.o \
    io.o \
    time_integrator.o \
    main_driver.o

# Default target
all: $(EXEC)

# Link step
$(EXEC): $(OBJS)
	$(FC) $(FFLAGS) -o $(EXEC) $(OBJS)

# Compile each module
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

# Clean up
clean:
	rm -f *.o *.mod $(EXEC)

# Run the executable
run: $(EXEC)
	./$(EXEC)
