FC = gfortran
FFLAGS = -O2 -fdefault-real-8 -Wall
TARGET = metriplectic_galerkin
SRC = metriplectic_galerkin.f90

all: $(TARGET)

$(TARGET): $(SRC)
	$(FC) $(FFLAGS) -o $(TARGET) $(SRC)

run: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(TARGET) *.mod *.o *.dat
