#include <vector>
#include <iostream>
#include <cmath>


const int NX = 100; // Number of grid points in x-direction
const int NY = 100; // Number of grid points in y-direction
double density[NX][NY], velocityX[NX][NY], velocityY[NX][NY], pressure[NX][NY], magneticFieldX[NX][NY], magneticFieldY[NX][NY];


void initialize() {
    for (int i = 0; i < NX; ++i) {
        for (int j = 0; j < NY; ++j) {
            density[i][j] = 1.0; // Example: uniform density
            velocityX[i][j] = 0.0;
            velocityY[i][j] = 0.0;
            pressure[i][j] = 1.0;
            magneticFieldX[i][j] = 0.1; // Example: weak magnetic field
            magneticFieldY[i][j] = 0.0;
        }
    }
}
