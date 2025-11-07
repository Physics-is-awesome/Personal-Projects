#include <iostream>
#include <fstream>
#include <string>

extern "C" void c_function(const char* fstring, int length) {
    std::string cpp_string(fstring, length);  // Construct C++ string from Fortran string
    std::cout << "Received string: " << cpp_string << std::endl;

    std::ofstream fortranFile("generated_code.F90");
    if (!fortranFile) {
        std::cerr << "Error: Could not create the file!" << std::endl;
        return;
    }

    fortranFile << "Module declare_2\n";
    fortranFile << "  IMPLICIT NONE\n";
    fortranFile << "  real(8) :: " << cpp_string << "\n";
    fortranFile << "END Module declare_2\n";
    fortranFile.close();

    std::cout << "Fortran code has been generated in 'generated_code.F90'!" << std::endl;
}
int main() {
    const char* fortran_style_string = "velocity";
    int length = 8;  // Length of the string

    c_function(fortran_style_string, length);

    return 0;
}
