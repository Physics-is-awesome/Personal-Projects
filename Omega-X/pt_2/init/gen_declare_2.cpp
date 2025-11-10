#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
extern "C" void c_function(const char* fstring, int length) {
    std::string cpp_string(fstring, length);  // Construct C++ string from Fortran string
    std::cout << "Received string: " << cpp_string << std::endl;

    std::stringstream ss(cpp_string);
    std::string token;
    std::vector<std::string> result;
    std::vector<std::string> words; 
    while (ss >> token) {
        words.push_back(token);
    }

    std::ofstream header("generated_vars.h");
    header << "#ifndef GENERATED_VARS_H\n";
    header << "#define GENERATED_VARS_H\n\n";

    for (const auto& word : words) {
        header << "extern double " << word << ";\n";
    }

    header << "\n#endif // GENERATED_VARS_H\n";
    header.close(); 

    
    
    std::ofstream fortranFile("generated_code.F90");
    if (!fortranFile) {
        std::cerr << "Error: Could not create the file!" << std::endl;
        return;
    }

    fortranFile << "Module declare_2\n";
    fortranFile << "  IMPLICIT NONE\n";
    fortranFile << "  real(8), allocatable :: " << cpp_string << "\n";
    fortranFile << "END Module declare_2\n";
    fortranFile.close();
    std::cout << "Fortran code has been generated in 'generated_code.F90'!" << std::endl;
}
