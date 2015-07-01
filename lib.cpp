//C Lirary for the language
#include <iostream>
#include <cmath>

//Print functions!
extern "C" void __PrintlnInt(int value) { std::cout << value << "\n"; }
extern "C" void __PrintlnFloat(float value) { std::cout << value << "\n"; }
extern "C" void __Print () { std::cout << "This is the print function from C!"; }
