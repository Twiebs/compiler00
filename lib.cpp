#include <iostream>

//C Lirary for the language

extern "C"
int printlnInt(int value) {
    std::cout << value << "\n";
    return 0;
}

extern "C"
int println() {
  std::cout << "This is the println function from C!" << "\n";
  return 0;
}

extern "C"
int print() {
  std::cout << "This is the print function from C!";
  return 0;
}
