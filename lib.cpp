#include <iostream>

//C Lirary for the language

extern "C"
int PrintlnInt(int value) {
    std::cout << value << "\n";
    return 0;
}

extern "C"
int Println () {
  std::cout << "This is the println function from C!" << "\n";
  return 0;
}

extern "C"
int Print () {
  std::cout << "This is the print function from C!";
  return 0;
}
