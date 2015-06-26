#include <iostream>

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

// int main() {
//   std::cout << "Hello World!" << "\n";
//   return 0;
// }
