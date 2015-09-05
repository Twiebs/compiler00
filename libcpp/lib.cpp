//C Lirary for the language
#include <iostream>
#include <cmath>

#include <stdio.h>
#include <stdint.h>

typedef uint32_t U32;
typedef uint8_t  U8;

//Print functions!
extern "C" void __PrintlnInt(int value) { std::cout << value << "\n"; }
extern "C" void __PrintlnFloat(float value) { std::cout << value << "\n"; }
extern "C" void __PrintlnStr(U8* msg) { std::cout << msg << "\n"; }
extern "C" void __PrintInt(int value) { std::cout << value; }
extern "C" void __PrintFloat(float value) { std::cout << value; }
extern "C" void __PrintStr(U8* msg) { std::cout << msg; }
//SDL Runtime lib

//===============
//   WINDOW
//===============
#include <SDL/SDL.h>
#include <GL/glew.h>
#define global_variable static
extern "C" void SetPixel(U32 x, U32 y, U8 r, U8 g, U8 b) {

}

extern "C" int CreateWindow() {
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    std::cout << "SDL Could not init!" << "\n";
    return -1;
  }
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  SDL_Surface* screen;
  screen = SDL_SetVideoMode(1280, 720, 0, SDL_OPENGL);
	if (!screen) {
		return -1;
	}
}

void NewFrame() {
  SDL_GL_SwapBuffers();
  glClearColor(1.0f, 0.1f, 0.1f, 1.0f);
  glClear(GL_COLOR_BUFFER_BIT);
}
