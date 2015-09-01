//C Lirary for the language
#include <iostream>
#include <cmath>

#include <stdio.h>
#include <stdint.h>

typedef uint8_t U8;

//Print functions!
extern "C" void __PrintlnInt(int value) { std::cout << value << "\n"; }
extern "C" void __PrintlnFloat(float value) { std::cout << value << "\n"; }
extern "C" void __PrintlnStr(U8* msg) { std::cout << msg << "\n"; }
extern "C" void __PrintInt(int value) { std::cout << value; }
extern "C" void __PrintFloat(float value) { std::cout << value; }
extern "C" void __PrintStr(U8* msg) { std::cout << msg; }
//SDL Runtime lib

#include <SDL/SDL.h>
#include <GL/glew.h>
extern "C"
int CreateWindow() {
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    std::cout << "SDL Could not init!" << "\n";
    return -1;
  }
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  SDL_Surface* screen;
  screen = SDL_SetVideoMode(1280, 720, 0, SDL_OPENGL);
	if (!screen) {
    std::cout << "SDL Could not create screen!" << "\n";
		return -1;
	}

  std::cout << "Initalized sucuessfuly" << "\n";

  static bool running = true;
  while(running) {
    SDL_Event event;
    while (SDL_PollEvent(&event)) {
      switch (event.type) {
      case SDL_KEYDOWN:
        break;
      case SDL_KEYUP:
        break;
      case SDL_QUIT:
        running = false;
        break;
      }
    }
    glClearColor(1.0f, 0.1f, 0.1f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);
    //Do main loop stuff...
    SDL_GL_SwapBuffers();
  }
  SDL_Quit();
  return 0;
}
