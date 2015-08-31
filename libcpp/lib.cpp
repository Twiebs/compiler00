//C Lirary for the language
#include <iostream>
#include <cmath>

#include <stdio.h>
//Print functions!
extern "C" void __PrintlnInt(int value) { std::cout << value << "\n"; }
extern "C" void __PrintlnFloat(float value) { std::cout << value << "\n"; }
extern "C" void __PrintlnStr(const char* str) { std::cout << str << "\n"; }
extern "C" void __Print () { std::cout << "This is the print function from C!"; }
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
