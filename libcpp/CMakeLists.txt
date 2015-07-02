cmake_minimum_required(VERSION 2.8.8)

set(SRCS lib.cpp)

find_package(SDL REQUIRED)
if(NOT SDL_FOUND)
	message("Could not find SDL")
endif()

add_library(bang STATIC ${SRCS} )
target_link_libraries(bang ${SDL_LIBRARY})
