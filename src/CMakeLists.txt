cmake_minimum_required(VERSION 2.8.8)
project(LLVMLang)

if(NOT CMAKE_ECLIPSE_VERSION)
    add_definitions(-DENABLE_COLOR_OUTPUT)
endif()

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Uisng LLVMConfig.cmake in: ${LLVM_DIR}")

add_definitions(-std=c++14)
add_definitions(${LLVM_DEFINITIONS})

llvm_map_components_to_libnames(LLVM_LIBS support core irreader bitwriter mcjit)
message(STATUS "Found Libararies: " ${LLVM_LIBS})

if(MSVC)
  include_directories("../include")
  link_directories("../lib")
endif()


set(SRCS Main.cpp AST.cpp CodeGenerator.cpp Lexer.cpp Parser.cpp)
add_executable(LLVMLang ${SRCS})
target_link_libraries(LLVMLang ${LLVM_LIBS})
