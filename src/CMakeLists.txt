cmake_minimum_required(VERSION 2.8.8)
project(LLVMLang)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Uisng LLVMConfig.cmake in: ${LLVM_DIR}")


set(SRCS Main.cpp CodeGenerator.cpp Lexer.cpp Parser.cpp)

include_directories("../include")
link_directories("../lib")

add_executable(LLVMLang ${SRCS})

TARGET_LINK_LIBRARIES(LLVMLang ${LLVM_LIBS})
