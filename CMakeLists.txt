cmake_minimum_required(VERSION 2.8.8)
set(CMAKE_CXX_COMPILER clang++)

project(LLVMLang)
if(WIN32)
    set(CMAKE_PREFIX_PATH C:/LLVM/share/llvm/cmake)
    list(APPEND CMAKE_MODULE_PATH C:/LLVM/share/llvm/cmake)
    message(STATUS ${CMAKE_MODULE_PATH})
endif(WIN32)

if(NOT CMAKE_ECLIPSE_VERSION)
    add_definitions(-DENABLE_COLOR_OUTPUT)
endif()



find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Uisng LLVMConfig.cmake in: ${LLVM_DIR}")


add_definitions(-std=c++14 -g -O0 )
add_definitions(${LLVM_DEFINITIONS})

llvm_map_components_to_libnames(LLVM_LIBS ${LLVM_TARGETS_TO_BUILD} 
  core
  support
  irreader
  bitwriter
  mcjit
  interpreter
  bitreader
  asmparser
  asmprinter
  irreader
  target
  codegen
  linker
  
  aarch64
  aarch64asmparser
  aarch64asmprinter
  aarch64codegen
  aarch64desc
  aarch64disassembler
  aarch64info
  aarch64utils

  x86asmparser
  x86asmprinter
  x86codegen
  x86desc
  x86disassembler
  x86info
  x86utils
)
message(STATUS "Found Libararies: " ${LLVM_LIBS})


if(MSVC)
  include_directories(C:/LLVM/include)
  link_directories(C:/LLVM/lib)
endif()

add_subdirectory(src)