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

llvm_map_components_to_libnames(LLVM_LIBS
support
core
irreader
bitwriter
mcjit
bitreader
asmparser
asmprinter
irreader
target
codegen
linker

#=======
#Targets
#=======

aarch64
aarch64asmparser
aarch64asmprinter
aarch64codegen
aarch64desc
aarch64disassembler
aarch64info
aarch64utils

arm
armasmparser
armasmprinter
armcodegen
armdesc
armdisassembler
arminfo

cppbackend
cppbackendcodegen
cppbackendinfo

hexagon
hexagoncodegen
hexagondesc
hexagondisassembler
hexagoninfo

mips
mipsasmparser
mipsasmprinter
mipscodegen
mipsdesc
mipsdisassembler
mipsinfo

msp430
msp430asmprinter
msp430codegen
msp430desc
msp430info

nvptx
nvptxasmprinter
nvptxcodegen
nvptxdesc
nvptxinfo

powerpc
powerpcasmparser
powerpcasmprinter
powerpccodegen
powerpcdesc
powerpcdisassembler
powerpcinfo

r600
r600asmparser
r600asmprinter
r600codegen
r600desc
r600info

sparc
sparcasmparser
sparcasmprinter
sparccodegen
sparcdesc
sparcdisassembler
sparcinfo

systemz
systemzasmparser
systemzasmprinter
systemzcodegen
systemzdesc
systemzdisassembler
systemzinfo

x86asmparser
x86asmprinter
x86codegen
x86desc
x86disassembler
x86info
x86utils

xcore
xcoreasmprinter
xcorecodegen
xcoredesc
xcoredisassembler
xcoreinfo



#analysis

#asmparser
#asmprinter
#bitreader
#bitwriter
#codegen
#core

#debuginfo
#engine
#executionengine

#instcombine
#instrumentation
#interpreter
#ipa
#ipo
#irreader
#lineeditor
#linker
#lto
#mc
#mcdisassembler
##mcjit
#mcparser

#native
#nativecodegen

#objcarcopts
#object
#option

#profiledata

#runtimedyld
#scalaropts
#selectiondag

#support

#tablegen
#target
#transformutils
#vectorize
#x86

)
message(STATUS "Found Libararies: " ${LLVM_LIBS})


if(MSVC)
  include_directories("../include")
  link_directories("../lib")
endif()

add_subdirectory(src)
add_subdirectory(libcpp)
