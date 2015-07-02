#! /usr/bin/python
import os
import subprocess

os.chdir("build")
subprocess.call("make", shell=True)
os.chdir("..")
subprocess.call("build/src/LLVMLang test.src", shell=True)
subprocess.call("llc -filetype=obj test.bc -o test.o", shell=True)
subprocess.call("clang++ test.o -Lbuild/libcpp -lbang -lSDL -lGL -o app", shell=True)
