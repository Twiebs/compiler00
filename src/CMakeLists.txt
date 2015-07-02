set(SRCS Main.cpp AST.cpp CodeGenerator.cpp Lexer.cpp Parser.cpp)
add_executable(LLVMLang ${SRCS})
target_link_libraries(LLVMLang ${LLVM_LIBS})
