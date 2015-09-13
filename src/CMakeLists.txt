set(SRCS Build.cpp AST.cpp Parser.cpp Lexer.cpp Codegen.cpp)

add_executable(bang ${SRCS})
target_link_libraries(bang ${LLVM_LIBS})
install(TARGETS bang DESTINATION /usr/bin)
