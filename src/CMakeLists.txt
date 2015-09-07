set(SRCS Build.cpp AST.cpp Codegen.cpp Lexer.cpp Parser.cpp)

add_executable(bang ${SRCS})
target_link_libraries(bang ${LLVM_LIBS})
install(TARGETS bang DESTINATION /usr/bin)
