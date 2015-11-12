set(SRCS Build.cpp AST.cpp Lexer.cpp Parser.cpp Parser.hpp Analysis.cpp Codegen.cpp Interpreter.cpp ParserCommon.cpp ParserCommon.hpp)

add_executable(bang ${SRCS})
target_link_libraries(bang ${LLVM_LIBS})
install(TARGETS bang DESTINATION /usr/bin)
