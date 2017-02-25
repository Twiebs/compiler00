set(SRCS Build.cpp AST.cpp AST.hpp Lexer.cpp Lexer.hpp Parser.cpp Parser.hpp Analysis.cpp Analysis.hpp LLVMCodegen.cpp)

add_executable(bang ${SRCS})
target_link_libraries(bang ${LLVM_LIBS})
install(TARGETS bang DESTINATION /usr/bin)
