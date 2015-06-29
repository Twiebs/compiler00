#ifndef PARSER_HPP_
#define PARSER_HPP_

#include <vector>
#include <unordered_map>
#include <fstream>

#include "CodeGenerator.hpp"
#include "Lexer.hpp"

class Parser {
public:
	Parser(llvm::Module* module, std::string filename, std::ifstream* stream);
	virtual ~Parser();

	void ParseFile();
private:
	llvm::Module* module;
	CodeGenerator* codeGenerator;
	Lexer* lexer;

	std::unordered_map<int32, int32> precedenceMap;

	AST::Node* ParseStatement();

	AST::Expression* ParseExpression();
	AST::Expression* ParseExpressionRHS(int32 exprPrec, AST::Expression* lhs);

	AST::Expression* ParsePrimaryExpression();


	int32 GetCurrentTokenPrecedence();

	void CreateType(std::string name, llvm::Type* type);
	//TODO these probably should not exist inside the parser.
	//They should be moved into a memory manager or something...
	//or... we could strip out this whole thing and move to a much more c style syntax
	//But considering that this is a compiler this probably is'nt the time that we want to be experimenting with that...
};

#endif /* PARSER_HPP_ */
