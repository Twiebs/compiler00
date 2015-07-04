#pragma once
#include <vector>
#include <unordered_map>
#include <fstream>

#include "CodeGenerator.hpp"
#include "Lexer.hpp"

struct Unit {
	ASTBlock scope;
};

class Parser {
public:
	Parser(llvm::Module* module, CodeGenerator* codeGenerator);
      ~Parser();

	void ParseFile(std::string fileName);

private:
	CodeGenerator* codeGenerator;
	Lexer* lexer;
	llvm::Module* module;

	ASTBlock* previousScope;
	ASTBlock* currentScope;
	Unit* currentUnit;

	std::unordered_map<int32, int32> precedenceMap;
	std::unordered_map<std::string, Unit*> parsedUnits;

	ASTNode* ParseStatement();
	ASTExpression* ParseExpression();
	ASTExpression* ParseExpressionRHS(int32 exprPrec, ASTExpression* lhs);
	ASTExpression* ParsePrimaryExpression();

	int32 GetCurrentTokenPrecedence();

	void SetScope();
};
