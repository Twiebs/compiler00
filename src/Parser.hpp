#pragma once
#include <vector>
#include <unordered_map>
#include <fstream>

#include "CodeGenerator.hpp"
#include "Lexer.hpp"

struct Unit {
	std::string filename;
	std::vector<std::string> importedUnits;
	ASTBlock scope;
};

class Parser {
public:
	Parser(std::vector<std::string> importDirectories, llvm::Module* module, CodeGenerator* codeGenerator);
  ~Parser();

	void ParseFile(std::string fileName);

private:
	CodeGenerator* codeGenerator;
	Lexer* lexer;
	llvm::Module* module;

	ASTBlock* previousScope;
	ASTBlock* currentScope;
	Unit* currentUnit;

	std::vector<std::string> importDirectories;
	std::unordered_map<S32, S32> precedenceMap;
	std::unordered_map<std::string, Unit*> parsedUnits;

	//Holds ASTDefinitions for primitave types!
	Unit* primitiveUnit;
	ASTIdentifier* FindIdentifierInScope(Unit* activeUnit, ASTBlock* block, std::string identString);
	Unit* CreateUnit(std::string filename);

	bool ExpectAndEat(Token token);

	ASTNode* ParseStatement();
	ASTExpression* ParseExpression();
	ASTExpression* ParseExpressionRHS(S32 exprPrec, ASTExpression* lhs);
	ASTExpression* ParsePrimaryExpression();

	S32 GetCurrentTokenPrecedence();

	void SetScope();
};
