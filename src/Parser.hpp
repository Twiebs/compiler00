#pragma once
#include <vector>
#include <unordered_map>
#include <fstream>

#include "CodeGenerator.hpp"
#include "Lexer.hpp"

//Considering that we dont need to preserve the tokenString here perhaps we can simply return it or somthging?
// Do we need some sort of internal lex_state so that we can keep track of those things without any overhea?
//For now ill just put it inside the package state and see what happens

//The current state of a parse as it walks a file
//This is much better
struct ParseState {
	ASTBlock* currentScope;
	LexState lex;	//TODO Consider storing the token information directly inside the parse state
	//And have the lext state contain only the information it needs to sucuessfuly lex the file...
	//This makes a lot of sense	//We can possibly return a token struct or somthing like that?
	//Mabye pass a pointer to a tokenstruct that gets initalized when the lexer lexes a file and then the result of the lexing
	//Is stored inside ofthat token struct!
};

//Mabye this is not a ParseState
//It could be considered a PackageState instead
//It will contain the file handle / etc that we need and then the packages could be moved
//out into the BuildContext
//This probably belongs inside the build.cpp
struct Package {
	std::string name;	//Just a simple name
	std::vector<std::string> importedPackages;
	ASTBlock scope;
};


int GetTokenPrecedence(Token token);
Package* FindPackage(ParseState* state, const std::string& packageName);

class Parser {
public:
	Parser(std::vector<std::string> importDirectories, llvm::Module* module, CodeGenerator* codeGenerator);
  ~Parser();

	void ParseFile(std::string fileName);

private:
	CodeGenerator* codeGenerator;
	Lexer* lexer;
	llvm::Module* module;

	//Iteresting...
	ASTBlock* previousScope;
	ASTBlock* currentScope;
	Package* currentPackage;

	std::vector<std::string> importDirectories;
	std::unordered_map<S32, S32> precedenceMap;
	std::unordered_map<std::string, Package*> parsedPackages;

	//Holds ASTDefinitions for primitave types!
	Package* primitivePackage;
	ASTIdentifier* FindIdentifierInScope(Package* activePackage, ASTBlock* block, std::string identString);
	Package* CreatePackage(std::string filename);

	bool ExpectAndEat(Token token);

	ASTNode* ParseStatement();
	ASTExpression* ParseExpression();
	ASTExpression* ParseExpressionRHS(S32 exprPrec, ASTExpression* lhs);
	ASTExpression* ParsePrimaryExpression();

	void SetScope();
};
