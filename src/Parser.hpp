/*
 * Parser.hpp
 *
 *  Created on: Jun 18, 2015
 *      Author: torin
 */

#ifndef PARSER_HPP_
#define PARSER_HPP_

#include <vector>
#include <unordered_map>
#include <fstream>

#include "CodeGenerator.hpp"
#include "Lexer.hpp"

class Parser {
public:
	Parser(llvm::Module* module, std::ifstream* stream);
	virtual ~Parser();

	ASTNode* HandleToken(Token token);

	//Runs the parser until the EOF is hit
	void ParseFile();

	ASTNode* ParsePrimary();
	//This could be anything...
	ASTNode* ParseIdentifier();
	ASTExpression* ParseExpression();
	ASTNumber* ParseNumber();
	ASTFunction* ParseFunction(std::string& identifierName);

	ASTType* ParseType();
	ASTType* StringToType(std::string string);

private:
	llvm::Module* module;
	CodeGenerator* codeGenerator;
	Lexer* lexer;

	std::vector<ASTType> types;
	std::unordered_map<std::string, uint32> typeMap;
};

#endif /* PARSER_HPP_ */
