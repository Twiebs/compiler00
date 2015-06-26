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
	Parser(llvm::Module* module, std::string filename, std::ifstream* stream);
	virtual ~Parser();

	ASTNode* HandleToken(Token token);

	//Runs the parser until the EOF is hit
	void ParseFile();

	AST::Node* ParsePrimary();
	AST::Node* ParseIdentifier();

	ASTExpression* ParseExpression();
	ASTFunction* ParseFunction(std::string& identifierName);

	ASTType* ParseType();
	AST::TypeDefinition* StringToTypeDefinition(std::string string);
	ASTType* StringToType(std::string string);

private:
	llvm::Module* module;
	CodeGenerator* codeGenerator;
	Lexer* lexer;

	//TODO add more create... here to remove memory management from the equation?
	void CreateType(std::string name, llvm::Type* type);

	std::vector<AST::TypeDefinition> typeDefinitions;
	std::unordered_map<std::string, uint32> namedTypeDefinitions;

	std::vector<ASTType> types;
	std::unordered_map<std::string, uint32> typeMap;

	std::unordered_map<std::string, AST::Identifier*> identifiers;
};

#endif /* PARSER_HPP_ */
