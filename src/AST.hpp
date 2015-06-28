/*
 * AST.hpp
 *
 *  Created on: Jun 18, 2015
 *      Author: Torin Wiebelt
 */

#ifndef AST_HPP_
#define AST_HPP_

#include <vector>
#include <unordered_map>
#include "llvm/IR/Value.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "Lexer.hpp"

//Please note
//This is fucking retarded!

enum class ASTNodeType {
	TypeDefinition,
	//Decleration,
	Identifier,
	Variable,
	Function,
	IntegerLiteral,
	FloatLiteral,
	Call
};

enum class BinOp {
	ADD,
	SUB,
	MUL,
	DIV,
	Count
};

namespace AST {

struct Node {
	ASTNodeType nodeType;
};


struct Identifier {
	FilePosition position;	//Where was the identifier declared
	std::string name;				//What is the name of the identifier
	Node* node = nullptr;		//What node does this identifier point to?  If its a nullptr then this identifier has not been resolved yet!
};

struct Expression : public Node{

};

struct BinaryOperation {
	BinOp op;
	Expression* lhs, rhs;
};


struct TypeDefinition : public Node{
	Identifier* identifier;
	llvm::Type* llvmType;
};

struct Variable : public Expression {
	Identifier* identifier;
	TypeDefinition* type;
	Expression* initalExpression;
	llvm::AllocaInst* allocaInst;
};

struct Function : public Node {
	Identifier* identifier;
	TypeDefinition* returnType;
	std::vector<Variable*> args;
	std::vector<Node*> body;
};

struct Call : public Node {
	Function* function;
	std::vector<Expression*> args;
};

struct IntegerLiteral : public Expression {
	TypeDefinition* intType;
	int64 value;
};

struct FloatLiteral : public Expression {
	TypeDefinition* floatType;
	float64 value;
};

void InitalizeLanguagePrimitives(llvm::Module* module);
void CreateType(std::string name, llvm::Type* type);

Identifier* FindIdentifier(std::string name);
Identifier* CreateIdentifier(std::string name);
Variable* CreateVariable();
Function* CreateFunction();
Call* CreateCall();

IntegerLiteral* CreateIntegerLiteral();
IntegerLiteral* CreateIntegerLiteral(int64 value);

FloatLiteral* CreateFloatLiteral();
}

#endif //AST_HPP
