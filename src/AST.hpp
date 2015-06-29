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
	BLOCK,
	TypeDefinition,
	//Decleration,
	BINOP,
	Identifier,
	Variable,
	VARIABLE_MUTATION,
	RETURN_VALUE,
	Function,
	IntegerLiteral,
	FloatLiteral,
	Call
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


struct BinaryOperation : public Expression{
	Token binop;
	Expression* lhs;
	Expression* rhs;
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

//It is now time to have somesort of notion of scope!
struct Block : public Node {
	Block* parent;	//null if the global scope
	std::vector<Node*> members;
	std::unordered_map<std::string, Identifier*> identifiers;
};

struct ReturnValue : public Expression {
	Expression* value;
};

struct VariableMutation : public Node {
	Token op;
	Variable* variable;
	Expression* value;
};

struct Function : public Block {
	Identifier* ident;
	TypeDefinition* returnType;
	std::vector<Variable*> args;
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

Identifier* FindIdentifier(Block* block, std::string name);
Identifier* CreateIdentifier(Block* block, std::string name);
Variable* CreateVariable();
Block* CreateBlock(AST::Block* block);
BinaryOperation* CreateBinaryOperation(Token binop, AST::Expression* lhs, AST::Expression* rhs);
ReturnValue* CreateReturnValue(AST::Expression* value);

VariableMutation* CreateVariableMutation(Token op, AST::Variable* variable, AST::Expression* expr);
Function* CreateFunction();
Call* CreateCall();

IntegerLiteral* CreateIntegerLiteral();
IntegerLiteral* CreateIntegerLiteral(int64 value);

FloatLiteral* CreateFloatLiteral(float64 value);


extern AST::Block* globalScope;

}

#endif //AST_HPP
