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

	IF,
	ELSE,
	FOR,
	WHILE,

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

//Both identfieirs and FunctionSets are not really part of the AST
//They are metadata that mark up how nodes of the ast are handled
//An identifier is simply a handle to a node in the ast.
//This might end up being the best way to refer to ASTNodes rather than actualy passing around pointers
//Then FilePositions might be able to be stored within the nodes themselfes along with there name
//and ientifiers only become these abstract constructs that sole puprose it to access the actualy data of the nodes
//which will be stored elsewhere!
//Actualy it might be better to start that flag system that could be implemented
//Then we could get some very intersing behaviours on these identifiers!
struct Identifier {
	FilePosition position;	//Where was the identifier declared
	std::string name;				//What is the name of the identifier
	//It might be a good idea to store information about what this identifier is actualy refering to!
	Node* node = nullptr;		//What node does this identifier point to?  If its a nullptr then this identifier has not been resolved yet!
};


struct TypeDefinition : public Node {
	Identifier* identifier;
	llvm::Type* llvmType;
};

struct Expression : public Node {
	//We need to care about what an expression is going to evaluate to...
	AST::TypeDefinition* type;
};






//It is now time to have somesort of notion of scope!
struct Block : public Node {
	uint8 depth;
	Block* parent;	//null if the global scope
	std::vector<Node*> members;
	std::unordered_map<std::string, Identifier*> identifiers;
};

struct IfStatement : Node {
	Expression* expr;
	Block* ifBlock;
	Block* elseBlock;
};


struct Variable : public Expression {
	Identifier* identifier;
	Block* block;
	Expression* initalExpression;
	llvm::AllocaInst* allocaInst;
};

struct Function : public Block {
	Identifier* ident;
	TypeDefinition* returnType;
	std::vector<Variable*> args;
	llvm::Function* code;
};

//REFACTOR this could use a better name
//An identifier will point to a function set which will have various functions that corespond to that identifier stored
//within it!  This might be a pretty large falicy but for now i will certianly allow it..
//Mabye when namespace support is added to the language it might be possible to consider a function a namespace and it has
//members that are the actual concrete functions that corespond to that identifier but have diffrence function signitures
//@Refactor this is currently designated as a node even though this data structure does not particpate in the AST
//It is simply just a container that points to overloaded functions!
struct FunctionSet : public Node{
	Identifier* ident;
	std::vector<Function*> functions;
};


struct BinaryOperation : public Expression{
	Token binop;
	Expression* lhs;
	Expression* rhs;
};


struct ReturnValue : public Expression {
	Expression* value;
};

struct VariableMutation : public Node {
	Token op;
	Variable* variable;
	Expression* value;
};


struct Call : public Node {
	Identifier* ident;
	Function* function;
	std::vector<Expression*> args;
};

struct IntegerLiteral : public Expression {
	int64 value;
};

struct FloatLiteral : public Expression {
	float64 value;
};

void InitalizeLanguagePrimitives(llvm::Module* module);
void CreateType(AST::Block* block, std::string name, llvm::Type* type);

Identifier* FindIdentifier(Block* block, std::string name);
Identifier* CreateIdentifier(Block* block, std::string name);
Variable* CreateVariable(Block* block);
Block* CreateBlock(AST::Block* block);
BinaryOperation* CreateBinaryOperation(Token binop, AST::Expression* lhs, AST::Expression* rhs);
ReturnValue* CreateReturnValue(AST::Expression* value);

//Conrol flow
IfStatement* CreateIfStatement(Expression* expr);

VariableMutation* CreateVariableMutation(Token op, AST::Variable* variable, AST::Expression* expr);
Function* CreateFunction(AST::Block* block);
Call* CreateCall();


IntegerLiteral* CreateIntegerLiteral(int64 value);
FloatLiteral* CreateFloatLiteral(float64 value);
//TODO string literals!


extern AST::Block* globalScope;

}

#endif //AST_HPP
