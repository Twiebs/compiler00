#pragma once

#include <vector>
#include <unordered_map>

#include "llvm/IR/Value.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"

#include "Lexer.hpp"

enum ASTNodeType {
	AST_IDENTIFIER,
	AST_BLOCK,
	AST_DEFINITION,

	AST_IF,
	AST_ITER,

	AST_FUNCTION,
	AST_CALL,
	AST_RETURN,

	AST_INTEGER_LITERAL,
	AST_FLOAT_LITERAL,

	AST_VARIABLE,
	AST_MUTATION,
	AST_BINOP,
};

struct ASTNode {
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
struct ASTIdentifier {
	FileSite site;	// Where was the identifier declared
	std::string name;				//What is the name of the identifier
	//It might be a good idea to store information about what this identifier is actualy refering to!
	ASTNode* node = nullptr;		//What node does this identifier point to?  If its a nullptr then this identifier has not been resolved yet!
};

struct ASTDefinition : public ASTNode {
	ASTIdentifier* identifier;
	llvm::Type* llvmType;
};

struct ASTExpression : public ASTNode {
	//We need to care about what an expression is going to evaluate to...
	ASTDefinition* type;
};

//It is now time to have somesort of notion of scope!
struct ASTBlock : public ASTNode {
	U8 depth = 0;
	ASTBlock* parent = nullptr;	//null if the global scope
	std::vector<ASTNode*> members;
	std::unordered_map<std::string, ASTIdentifier*> identifiers;
};

struct ASTIfStatement : public ASTNode {
	ASTExpression* expr;
	ASTBlock* ifBlock;
	ASTBlock* elseBlock;
};

struct ASTIter : public ASTNode {
	ASTIdentifier* varIdent;
	ASTExpression* start;
	ASTExpression* end;
	ASTExpression* step;
	ASTBlock* body;
};

struct ASTVariable : public ASTExpression {
	ASTIdentifier* identifier;
	ASTBlock* block;
	ASTExpression* initalExpression = nullptr;
	llvm::AllocaInst* allocaInst;
};

struct ASTFunction : public ASTBlock {
	ASTIdentifier* ident;
	ASTDefinition* returnType;
	std::vector<ASTVariable*> args;
	llvm::Function* code;
};

// REFACTOR this could use a better name
//An identifier will point to a function set which will have various functions that corespond to that identifier stored
//within it!  This might be a pretty large falicy but for now i will certianly allow it..
//Mabye when namespace support is added to the language it might be possible to consider a function a namespace and it has
//members that are the actual concrete functions that corespond to that identifier but have diffrence function signitures
//@Refactor this is currently designated as a node even though this data structure does not particpate in the AST
//It is simply just a container that points to overloaded functions!
struct ASTFunctionSet : public ASTNode {
	ASTIdentifier* ident;
	std::vector<ASTFunction*> functions;
};

struct ASTBinaryOperation : public ASTExpression {
	TokenType binop;
	ASTExpression* lhs;
	ASTExpression* rhs;
};

struct ASTReturn : public ASTExpression {
	ASTExpression* value;
};

// We dont consider mutations of variables as opperations right?
// Or should we consider mutations the same as BinaryOperations
// And just be strict about where we allow them?
struct ASTMutation : public ASTNode {
	TokenType op;
	ASTVariable* variable;
	ASTExpression* value;
};

struct ASTCall : public ASTNode {
	ASTIdentifier* ident;
	ASTFunction* function;
	std::vector<ASTExpression*> args;
};

struct ASTIntegerLiteral : public ASTExpression {
	S64 value;
};

struct ASTFloatLiteral : public ASTExpression {
	F64 value;
};

void InitalizeLanguagePrimitives(ASTBlock* scope, llvm::Module* module);

//Make it simple for now
//We should never actualy have to index into these manualy durring the thingy!
template<typename T>
class NodeAllocator {
	NodeAllocator();
	~NodeAllocator();

	T* Alloc();
	void Free();

private:
	U32 count;
	U32 max;
	T* memory;
};

extern ASTBlock global_defaultGlobalScope;
extern ASTDefinition* global_voidType;
extern ASTDefinition* global_S32Type;
extern ASTDefinition* global_F32Type;

//Identifiers
ASTIdentifier* FindIdentifier(ASTBlock* block, const std::string& name);
ASTIdentifier* FindIdentifier(ASTBlock* block, const Token& token);
ASTIdentifier* CreateIdentifier(ASTBlock* scope, const Token& token);
ASTIdentifier* CreateIdentifier(ASTBlock* scope, const std::string& name);
ASTIdentifier* FindIdentifier(const std::string& name);
ASTIdentifier* CreateIdentifier(const std::string& name);
void ResolveIdentifier(ASTBlock* block, const std::string& name);	//WTF

ASTDefinition* CreateType(ASTBlock* block, const std::string& name, llvm::Type* type);

ASTBlock* CreateBlock(ASTBlock* block);
ASTFunction* CreateFunction(ASTBlock* block);
ASTIfStatement* CreateIfStatement(ASTExpression* expr);
ASTIter* CreateIter(ASTExpression* start, ASTExpression* end, ASTExpression* step);
ASTReturn* CreateReturnValue(ASTExpression* value);

ASTCall* CreateCall();
ASTVariable* CreateVariable(ASTBlock* block);
ASTBinaryOperation* CreateBinaryOperation(TokenType binop, ASTExpression* lhs, ASTExpression* rhs);
ASTMutation* CreateMutation(TokenType op, ASTVariable* variable, ASTExpression* expr);

ASTIntegerLiteral* CreateIntegerLiteral(S64 value);
ASTFloatLiteral* CreateFloatLiteral(F64 value);

std::string ToString(ASTNodeType nodeType);
