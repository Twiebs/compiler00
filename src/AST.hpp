#pragma once

#include <vector>
#include <unordered_map>

#include "llvm/IR/Value.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Instructions.h"

#include "Lexer.hpp"

#define ARENA_BLOCK_SIZE 4096
struct MemoryArena {
  size_t used = 0;
  size_t capacity = 0;
  void* memory = nullptr;
};
void* Allocate (MemoryArena* arena, size_t size);

enum ASTNodeType {
	AST_IDENTIFIER,
	AST_BLOCK,
	AST_DEFINITION,

	AST_FUNCTION,
	AST_STRUCT,

	AST_MEMBER_OPERATION,
	AST_VARIABLE_OPERATION,

	AST_MEMBER_EXPR,

	AST_VARIABLE,
	AST_VAR_EXPR,
	AST_MUTATION,

	AST_IF,
	AST_ITER,

	AST_CALL,
	AST_RETURN,

	AST_INTEGER_LITERAL,
	AST_FLOAT_LITERAL,
	AST_STRING_LITERAL,

	AST_BINOP,
};

struct ASTNode {
	ASTNodeType nodeType;
};

// Both identfieirs and FunctionSets are not really part of the AST
// They are metadata that mark up how nodes of the ast are handled
// An identifier is simply a handle to a node in the ast.
// This might end up being the best way to refer to ASTNodes rather than actualy passing around pointers
// Then FilePositions might be able to be stored within the nodes themselfes along with there name
// and ientifiers only become these abstract constructs that sole puprose it to access the actualy data of the nodes
// which will be stored elsewhere!
// Actualy it might be better to start that flag system that could be implemented
// Then we could get some very intersing behaviours on these identifiers!
struct ASTIdentifier {
	FileSite site;	// Where was the identifier declared
	std::string name;				// What is the name of the identifier
	// It might be a good idea to store information about what this identifier is actualy refering to!
	ASTNode* node = nullptr;		// What node does this identifier point to?  If its a nullptr then this identifier has not been resolved yet!
};

struct ASTDefinition : public ASTNode {
	ASTIdentifier* identifier;
	llvm::Type* llvmType;
};

struct ASTExpression : public ASTNode {
	// We need to care about what an expression is going to evaluate to...
	ASTDefinition* type;
};

// It is now time to have somesort of notion of scope!
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
	bool isPointer = false;
};

struct ASTFunction : public ASTBlock {
	ASTIdentifier* ident;
	ASTDefinition* returnType;
	std::vector<ASTVariable*> args;
	llvm::Function* code = nullptr;
};

// I dont like the idea of storing identifiers
// Inside of  ASTNodes
// Why is it relevant at all?
struct ASTStruct : public ASTDefinition {
	std::vector<std::string> memberNames;
	std::vector<ASTDefinition*> memberTypes;
};


enum Operation {
  OPERATION_ASSIGN,
  OPERATION_ADD,
  OPERATION_SUB,
  OPERATION_MUL,
  OPERATION_DIV
};

enum AccessModifer {
	ACCESS_LOAD,
	ACCESS_VALUE,
	ACCESS_ADDRESS,
};

struct ASTMemberExpr : public ASTExpression {
	ASTVariable* structVar;
  AccessModifer accessMod;
  U32 indexCount;
};

struct ASTVarExpr : public ASTExpression {
	ASTVariable* var;
	std::vector<U32> accessIndices;
	AccessModifer accessMod;
};

struct ASTMemberOperation : public ASTNode {
	ASTVariable* structVar;
	ASTExpression* expr;
	Operation operation;
  U32 indexCount;
};


// REFACTOR this could use a better name
// An identifier will point to a function set which will have various functions that corespond to that identifier stored
// within it!  This might be a pretty large falicy but for now i will certianly allow it..
// Mabye when namespace support is added to the language it might be possible to consider a function a namespace and it has
// members that are the actual concrete functions that corespond to that identifier but have diffrence function signitures
// @Refactor this is currently designated as a node even though this data structure does not particpate in the AST
// It is simply just a container that points to overloaded functions!
struct ASTFunctionSet : public ASTNode {
	ASTIdentifier* ident;
	ASTBlock* parent;
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

// This is the fullstatement that the parse ident should return
// If a variable mutation is being parsed
struct ASTVariableOperation : public ASTNode {
	ASTVariable* variable;
	ASTExpression* value;
};

// ASTCall stores its arguments after the struct itself
// The procede directly after the arg count and are just pointers to
// other nodes in the AST... for now... perhaps these should be concrete structs
// stored here and we do some crazyness to pack them at the end
struct ASTCall : public ASTNode {
	ASTFunction* function;
	U32 argCount;
};

struct ASTIntegerLiteral : public ASTExpression {
	S64 value;
};

struct ASTFloatLiteral : public ASTExpression {
	F64 value;
};

struct ASTStringLiteral : public ASTExpression {
	U32 charCount;
};

void InitalizeLanguagePrimitives(ASTBlock* scope, llvm::Module* module);

extern ASTBlock global_defaultGlobalScope;
extern ASTDefinition* global_voidType;
extern ASTDefinition* global_U8Type;
extern ASTDefinition* global_U16Type;
extern ASTDefinition* global_U32Type;
extern ASTDefinition* global_U64Type;
extern ASTDefinition* global_S8Type;
extern ASTDefinition* global_S16Type;
extern ASTDefinition* global_S32Type;
extern ASTDefinition* global_S64Type;
extern ASTDefinition* global_F16Type;
extern ASTDefinition* global_F32Type;
extern ASTDefinition* global_F64Type;
extern ASTDefinition* global_F128Type;

//Identifiers
ASTIdentifier* CreateIdentifier(ASTBlock* scope, const Token& token);
ASTIdentifier* CreateIdentifier(ASTBlock* scope, const std::string& name);
ASTIdentifier* CreateIdentifier(const std::string& name);
ASTIdentifier* FindIdentifier(ASTBlock* block, const std::string& name);
ASTIdentifier* FindIdentifier(ASTBlock* block, const Token& token);
ASTIdentifier* FindIdentifier(const std::string& name);
void ResolveIdentifier(ASTBlock* block, const std::string& name);	// WTF

// Statements
ASTFunctionSet* CreateFunctionSet(ASTIdentifier* ident, ASTBlock* block);		// This is where identifiers are resolved into
ASTFunction* CreateFunction(ASTFunctionSet* funcSet);	// Functions now must be created within a function set
ASTFunction* FindMatchingFunction(ASTIdentifier* ident, ASTFunction* function);
ASTFunction* FindFunction (ASTFunctionSet* funcSet, ASTExpression** args, U32 argc);

ASTBlock* CreateBlock(ASTBlock* block);

ASTStruct* CreateStruct (ASTBlock* block);
ASTStruct* CreateStruct();
S32 GetMemberIndex(ASTStruct* structDefn, const std::string& memberName);

S32 GetMemberIndex(ASTStruct* structDefn, const std::string& memberName);

ASTDefinition* CreateType(ASTBlock* block, const std::string& name, llvm::Type* type);

ASTVariable* CreateVariable(MemoryArena* arena, ASTBlock* block, ASTExpression* initalExpr = nullptr);

// Operations
ASTVariableOperation* CreateVariableOperation(MemoryArena* arena, ASTVariable* variable, ASTExpression* expr);
ASTBinaryOperation*   CreateBinaryOperation(MemoryArena* arena, TokenType binop, ASTExpression* lhs, ASTExpression* rhs);
ASTMemberOperation*   CreateMemberOperation(MemoryArena* arena, ASTVariable* structVar, Operation mode, ASTExpression* expr, U32* indices, U32 indexCount);

// Control Flow
ASTIfStatement* CreateIfStatement(ASTExpression* expr);	// TODO Why are ifstatements created without a body? also the body should probably be emitted into a stack thingyyy and then coppied into the if statement???
ASTIter* CreateIter(ASTIdentifier* ident, ASTExpression* start, ASTExpression* end, ASTExpression* step = nullptr, ASTBlock* body = nullptr);
ASTReturn* CreateReturnValue(MemoryArena* arena, ASTExpression* value);

//===============
//  Expressions
//===============
ASTVarExpr* CreateVarExpr(MemoryArena* arena, ASTVariable* var, AccessModifer accessMod);
ASTMemberExpr* CreateMemberExpr(MemoryArena* arena, ASTVariable* structVar, AccessModifer accessMod, U32* indices, U32 indexCount);

ASTCall* CreateCall(MemoryArena* arena, ASTExpression** argumentList, U32 argumentCount);
ASTIntegerLiteral* CreateIntegerLiteral(MemoryArena* arena, S64 value);
ASTFloatLiteral* CreateFloatLiteral(MemoryArena* arena, F64 value);
ASTStringLiteral* CreateStringLiteral (MemoryArena* arena, const std::string& string);


std::string ToString(ASTNodeType nodeType);
