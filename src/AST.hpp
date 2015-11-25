#pragma once

#include <vector>
#include <unordered_map>

#include "Common.hpp"
#include "Lexer.hpp"



// TODO add dynamic array
template<typename T>
class Array {
    size_t capacity;
    U32 count;
    T* data;
};

template<size_t MEMORY_SIZE>
struct MemoryBlock {
	size_t used = 0;
	U8 memory[MEMORY_SIZE];
};


//class ASTNodeAllocator {
//	typedef MemoryBlock<4096> ASTMemblock;
//	std::vector<ASTMemblock*> blocks;
//	void* Allocate(size_t size);
//
//public:
//	template<typename TNode, typename... TArgs>
//	T* CreateNode(TArgs... args);
//};
//
//template<typename TNode, typename... TArgs>
//T* ASTNodeAllocator::CreateNode(TArgs..args) {
//	
//}

#define ARENA_BLOCK_SIZE 4096
struct MemoryArena {
    size_t used = 0;
    size_t capacity = 0;
    void* memory = nullptr;
    MemoryArena* next = nullptr;

    template<typename T, typename... Args>
    T* alloc(Args... args);
};

void* Allocate (MemoryArena* arena, size_t size);

template<typename T, typename... Args>
T* MemoryArena::alloc(Args... args) {
    return new (Allocate(this, sizeof(T))) T(args...);
};



enum ASTNodeType {
	AST_INVALID,
	AST_BLOCK,
	AST_DEFINITION,

	AST_FUNCTION,
    AST_FUNCTION_SET, // TODO set function sets to this seperate nodeType
	AST_STRUCT,

    AST_VARIABLE,

	AST_MEMBER_OPERATION,
	AST_VARIABLE_OPERATION,
    AST_BINARY_OPERATION,
    AST_UNARY_OPERATION, // TODO consider switching to these for consistancy!

	AST_MEMBER_EXPR,
    AST_VAR_EXPR,
    AST_CAST,
    AST_CALL,

	AST_IF,
	AST_ITER,
	AST_RETURN,

	AST_INTEGER_LITERAL,
	AST_FLOAT_LITERAL,
	AST_STRING_LITERAL,
};

struct ASTNode {
	ASTNodeType nodeType;
	SourceLocation sourceLocation;
};

struct ASTDefinition : public ASTNode {
    const char* name;
	FileSite sourceLocation;

	void* llvmType = nullptr;
	ASTDefinition() { nodeType = AST_DEFINITION; }
    ASTDefinition(FileSite sourceLocation) 
		:sourceLocation(sourceLocation)
	{ nodeType = AST_DEFINITION; }
};

struct ASTExpression : public ASTNode {
	ASTDefinition* type;
};

struct ASTBlock : public ASTNode {
	ASTBlock* parent = nullptr;
	std::vector<ASTNode*> members;
    std::unordered_map<std::string, ASTNode*> identmap;
    ASTBlock() { nodeType = AST_BLOCK; }
};

struct ASTIfStatement : public ASTNode {
	ASTExpression* expr;
	ASTNode* ifBody = nullptr;
	ASTNode* elseBody = nullptr;
    ASTIfStatement() { nodeType = AST_IF; }
};


struct ASTVariable : public ASTExpression {
    char* name;
	char* typeName;
	FileSite site;	// This is where this variable was declared.
	ASTExpression* initalExpression;
    void* allocaInst = nullptr;
	S8 indirectionLevel;

	ASTVariable (ASTExpression* initialExpr, S8 indirectionLevel)
		: initalExpression(initialExpr),
		indirectionLevel(indirectionLevel)
	{
		nodeType = AST_VARIABLE;
		this->type = nullptr;
	}
};

struct ASTIter : public ASTNode {
	ASTExpression* start;
	ASTExpression* end;
	ASTExpression* step;
	ASTVariable* var;
	ASTBlock* body;

	ASTIter() { nodeType = AST_ITER; };
};

struct ASTFunction : public ASTBlock {
	char* name;
    ASTDefinition* returnType = nullptr;
    bool isVarArgs = false;
	std::vector<ASTVariable*> args;
	void* llvmFunction = nullptr;
	ASTFunction() { nodeType = AST_FUNCTION; };
};

// REFACTOR this could use a better name
// An identifier will point to a function set which will have various functions that corespond to that identifier stored
// within it!  This might be a pretty large falicy but for now i will certianly allow it..
// Mabye when namespace support is added to the language it might be possible to consider a function a namespace and it has
// members that are the actual concrete functions that corespond to that identifier but have diffrence function signitures
// @Refactor this is currently designated as a node even though this data structure does not particpate in the AST
// It is simply just a container that points to overloaded functions!

struct ASTFunctionSet : public ASTNode {
	ASTFunctionSet* parent;
    std::vector<ASTFunction*> functions;
	ASTFunctionSet(ASTFunctionSet* parent) : parent(parent) { nodeType = AST_FUNCTION_SET; }
};

// Structs should be stored seperately from the functions because they dont depend
// on anything except other structs, therefore it is more efficant to codegen them
// linearly
struct ASTStructMember {
    char* name;
    bool isPointer = false;
    ASTExpression* initalExpr;
    ASTDefinition* type;
};

struct ASTStruct : public ASTDefinition {
    ASTStructMember* members;
    U32 memberCount;
	ASTStruct(const FileSite& site)
		: ASTDefinition(site)
	{ nodeType = AST_STRUCT; }
};

enum Operation {
    OPERATION_ASSIGN,
    OPERATION_ADD,
    OPERATION_SUB,
    OPERATION_MUL,
    OPERATION_DIV,
    OPERATION_LT,
    OPERATION_GT,
    OPERATION_LTE,
    OPERATION_GTE,
    OPERATION_LOR,
    OPERATION_LAND,
};

enum UnaryOperator {
	UNARY_INDIRECTION,
	UNARY_NOT
};

struct ASTUnaryOp : ASTExpression {
	UnaryOperator op;
	ASTExpression* expr;
	S8 indirectionLevel;
	ASTUnaryOp(UnaryOperator op, ASTExpression* expr)
		: op(op), expr(expr) { }
};

struct ASTMemberAccess {
	U32 memberCount;
	U32* indices;
	char** memberNames;
};

struct ASTMemberExpr : public ASTExpression {
	ASTVariable* structVar;
	ASTMemberAccess access;

	ASTMemberExpr (ASTVariable* structVar) :
			structVar(structVar) { nodeType = AST_MEMBER_EXPR; }
};

struct ASTVarExpr : public ASTExpression {
	ASTVariable* var;
	ASTVarExpr(ASTVariable* variable)
		: var(variable)
	{ 
		nodeType = AST_VAR_EXPR; 
	}
};

struct ASTMemberOperation : public ASTNode {
	ASTVariable* structVar;
	ASTExpression* expr;
	Operation operation;
	ASTMemberAccess access;

    ASTMemberOperation(const SourceLocation& location, ASTVariable* structVar, ASTExpression* expr, Operation operation) :
			structVar(structVar), expr(expr), operation(operation) 
	{ 
		nodeType = AST_MEMBER_OPERATION; 
		sourceLocation = location;
	}
};

struct ASTVariableOperation : public ASTNode {
	ASTVariable* variable;
	Operation operation;
	ASTExpression* expr;

	ASTVariableOperation(ASTVariable* variable, Operation operation, ASTExpression* expr)
			: variable(variable), operation(operation), expr(expr) { nodeType = AST_VARIABLE_OPERATION; }
};

struct ASTBinaryOperation : public ASTExpression {
	Operation operation;
	ASTExpression* lhs;
	ASTExpression* rhs;
    ASTBinaryOperation(Operation operation, ASTExpression* lhs, ASTExpression* rhs)
        : operation(operation), lhs(lhs), rhs(rhs) { nodeType = AST_BINARY_OPERATION; }
};

struct ASTReturn : public ASTExpression {
	FileSite site;
	ASTExpression* value;
	ASTReturn(ASTExpression* expr, const FileSite& site)
			: site(site), value(expr) { nodeType = AST_RETURN; }
};

struct ASTCall : public ASTExpression {
    char* name;
    U32 argCount;
    ASTExpression** args;
	ASTFunction* function = nullptr;
    ASTCall() { nodeType = AST_CALL; }
};

struct ASTCast : public ASTExpression {
    ASTExpression* expr;
    ASTCast() { nodeType = AST_CAST; }
    ASTCast(ASTDefinition* type, ASTExpression* expr);
};

struct ASTLiteral : public ASTExpression {
    union { S64 intVal; F64 fltVal; U8* strVal; };
	explicit ASTLiteral(S32 intVal) : intVal(intVal) { nodeType = AST_INTEGER_LITERAL; }
	explicit ASTLiteral(F64 fltVal) : fltVal(fltVal) { nodeType = AST_FLOAT_LITERAL; }
	explicit ASTLiteral(U8* strVal) : strVal(strVal) { nodeType = AST_STRING_LITERAL; }
};

struct ASTIntegerLiteral : public ASTExpression {
	S64 value;
};

struct ASTFloatLiteral : public ASTExpression {
	F64 value;
};

struct ASTStringLiteral : public ASTExpression {
	U8* value;
};

void InitalizeLanguagePrimitives(MemoryArena* arena, ASTBlock* scope);

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
extern ASTIntegerLiteral* global_trueLiteral;
extern ASTIntegerLiteral* global_falseLiteral;


// Identifiers
void AssignIdent (ASTBlock* block, ASTNode* node, const std::string& name);
ASTNode* FindNodeWithIdent(ASTBlock* block, const std::string& name);

// Statements
ASTFunctionSet* CreateFunctionSet (MemoryArena* arena);		// This is where identifiers are resolved into
ASTFunction* CreateFunction(MemoryArena* arena, ASTBlock* block, const std::string& name, ASTFunctionSet* funcSet);	// Functions now must be created within a function set

ASTBlock* CreateBlock(MemoryArena* arena, ASTBlock* block);

ASTStruct* CreateStruct (MemoryArena* arena, const FileSite& site, const std::string& name, ASTStructMember* members, U32 memberCount);
S32 GetMemberIndex(ASTStruct* structDefn, const std::string& memberName);

ASTVariable* CreateVariable(MemoryArena* arena, const FileSite& site, const std::string& name, ASTExpression* initalExpr = nullptr, S8 indirectionLevel = 0);

ASTCast* CreateCast(MemoryArena* arena, ASTDefinition* typeDefn, ASTExpression* expr);

// Operations
ASTVariableOperation* CreateVariableOperation (MemoryArena* arena, ASTVariable* variable, Operation op, ASTExpression* expr);
// ASTMemberOperation*   CreateMemberOperation (MemoryArena* arena, ASTVariable* structVar, Operation op, ASTExpression* expr, U32* indices, U32 indexCount);
// ASTMemberOperation*   CreateMemberOperation (MemoryArena* arena, ASTVariable* structVar, Operation operation, ASTExpression* expr, const std::vector<std::string>& memberNames);
ASTMemberOperation*   CreateMemberOperation (MemoryArena* arena, ASTVariable* structVar, Operation op, ASTExpression* expr);

ASTBinaryOperation*   CreateBinaryOperation (MemoryArena* arena, Operation operation, ASTExpression* lhs, ASTExpression* rhs);

// Control Flow
ASTIfStatement* CreateIfStatement(MemoryArena* arena, ASTExpression* expr);	// TODO Why are ifstatements created without a body? also the body should probably be emitted into a stack thingyyy and then coppied into the if statement???
ASTIter* CreateIter(MemoryArena* arena, ASTVariable* var, ASTExpression* start, ASTExpression* end, ASTExpression* step = nullptr, ASTBlock* body = nullptr);
ASTReturn* CreateReturnValue(MemoryArena* arena, ASTExpression* value);

//===============
//  Expressions
//===============

ASTVarExpr* CreateVarExpr(MemoryArena* arena, ASTVariable* var, UnaryOperator accessMod);
ASTMemberExpr* CreateMemberExpr(MemoryArena* arena, ASTVariable* structVar);


// ASTMemberExpr* CreateMemberExpr(MemoryArena* arena, ASTVariable* structVar, UnaryOperator unary, const std::vector<std::string>& memberNames);

ASTCall* CreateCall(MemoryArena* arena, ASTExpression** argumentList, U32 argumentCount, const std::string& name);

ASTIntegerLiteral* CreateIntegerLiteral(MemoryArena* arena, S64 value);
ASTFloatLiteral* CreateFloatLiteral(MemoryArena* arena, F64 value);
ASTStringLiteral* CreateStringLiteral (MemoryArena* arena, const std::string& string);

std::string ToString(ASTNodeType nodeType);
std::string ToString(Operation operation);

bool isFloatingPoint(ASTDefinition* type);
bool isInteger(ASTDefinition* type);
bool isSignedInteger(ASTDefinition* type);
bool isUnsignedInteger (ASTDefinition* type);
bool isType(ASTNode* node);

// TODO make this a lookuptable rather than branching
inline int GetTokenPrecedence (const Token& token) {
	if (token.type == TOKEN_LOGIC_OR) 				return 5;
	if (token.type == TOKEN_LOGIC_AND) 				return 5;
	if (token.type == TOKEN_LOGIC_LESS)           	return 10;
	if (token.type == TOKEN_LOGIC_GREATER)        	return 10;
	if (token.type == TOKEN_LOGIC_LESS_EQUAL)     	return 10;
	if (token.type == TOKEN_LOGIC_GREATER_EQAUL)  	return 10;
	if (token.type == TOKEN_ADD) 					return 20;
	if (token.type == TOKEN_SUB) 					return 20;
	if (token.type == TOKEN_MUL) 					return 40;
	if (token.type == TOKEN_DIV) 					return 40;
	return -1;
}

// HACK there is a better way to do this but for now this works.
// This is nessecary because i dont want operations tied to the parsing of tokens
// because there may be suport for user defined operators in the future
// and intrinsic operations should be seperated from their tokens.
inline Operation TokenToOperation (const Token& token) {
	switch (token.type) {
		case TOKEN_EQUALS: return OPERATION_ASSIGN;
		case TOKEN_ADD: return OPERATION_ADD;
		case TOKEN_SUB: return OPERATION_SUB;
		case TOKEN_MUL: return OPERATION_MUL;
		case TOKEN_DIV: return OPERATION_DIV;
		case TOKEN_ADD_EQUALS: return OPERATION_ADD;
		case TOKEN_SUB_EQUALS: return OPERATION_SUB;
		case TOKEN_MUL_EQUALS: return OPERATION_MUL;
		case TOKEN_DIV_EQUALS: return OPERATION_DIV;

		case TOKEN_LOGIC_GREATER: return OPERATION_GT;
		case TOKEN_LOGIC_LESS: return OPERATION_LT;
		case TOKEN_LOGIC_GREATER_EQAUL: return OPERATION_GTE;
		case TOKEN_LOGIC_LESS_EQUAL: return OPERATION_LTE;

		case TOKEN_LOGIC_OR: return OPERATION_LOR;
		case TOKEN_LOGIC_AND: return OPERATION_LAND;
	}

	assert(false);
	return static_cast<Operation>(0);
}