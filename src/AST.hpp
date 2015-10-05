#pragma once

#include <vector>
#include <unordered_map>

#include "Common.hpp"

enum TokenType {
	TOKEN_UNKOWN,
	TOKEN_IMPORT,
	TOKEN_FOREIGN,
	TOKEN_IDENTIFIER,

	TOKEN_TYPE_DEFINE,
	TOKEN_TYPE_DECLARE,
	TOKEN_TYPE_INFER,
	TOKEN_TYPE_RETURN,

	TOKEN_STRUCT,
	TOKEN_ACCESS,	// TODO MemberAccess to be clearer?

	TOKEN_ADDRESS,
	TOKEN_VALUE,

	// Binops
	TOKEN_ADD,
	TOKEN_SUB,
	TOKEN_MUL,
	TOKEN_DIV,

	TOKEN_LOGIC_NOT,

	TOKEN_LOGIC_OR,
	TOKEN_LOGIC_AND,
	TOKEN_LOGIC_EQUAL,
	TOKEN_LOGIC_GREATER,
	TOKEN_LOGIC_LESS,
	TOKEN_LOGIC_GREATER_EQAUL,
	TOKEN_LOGIC_LESS_EQUAL,

	TOKEN_EQUALS,
	TOKEN_ADD_EQUALS,
	TOKEN_SUB_EQUALS,
	TOKEN_MUL_EQUALS,
	TOKEN_DIV_EQUALS,

	// Keywords
	TOKEN_IF,
	TOKEN_ELSE,
	TOKEN_ITER,
	TOKEN_TO,
	TOKEN_RETURN,

	// Literals
	TOKEN_TRUE,
	TOKEN_FALSE,

	// TODO we check if the token has a dot in it during the lex phase
	// then we check if it has a dot again in the parExpr phase.
	// Just return a different token it will be cheaper
	TOKEN_NUMBER,
	TOKEN_STRING,

    TOKEN_DOTDOT,

	TOKEN_PAREN_OPEN,
	TOKEN_PAREN_CLOSE,
	TOKEN_BLOCK_OPEN,
	TOKEN_BLOCK_CLOSE,
	TOKEN_ARRAY_OPEN,
	TOKEN_ARRAY_CLOSE,
	TOKEN_EOF
};

struct FileSite {
	std::string filename;
	U32 lineNumber;
	U32 columNumber;
	friend std::ostream& operator<<(std::ostream& output, const FileSite& site) {
		output << "[" << site.filename << " " << site.lineNumber << ":" << site.columNumber<< "]";
		return output;
	}
};

struct Token {
	FileSite site;
	TokenType type;
	std::string string;
};

// TODO add dynamic array
template<typename T>
class Array {
    size_t capacity;
    U32 count;
    T* data;
};



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
	AST_IDENTIFIER,
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
};

struct ASTDefinition : public ASTNode {
    char* name;
	void* llvmType = nullptr;
    ASTDefinition() { nodeType = AST_DEFINITION; }
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
	FileSite site;	// This is where this variable was declared.
	ASTExpression* initalExpression = nullptr;
    void* allocaInst = nullptr;
    bool isPointer = false;
    ASTVariable() { nodeType = AST_VARIABLE; }
};

struct ASTIter : public ASTNode {
	ASTExpression* start;
	ASTExpression* end;
	ASTExpression* step;
	ASTVariable* var;
	ASTBlock* body;
};



struct ASTFunction : public ASTBlock {
	char* name;
    ASTDefinition* returnType = nullptr;
    bool isVarArgs = false;
	std::vector<ASTVariable*> args;
	void* llvmFunction = nullptr;
};

// REFACTOR this could use a better name
// An identifier will point to a function set which will have various functions that corespond to that identifier stored
// within it!  This might be a pretty large falicy but for now i will certianly allow it..
// Mabye when namespace support is added to the language it might be possible to consider a function a namespace and it has
// members that are the actual concrete functions that corespond to that identifier but have diffrence function signitures
// @Refactor this is currently designated as a node even though this data structure does not particpate in the AST
// It is simply just a container that points to overloaded functions!

struct ASTFunctionSet : public ASTNode {
    std::vector<ASTFunction*> functions;
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
	UNARY_LOAD,
	UNARY_VALUE,
	UNARY_ADDRESS,
	UNARY_NOT
};

struct ASTMemberExpr : public ASTExpression {
	ASTVariable* structVar;
	UnaryOperator accessMod;
	U32 indexCount;
    U32* indices;
};

struct ASTVarExpr : public ASTExpression {
	ASTVariable* var;
	std::vector<U32> accessIndices;
	UnaryOperator accessMod;
};


struct ASTMemberOperation : public ASTNode {
	ASTVariable* structVar;
	ASTExpression* expr;
	Operation operation;
	U32 indexCount;
    U32* indices;
    ASTMemberOperation() { nodeType = AST_MEMBER_OPERATION; }
};

struct ASTVariableOperation : public ASTNode {
	ASTVariable* variable;
	Operation operation;
	ASTExpression* expr;
};

struct ASTBinaryOperation : public ASTExpression {
	Operation operation;
	ASTExpression* lhs;
	ASTExpression* rhs;
    ASTBinaryOperation(Operation operation, ASTExpression* lhs, ASTExpression* rhs)
        : operation(operation), lhs(lhs), rhs(rhs) { nodeType = AST_BINARY_OPERATION; }
};

struct ASTReturn : public ASTExpression {
	ASTExpression* value;
};


struct ASTCall : public ASTNode {
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
    union {
        S64 intVal;
        F64 floatVal;
        U8* stringVal;
    };
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

void InitalizeLanguagePrimitives(MemoryArena* arena, ASTBlock* scope);

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

// Identifiers
void AssignIdent (ASTBlock* block, ASTNode* node, const std::string& name);
ASTNode* FindNodeWithIdent(ASTBlock* block, const std::string& name);

// Statements
ASTFunctionSet* CreateFunctionSet (MemoryArena* arena);		// This is where identifiers are resolved into
ASTFunction* CreateFunction(MemoryArena* arena, ASTBlock* block, const std::string& name, ASTFunctionSet* funcSet);	// Functions now must be created within a function set

ASTBlock* CreateBlock(MemoryArena* arena, ASTBlock* block);

ASTStruct* CreateStruct (MemoryArena* arena, const std::string& name, ASTStructMember* members, U32 memberCount);
S32 GetMemberIndex(ASTStruct* structDefn, const std::string& memberName);

ASTVariable* CreateVariable(MemoryArena* arena, const FileSite& site, const std::string& name, ASTExpression* initalExpr = nullptr);

ASTCast* CreateCast(MemoryArena* arena, ASTDefinition* typeDefn, ASTExpression* expr);

// Operations
ASTVariableOperation* CreateVariableOperation(MemoryArena* arena, ASTVariable* variable, Operation op, ASTExpression* expr);
ASTMemberOperation*   CreateMemberOperation(MemoryArena* arena, ASTVariable* structVar, Operation op, ASTExpression* expr, U32* indices, U32 indexCount);
ASTBinaryOperation*   CreateBinaryOperation(MemoryArena* arena, Operation operation, ASTExpression* lhs, ASTExpression* rhs);

// Control Flow
ASTIfStatement* CreateIfStatement(MemoryArena* arena, ASTExpression* expr);	// TODO Why are ifstatements created without a body? also the body should probably be emitted into a stack thingyyy and then coppied into the if statement???
ASTIter* CreateIter(MemoryArena* arena, ASTVariable* var, ASTExpression* start, ASTExpression* end, ASTExpression* step = nullptr, ASTBlock* body = nullptr);
ASTReturn* CreateReturnValue(MemoryArena* arena, ASTExpression* value);

//===============
//  Expressions
//===============
ASTVarExpr* CreateVarExpr(MemoryArena* arena, ASTVariable* var, UnaryOperator accessMod);
ASTMemberExpr* CreateMemberExpr(MemoryArena* arena, ASTVariable* structVar, UnaryOperator accessMod, U32* indices, U32 indexCount);

ASTCall* CreateCall(MemoryArena* arena, ASTExpression** argumentList, U32 argumentCount, const std::string& name);

ASTIntegerLiteral* CreateIntegerLiteral(MemoryArena* arena, S64 value);
ASTFloatLiteral* CreateFloatLiteral(MemoryArena* arena, F64 value);
ASTStringLiteral* CreateStringLiteral (MemoryArena* arena, const std::string& string);

std::string ToString(ASTNodeType nodeType);
std::string ToString(Operation operation);

bool isFloatingPoint(ASTDefinition* type);
bool isSignedInteger(ASTDefinition* type);
bool isUnsignedInteger (ASTDefinition* type);