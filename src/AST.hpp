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
	TOKEN_ACCESS,	// MemberAccess to be clearer?

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
	TOKEN_BOOLEAN_EQUAL,
	TOKEN_BOOLEAN_GREATER,
	TOKEN_BOOLEAN_LESS,
	TOKEN_BOOLEAN_GREATER_EQAUL,
	TOKEN_BOOLEAN_LESS_EQUAL,

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
	TOKEN_SCOPE_OPEN,
	TOKEN_SCOPE_CLOSE,
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
};

void* Allocate (MemoryArena* arena, size_t size);

enum ASTNodeType {
	AST_IDENTIFIER,
	AST_BLOCK,
	AST_DEFINITION,

	AST_FUNCTION,
	AST_STRUCT,

    AST_VARIABLE,

	AST_MEMBER_OPERATION,
	AST_VARIABLE_OPERATION,

	AST_MEMBER_EXPR,
    AST_VAR_EXPR,
    AST_CALL,

	AST_IF,
	AST_ITER,
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
	void* llvmType;  // Wha the fuck???? this makes no sense
};

struct ASTExpression : public ASTNode {
	// We need to care about what an expression is going to evaluate to...
	ASTDefinition* type;
};

// It is now time to have some sort of notion of scope!
struct ASTBlock : public ASTNode {
	U8 depth = 0;
	ASTBlock* parent = nullptr;	//null if the global scope
	std::vector<ASTNode*> members;
	std::unordered_map<std::string, ASTIdentifier*> identifiers;
};

struct ASTIfStatement : public ASTNode {
	ASTExpression* expr;
	ASTNode* ifBody;
	ASTNode* elseBody;
};


struct ASTVariable : public ASTExpression {
	FileSite site;	// This is where this variable was declared.
	ASTBlock* block;	// also why would we ever need to store this???
	ASTExpression* initalExpression = nullptr;
	void* allocaInst;
	bool isPointer = false;
	char* name;
};

struct ASTIter : public ASTNode {
	ASTExpression* start;
	ASTExpression* end;
	ASTExpression* step;
	ASTVariable* var;
	ASTBlock* body;
};

struct ASTFunction : public ASTBlock {
	ASTIdentifier* ident;
	ASTDefinition* returnType;
    bool isVarArgs = false;
	std::vector<ASTVariable*> args;
	void* llvmFunction;
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
  OPERATION_DIV
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
};

struct ASTVarExpr : public ASTExpression {
	ASTVariable* var;
	std::vector<U32> accessIndices;
	UnaryOperator accessMod;
};

// TODO
// I think that this and a mutation are essentially the exact same thing
// Member access just needs to hold some indices to how deep its reaching into is constituent members
// With an extra U32 inside of the struct we can keep track of the indices of the access and just combine
// the member access with a variable mutation... or we could just rename this to something better like
// ASTMemberOperation and ASTVariableOperation which i think may be a much better alternative
// These two things would be statements and would not require any return values

struct ASTMemberOperation : public ASTNode {
	ASTVariable* structVar;
	ASTExpression* expr;
	Operation operation;
	U32 indexCount;
    U32* indices;
};

struct ASTVariableOperation : public ASTNode {
	ASTVariable* variable;
	Operation operation;
	ASTExpression* expr;
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

// ASTCall stores its arguments after the struct itself
// The procede directly after the arg count and are just pointers to
// other nodes in the AST... for now... perhaps these should be concrete structs
// stored here and we do some crazyness to pack them at the end

// We could create a call with a name here instead and store some bytes for the name of the functions
// it would take a small amount of extra memory and it would be silly to do resoltuion like that for everysingle function...
// or would it??? it seams to be the best option rather than doing craziness with jumping around with a dependcy graph
// because we need to hit all these nodes again to typecheck them anyway
// it also allows the complier to be more flexiable and provide things like syntax highlighting without having to do more complex things like resolving the type of stuff
// for now we can pack this name on the back of the node... it may turn out that we dont even need to store stuff like the function that this call points to since that is
// only neseccary for compile time to be fair this is a serious amount of data that would do things... however; it may be a good idea for now... at least for the purposes of experimentation
struct ASTCall : public ASTNode {
	ASTFunction* function;
	U32 argCount;
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

void InitalizeLanguagePrimitives(ASTBlock* scope);

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
ASTIdentifier* CreateIdentifier(ASTBlock* scope, const Token& token);
ASTIdentifier* CreateIdentifier(ASTBlock* scope, const std::string& name);
ASTIdentifier* CreateIdentifier(const std::string& name);
ASTIdentifier* FindIdentifier(ASTBlock* block, const std::string& name);
ASTIdentifier* FindIdentifier(ASTBlock* block, const char* name);
ASTIdentifier* FindIdentifier(ASTBlock* block, const Token& token);
ASTIdentifier* FindIdentifier(const std::string& name);

// Statements
ASTFunctionSet* CreateFunctionSet(ASTIdentifier* ident, ASTBlock* block);		// This is where identifiers are resolved into
ASTFunction* CreateFunction(ASTFunctionSet* funcSet);	// Functions now must be created within a function set
ASTFunction* FindMatchingFunction(ASTIdentifier* ident, ASTFunction* function);

ASTBlock* CreateBlock(ASTBlock* block);

ASTStruct* CreateStruct (MemoryArena* arena, ASTStructMember* members, U32 memberCount);
S32 GetMemberIndex(ASTStruct* structDefn, const std::string& memberName);

S32 GetMemberIndex(ASTStruct* structDefn, const std::string& memberName);

ASTVariable* CreateVariable(MemoryArena* arena, const FileSite& site, ASTBlock* block, const char* name, ASTExpression* initalExpr = nullptr);

// Operations
ASTVariableOperation* CreateVariableOperation(MemoryArena* arena, ASTVariable* variable, Operation op, ASTExpression* expr);
ASTMemberOperation*   CreateMemberOperation(MemoryArena* arena, ASTVariable* structVar, Operation op, ASTExpression* expr, U32* indices, U32 indexCount);
ASTBinaryOperation*   CreateBinaryOperation(MemoryArena* arena, TokenType binop, ASTExpression* lhs, ASTExpression* rhs);

// Control Flow
ASTIfStatement* CreateIfStatement(ASTExpression* expr);	// TODO Why are ifstatements created without a body? also the body should probably be emitted into a stack thingyyy and then coppied into the if statement???
ASTIter* CreateIter(ASTVariable* var, ASTExpression* start, ASTExpression* end, ASTExpression* step = nullptr, ASTBlock* body = nullptr);
ASTReturn* CreateReturnValue(MemoryArena* arena, ASTExpression* value);

//===============
//  Expressions
//===============
ASTVarExpr* CreateVarExpr(MemoryArena* arena, ASTVariable* var, UnaryOperator accessMod);
ASTMemberExpr* CreateMemberExpr(MemoryArena* arena, ASTVariable* structVar, UnaryOperator accessMod, U32* indices, U32 indexCount);

ASTCall* CreateCall(MemoryArena* arena, ASTExpression** argumentList, U32 argumentCount, const char* name);

ASTIntegerLiteral* CreateIntegerLiteral(MemoryArena* arena, S64 value);
ASTFloatLiteral* CreateFloatLiteral(MemoryArena* arena, F64 value);
ASTStringLiteral* CreateStringLiteral (MemoryArena* arena, const std::string& string);

std::string ToString(ASTNodeType nodeType);
