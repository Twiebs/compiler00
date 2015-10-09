#include "AST.hpp"
#include "Build.hpp"
#include <cstring>

void* Allocate (MemoryArena* arena, size_t size) {
  if (arena->used + size >= arena->capacity) {
      if (arena->next == nullptr) {
          arena->next = (MemoryArena*)(malloc(sizeof(MemoryArena) + ARENA_BLOCK_SIZE));
          arena->next = new (arena->next) MemoryArena;
          arena->next->capacity = ARENA_BLOCK_SIZE;
          arena->next->memory = arena->next + 1;
      }
      return Allocate(arena->next, size);
  } else {
    auto ptr = (U8*)arena->memory + arena->used;
    arena->used += size;
    return (void*)ptr;
  }
}

ASTDefinition* global_voidType;
ASTDefinition* global_U8Type;
ASTDefinition* global_U16Type;
ASTDefinition* global_U32Type;
ASTDefinition* global_U64Type;
ASTDefinition* global_S8Type;
ASTDefinition* global_S16Type;
ASTDefinition* global_S32Type;
ASTDefinition* global_S64Type;
ASTDefinition* global_F16Type;
ASTDefinition* global_F32Type;
ASTDefinition* global_F64Type;
ASTDefinition* global_F128Type;

// HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK
// HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK
// HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK
// HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK
// HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK
// HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK
// HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK
// HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK
// HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK) ^ (1 << 64)

// TODO put nodeType initalization in the constructor of the ASTNodes

internal inline ASTDefinition* CreatePrimitiveType(MemoryArena* arena, ASTBlock* block, const std::string& name) {
	auto typeDefn = new (Allocate(arena, sizeof(ASTDefinition))) ASTDefinition;
    typeDefn->name = (char*)Allocate(arena, name.size() + 1);
    memcpy(typeDefn->name, name.c_str(), name.size() + 1);
    AssignIdent(block, typeDefn, name);
	return typeDefn;
}

void InitalizeLanguagePrimitives(MemoryArena* arena, ASTBlock* block) {
	global_voidType = CreatePrimitiveType(arena, block, "Void");

	global_U8Type  = CreatePrimitiveType(arena, block, "U8");
	global_U16Type = CreatePrimitiveType(arena, block, "U16");
	global_U32Type = CreatePrimitiveType(arena, block, "U32");
	global_U64Type = CreatePrimitiveType(arena, block, "U64");

	global_S8Type  = CreatePrimitiveType(arena, block, "S8");
	global_S16Type = CreatePrimitiveType(arena, block, "S16");
	global_S32Type = CreatePrimitiveType(arena, block, "S32");
	global_S64Type = CreatePrimitiveType(arena, block, "S64");

	global_F16Type   = CreatePrimitiveType(arena, block, "F16");
	global_F32Type   = CreatePrimitiveType(arena, block, "F32");
	global_F64Type   = CreatePrimitiveType(arena, block, "F64");
	global_F128Type  = CreatePrimitiveType(arena, block, "F128");
}

void AssignIdent (ASTBlock* block, ASTNode* node, const std::string& name) {
    if (block->parent == nullptr) {
#if FORCE_SINGLE_THREADED
        assert(false && "Must implement lock on global scope with current build setup!");
#endif
        // if the block is null then this block belongs to the packages global scope and we need to lock it because multiple threads
        // can try and write to it.
    }
    block->identmap[name] = node;
}

ASTNode* FindNodeWithIdent (ASTBlock* block, const std::string& name) {
	auto result = block->identmap[name];
	if (!result && block->parent != nullptr)
		result = FindNodeWithIdent(block->parent, name);
	return result;
}

ASTCast::ASTCast(ASTDefinition* type, ASTExpression* expr) {
    nodeType = AST_CAST;
    this->type = type;
    this->expr = expr;
}

ASTVariableOperation* CreateVariableOperation(MemoryArena* arena, ASTVariable* var, Operation op, ASTExpression* expr) {
	auto result = (ASTVariableOperation*)Allocate(arena, sizeof(ASTVariableOperation));
	result->nodeType = AST_VARIABLE_OPERATION;
	result->variable = var;
    result->operation = op;
	result->expr = expr;
	return result;
}


//ASTMemberOperation* CreateMemberOperation(MemoryArena* arena, ASTVariable* structVar, Operation operation, ASTExpression* expr, U32* indices, U32 indexCount) {
//	auto result = new (Allocate(arena, sizeof(ASTMemberOperation))) ASTMemberOperation(structVar, expr, operation);
//	result->indices = (U32*)Allocate(arena, sizeof(U32) * indexCount);
//    result->memberCount = indexCount;
//    memcpy(result->indices, indices, sizeof(U32) * indexCount);
//    return result;
//}


ASTMemberOperation* CreateMemberOperation(MemoryArena* arena, ASTVariable* structVar, Operation operation, ASTExpression* expr, const std::vector<std::string>& memberNames) {
	auto result = new (Allocate(arena, sizeof(ASTMemberOperation))) ASTMemberOperation(structVar, expr, operation);
	result->memberNames = (char**)Allocate(arena, sizeof(char*) * memberNames.size());
	result->indices = (U32*)Allocate(arena, sizeof(U32) * memberNames.size());
	result->memberCount = memberNames.size();

	for (U32 i = 0; i < memberNames.size(); i++) {
		result->memberNames[i] = (char*)Allocate(arena, sizeof(memberNames[i].length() + 1));
		memcpy(result->memberNames[i], memberNames[i].c_str(), memberNames[i].length() + 1);
	}
	return result;
}

ASTBinaryOperation* CreateBinaryOperation(MemoryArena* arena, Operation operation, ASTExpression* lhs, ASTExpression* rhs) {
	auto result = new (Allocate(arena, sizeof(ASTBinaryOperation))) ASTBinaryOperation(operation, lhs, rhs);
    return result;
}

ASTStruct* CreateStruct(MemoryArena* arena, const std::string& name,  ASTStructMember* members, U32 memberCount) {
	auto structDefn = new (Allocate(arena, sizeof(ASTStruct))) ASTStruct;
    structDefn->name = (char*)Allocate(arena, name.size() + 1);
    structDefn->members= (ASTStructMember*)Allocate(arena, sizeof(ASTStructMember) * memberCount);
    structDefn->nodeType = AST_STRUCT;
    structDefn ->memberCount = memberCount;
    memcpy(structDefn ->members, members, memberCount * sizeof(ASTStructMember));
    memcpy(structDefn->name, name.c_str(), name.size() + 1);
	return structDefn;
}


ASTMemberExpr* CreateMemberExpr(MemoryArena* arena, ASTVariable* structVar, UnaryOperator accessMod, U32* indices, U32 indexCount) {
	auto result = new (Allocate(arena, (sizeof(ASTMemberExpr)))) ASTMemberExpr(structVar, accessMod);
    result->indices = (U32*)Allocate(arena, sizeof(result->indices) * indexCount);
    result->memberCount = indexCount;
	memcpy(result->indices, indices, indexCount * sizeof(U32));
	return result;
}

ASTMemberExpr* CreateMemberExpr(MemoryArena* arena, ASTVariable* structVar, UnaryOperator accessMod, const std::vector<std::string>& memberNames) {
	auto result = new (Allocate(arena, (sizeof(ASTMemberExpr)))) ASTMemberExpr(structVar, accessMod);
	result->memberNames = (char**)Allocate(arena, sizeof(char*) * memberNames.size());
	result->indices = (U32*)Allocate(arena, sizeof(U32) * memberNames.size());
	result->memberCount = memberNames.size();
	for (U32 i = 0; i < memberNames.size(); i++) {
		result->memberNames[i] = (char*)Allocate(arena, memberNames[i].length() + 1);
		memcpy(result->memberNames[i], memberNames[i].c_str(), memberNames.size() + 1);
	}
	return result;
}

ASTVarExpr* CreateVarExpr (MemoryArena* arena, ASTVariable* var, UnaryOperator accessMod) {
	auto result = (ASTVarExpr*)Allocate(arena, sizeof(ASTVarExpr));
	result->nodeType = AST_VAR_EXPR;
	result->var = var;
	result->type = var->type;
    result->accessMod = accessMod;
	return result;
}

S32 GetMemberIndex(ASTStruct* structDefn, const std::string& memberName) {
	for (auto i = 0; i < structDefn->memberCount; i++) {
        auto structMember = structDefn->members[i];
		if (!memberName.compare(structMember.name)) {
			return i;
		}
	}
	return -1;
}

// Perhaps a identifier can point to a FunctionResolver()
// Which determines the correct call for the function to use

ASTFunctionSet* CreateFunctionSet (MemoryArena* arena) {
	ASTFunctionSet* funcSet = (ASTFunctionSet*)Allocate(arena, sizeof(ASTFunctionSet));
	funcSet->nodeType = AST_FUNCTION;
	return funcSet;
}

ASTFunction* CreateFunction (MemoryArena* arena, ASTBlock* block, const std::string& name, ASTFunctionSet* funcSet) {
	ASTFunction* function = new (Allocate(arena, sizeof(ASTFunction))) ASTFunction;
    function->name = (char*)Allocate(arena, name.size() + 1);
    function->parent = block;
	function->nodeType = AST_FUNCTION;
    function->llvmFunction = nullptr;
    memcpy(function->name, name.c_str(), name.size() + 1);
    funcSet->functions.push_back(function);
    return function;
}

// We are going to try and do this thing with calls where we pack the arguments at the end
// Im not sure if we need to bother with the pointer since we know they will procede the argument count
// but for now it keeps it simple so i will leave it it will be intresting to see if it actualy works.  Eventualy this will
// use an allocator to create nodes for each package.
ASTCall* CreateCall (MemoryArena* arena, ASTExpression** argList, U32 argCount, const std::string& name) {
	auto call = new (Allocate(arena, sizeof(ASTCall))) ASTCall;
    call->name = (char*)Allocate(arena, name.size() + 1);
    call->args = (ASTExpression**)Allocate(arena, sizeof(ASTExpression*) * argCount);
    call->argCount = argCount;
	call->nodeType = AST_CALL;
	call->argCount = argCount;
	call->function = nullptr;
    memcpy(call->name, name.c_str(), name.size() + 1);
    memcpy(call->args, argList, sizeof(ASTExpression*)*argCount);
	return call;
}

ASTCast* CreateCast(MemoryArena* arena, ASTDefinition* typeDefn, ASTExpression* expr) {
    auto cast = new (Allocate(arena, sizeof(ASTCast))) ASTCast(typeDefn, expr);
}

ASTBlock* CreateBlock(MemoryArena* arena, ASTBlock* parent) {
	auto block = new (Allocate(arena, sizeof(ASTBlock))) ASTBlock;
    block->parent = parent;
	return block;
}

// This is a statement the value in its name might be confusing.
// We should begin to seperate out the difference between the statements and the expressions
// And stuff like that
ASTReturn* CreateReturnValue(MemoryArena* arena, ASTExpression* value) {
	auto result = (ASTReturn*)Allocate(arena, sizeof(ASTReturn));
	result->nodeType = AST_RETURN;
	result->value = value;
	return result;
}

ASTIntegerLiteral* CreateIntegerLiteral (MemoryArena* arena, S64 value) {
	auto result = (ASTIntegerLiteral*)Allocate(arena, sizeof(ASTIntegerLiteral));
	result->nodeType = AST_INTEGER_LITERAL;
	result->type = (ASTDefinition*)global_S32Type;
	result->value = value;
	return result;
}

ASTFloatLiteral* CreateFloatLiteral (MemoryArena* arena, F64 value) {
	auto result = (ASTFloatLiteral*)Allocate(arena, sizeof(ASTFloatLiteral));
	result->nodeType = AST_FLOAT_LITERAL;
	result->type = (ASTDefinition*)global_F32Type;
	result->value = value;
	return result;
}

ASTStringLiteral* CreateStringLiteral (MemoryArena* arena, const std::string& str) {
	auto result = (ASTStringLiteral*)Allocate(arena, sizeof(ASTStringLiteral) + (str.size() + 1));
	result->nodeType = AST_STRING_LITERAL;
	result->type = global_U8Type;
	result->charCount = str.size();
	auto strptr = (U8*)(result + 1);
	memcpy(strptr, str.data(), str.size());
	strptr += str.size();
	*strptr = '\0';
	return result;
}

ASTVariable* CreateVariable (MemoryArena* arena, const FileSite& site, const std::string& name, ASTExpression* initalExpr) {
	auto result = new (Allocate(arena, sizeof(ASTVariable))) ASTVariable;
    result->name = (char*)Allocate(arena, name.size() + 1);
    memcpy(result->name, name.c_str(), name.size() + 1);
	result->initalExpression = initalExpr;
    return result;
}

// Control flow
ASTIfStatement* CreateIfStatement(MemoryArena* arena, ASTExpression* expr) {
    auto result = new (Allocate(arena, sizeof(ASTIfStatement))) ASTIfStatement;
	result->expr = expr;
	return result;
}

ASTIter* CreateIter(MemoryArena* arena, ASTVariable* var, ASTExpression* start, ASTExpression* end, ASTExpression* step, ASTBlock* body) {
	auto result = (ASTIter*)Allocate(arena, sizeof(ASTIter));
	result->nodeType = AST_ITER;
	result->var = var;
	result->start = start;
	result->end = end;
	result->step = step;
	result->body = body;
	return result;
}

std::string ToString(ASTNodeType nodeType) {
	switch (nodeType) {
        case AST_IDENTIFIER:    return "Identifier";
        case AST_VARIABLE:      return "Variable";
        case AST_DEFINITION:    return "Definition";
        case AST_STRUCT:    return "Struct";
        default: return "Too Lazy to implement ToString for this nodeType";
	}
}

std::string ToString(Operation operation) {
    switch (operation) {
    case OPERATION_ADD: return "+";
    default: return "Did not implement OperationToString";
    }
}


bool isFloatingPoint(ASTDefinition* type) {
    if (type == global_F32Type) return true;
    if (type == global_F64Type) return true;
    return false;
}

bool isSignedInteger(ASTDefinition* type) {
    if (type == global_S32Type) return true;
    if (type == global_S64Type) return true;
    return false;
}

bool isUnsignedInteger (ASTDefinition* type) {
    if (type == global_U8Type) return true;
    if (type == global_U16Type) return true;
    if (type == global_U32Type) return true;
    if (type == global_U64Type) return true;
    return false;
}

bool isType(ASTNode* node) {
	return node->nodeType == AST_DEFINITION || node->nodeType == AST_STRUCT;
}