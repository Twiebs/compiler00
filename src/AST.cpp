#include "AST.hpp"
#include "Build.hpp"

//Returns the node assigned to the provided name.  If no node was associated
//with the name that the assocaition is created.  If the node returned from this
//procedure does not match the node provided than there is a name conflict!
ASTNode *AssignIdent(ASTBlock* block, ASTNode* node, InternString& name) {
  if (block->parent == nullptr) g_compiler.globalBlockMutex.lock();
  for (size_t i = 0; i < block->identifiers.size(); i++) {
    if (Equals(block->identifiers[i], name)) {
      if (block->parent == nullptr) g_compiler.globalBlockMutex.unlock();
      return block->identiferNodes[i];
    }
  }

  block->identifiers.push_back(name);
  block->identifierNodes.push_back(node);
  if (block->parent == nullptr) g_compiler.globalBlockMutex.unlock();
  return node;
}

ASTNode* FindNodeWithIdent(ASTBlock* block, const char *string, size_t length) {
  if (block->parent == nullptr) g_compiler.globalBlockMutex.lock();
    for (size_t i = 0; i < block->identifiers.size(); i++) {
    if (Equals(block->identifiers[i], string, length)) {
      if (block->parent == nullptr) g_compiler.globalBlockMutex.unlock();
      return block->identiferNodes[i];
    }
  }

  if (block->parent == nullptr) { 
    g_compiler.globalBlockMutex.unlock();
    return nullptr;
  } else {
    ASTNode *result = FindNodeWithIdent(block->parent, string, length);
    return result;
  }

  assert(false);
  return nullptr;
}


static inline ASTDefinition* CreatePrimitiveType(const char *name, Compiler *compiler) {
  ASTDefinition *typeDefn = compiler->blockAllocator.Allocate<ASTDefintion>(); 
  typdDefn->name = compiler->stringAllocator.CreateString(name, strlen(name));
  ASTNode *node = AssignIdent(block, typeDefn, name);
  assert(node == typeDefn);
	return typeDefn;
}

ASTCast::ASTCast(ASTDefinition* type, ASTExpression* expr) {
    nodeType = AST_CAST;
    this->type = type;
    this->expr = expr;
}

ASTVariableOperation* CreateVariableOperation (MemoryArena* arena, ASTVariable* var, Operation op, ASTExpression* expr) {
	auto result = new (Allocate(arena, sizeof(ASTVariableOperation))) ASTVariableOperation(var, op, expr);
	return result;
}

#if 0
//ASTMemberOperation* CreateMemberOperation(MemoryArena* arena, ASTVariable* structVar, Operation operation, ASTExpression* expr, U32* indices, U32 indexCount) {
//	auto result = new (Allocate(arena, sizeof(ASTMemberOperation))) ASTMemberOperation(structVar, expr, operation);
//	result->indices = (U32*)Allocate(arena, sizeof(U32) * indexCount);
//    result->memberCount = indexCount;
//    memcpy(result->indices, indices, sizeof(U32) * indexCount);
//    return result;
//}

//ASTMemberOperation* CreateMemberOperation (MemoryArena* arena, ASTVariable* structVar, Operation operation, ASTExpression* expr) {
//	auto result = new (Allocate(arena, sizeof(ASTMemberOperation))) ASTMemberOperation(structVar, expr, operation);
//	return result;
//}

//ASTMemberOperation* CreateMemberOperation(MemoryArena* arena, ASTVariable* structVar, Operation operation, ASTExpression* expr, const std::vector<std::string>& memberNames) {
//	auto result = new (Allocate(arena, sizeof(ASTMemberOperation))) ASTMemberOperation(structVar, expr, operation);
//	result->memberNames = (char**)Allocate(arena, sizeof(char*) * memberNames.size());
//	result->indices = (U32*)Allocate(arena, sizeof(U32) * memberNames.size());
//	result->memberCount = memberNames.size();
//
//	for (U32 i = 0; i < memberNames.size(); i++) {
//		result->memberNames[i] = (char*)Allocate(arena, memberNames[i].length() + 1);
//		memcpy(result->memberNames[i], memberNames[i].c_str(), memberNames[i].length() + 1);
//	}
//
//	return result;
//}
#endif

ASTBinaryOperation* CreateBinaryOperation(MemoryArena* arena, Operation operation, ASTExpression* lhs, ASTExpression* rhs) {
	auto result = new (Allocate(arena, sizeof(ASTBinaryOperation))) ASTBinaryOperation(operation, lhs, rhs);
    return result;
}

ASTStruct* CreateStruct(MemoryArena* arena, const FileSite& site, const std::string& name,  ASTStructMember* members, U32 memberCount) {
	auto structDefn = new (Allocate(arena, sizeof(ASTStruct))) ASTStruct(site);
    structDefn->name = (char *)Allocate(arena, name.size() + 1);
    structDefn->members= (ASTStructMember*)Allocate(arena, sizeof(ASTStructMember) * memberCount);
    structDefn->nodeType = AST_STRUCT;
    structDefn ->memberCount = memberCount;
    memcpy(structDefn->members, members, memberCount * sizeof(ASTStructMember));
    memcpy((void*)structDefn->name, name.c_str(), name.size() + 1);
	return structDefn;
}


ASTMemberExpr* CreateMemberExpr(MemoryArena* arena, ASTVariable* structVar) {
	auto result = new (Allocate(arena, (sizeof(ASTMemberExpr)))) ASTMemberExpr(structVar);
	return result;
}

//ASTMemberExpr* CreateMemberExpr(MemoryArena* arena, ASTVariable* structVar, UnaryOperator accessMod, const std::vector<std::string>& memberNames) {
//	auto result = new (Allocate(arena, (sizeof(ASTMemberExpr)))) ASTMemberExpr(structVar, accessMod);
//	result->memberNames = (char**)Allocate(arena, sizeof(char*) * memberNames.size());
//	result->indices = (U32*)Allocate(arena, sizeof(U32) * memberNames.size());
//	result->memberCount = memberNames.size();
//	for (U32 i = 0; i < memberNames.size(); i++) {
//		result->memberNames[i] = (char*)Allocate(arena, memberNames[i].length() + 1);
//		memcpy(result->memberNames[i], memberNames[i].c_str(), memberNames.size() + 1);
//	}
//	return result;
//}
//
//ASTVarExpr* CreateVarExpr (MemoryArena* arena, ASTVariable* var) {
//	auto result = new (Allocate(arena, sizeof(ASTVarExpr))) ASTVarExpr;
//	result->var = var;
//	result->type = var->type;
//	return result;
//}

S32 GetMemberIndex(ASTStruct* structDefn, const std::string& memberName) {
	for (U32 i = 0; i < structDefn->memberCount; i++) {
        auto structMember = structDefn->members[i];
		if (!memberName.compare(structMember.name)) {
			return i;
		}
	}
	return -1;
}

// Perhaps a identifier can point to a FunctionResolver()
// Which determines the correct call for the function to use

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
	return cast;
}

ASTBlock* CreateBlock(MemoryArena* arena, ASTBlock* parent) {
	auto block = new (Allocate(arena, sizeof(ASTBlock))) ASTBlock;
    block->parent = parent;
	return block;
}

// This is a statement the value in its name might be confusing.
// We should begin to seperate out the difference between the statements and the expressions
// And stuff like that
//ASTReturn* CreateReturnValue(MemoryArena* arena, ASTExpression* value) {
//	auto result = new (Allocate(arena, sizeof(ASTReturn))) ASTReturn;
//	result->value = value;
//	return result;
//}

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
	result->value = (U8*)Allocate(arena, str.size() + 1);
	memcpy(result->value, str.data(), str.size() + 1);
	return result;
}


ASTVariable* CreateVariable (MemoryArena* arena, const FileSite& site, const std::string& name, ASTExpression* initalExpr, S8 indirectionLevel) {
	auto result = new (Allocate(arena, sizeof(ASTVariable))) ASTVariable(initalExpr, indirectionLevel);
    result->name = (char*)Allocate(arena, name.size() + 1);
    memcpy(result->name, name.c_str(), name.size() + 1);
    return result;
}

// Control flow
ASTIfStatement* CreateIfStatement(MemoryArena* arena, ASTExpression* expr) {
    auto result = new (Allocate(arena, sizeof(ASTIfStatement))) ASTIfStatement;
	result->expr = expr;
	return result;
}

ASTIter* CreateIter(MemoryArena* arena, ASTVariable* var, ASTExpression* start, ASTExpression* end, ASTExpression* step, ASTBlock* body) {
	auto result = new (Allocate(arena, sizeof(ASTIter))) ASTIter;
	result->var = var;
	result->start = start;
	result->end = end;
	result->step = step;
	result->body = body;
	return result;
}

std::string ToString(ASTNodeType nodeType) {
	switch (nodeType) {
		case AST_INVALID:    return "INVALID NODE TPYE";
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

//===========================================================
//Consider storing flag information inside of type defintions
//so that these functions are not required

bool IsSignedInteger(ASTDefinition *type) {
  if (type == g_compiler->S32Type) return true;
  if (type == g_compiler->S64Type) return true;
  return false;
}

bool IsUnsignedInteger(ASTDefinition *type) {
  if (type == g_compiler->U8Type)  return true;
  if (type == g_compiler->U16Type) return true;
  if (type == g_compiler->U32Type) return true;
  if (type == g_compiler->U64Type) return true;
  return false;
}

bool IsFloatingPoint(ASTDefinition *type) {
  if (type == g_compiler->F32Type) return true;
  if (type == g_compiler->F64Type) return true;
  return false;
}

bool IsInteger(ASTDefinition* type) {
	bool result = isSignedInteger(type) || isUnsignedInteger(type);
	return result;
}

bool IsType(ASTNode *node) {
  bool result = node->nodeType == AST_DEFINITION || node->nodeType == AST_STRUCT;
  return result;
}


static inline bool IsBinaryOperator(TokenType type) {
  switch (type) {
    case TOKEN_ADD:
    case TOKEN_SUB:
    case TOKEN_MUL:
    case TOKEN_DIV:
    case TOKEN_CONSTRUCT:
      return true;
  }
  return false;
}





