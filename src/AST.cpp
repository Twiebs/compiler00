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

S32 GetMemberIndex(ASTStruct* structDefn, const std::string& memberName) {
	for (U32 i = 0; i < structDefn->memberCount; i++) {
        auto structMember = structDefn->members[i];
		if (!memberName.compare(structMember.name)) {
			return i;
		}
	}
	return -1;
}

ASTCast::ASTCast(ASTDefinition* type, ASTExpression* expr) {
  nodeType = AST_CAST;
  this->type = type;
  this->expr = expr;
}

ASTVariable::ASTVariable(Worker *worker, SourceLocation& location, ASTExpression* initialExpr, 
                        S8 indirectionLevel, const char *name, size_t nameLength) {
  nodeType = AST_VARIABLE;
  variableName = worker->stringAllocator.CreateString(name, nameLength);
  initalExpression = initialExpr;
  this->indirectionLevel = indirectionLevel;
  type = nullptr;
}

ASTIntegerLiteral::ASTIntegerLiteral(S64 value) {
  nodeType = AST_INTEGER_LITERAL;
  type = g_compiler.S64Type;
  this->value = value;
}

ASTFloatLiteral::ASTFloatLiteral(F64 value) {
  nodeType = AST_FLOAT_LITERAL;
  type = g_compiler.F64Type;
  this->value = value;
} 

ASTStringLiteral::ASTStringLiteral(Worker *worker, const char *string, size_t length) {
  nodeType = AST_STRING_LITERAL;
  type = &g_compiler.U8Type;
  value = worker->stringAllocator.CreateString(string, length);
}

//=====================================================================================

ASTStruct* CreateStruct(Worker *worker, const SourceLocation& location, const char *name, size_t nameLength,  ASTStructMember* members, U32 memberCount) {
  ASTStruct *structDefn = worker->astAllocator.Allocate<ASTStruct>(location);
  structDefn->nodeType = AST_STRUCT;
  structDefn ->memberCount = memberCount;
  structDefn->name = worker->stringAllocator.CreateString(name, nameLength);
  structDefn->members = (ASTStructMember *)worker->astAllocator.Allocate(sizeof(ASTStructMember) * memberCount);
  memcpy(structDefn->members, members, memberCount * sizeof(ASTStructMember));
	return structDefn;
}

ASTFunction* CreateFunction(Worker *worker, ASTBlock *block, const char *name, size_t nameLength, ASTFunctionSet* funcSet) {
  ASTFunction *function = worker->astAllocator<ASTFunction>(block);
  function->name = worker->stringAllocator.CreateString(name, nameLength);
  function->llvmFunction = nullptr;
  funcSet->functions.push_back(function);
  return function;
}

ASTCall* CreateCall(Worker *worker, ASTExpression **argList, U32 argCount, const char *name, size_t nameLength) {
	ASTCall *call = worker->astAllocator.Allocate<ASTCall>();
	call->nodeType = AST_CALL;
  call->name = worker->stringAllocator.CreateString(name, nameLength);
  call->args = (ASTExpression **)worker->astAllocator.Allocate(sizeof(ASTExpression*) * argCount);
  call->argCount = argCount;
	call->function = nullptr;
  memcpy(call->args, argList, sizeof(ASTExpression*)*argCount);
	return call;
}

ASTIfStatement* CreateIfStatement(MemoryArena* arena, ASTExpression* expr) {
  ASTIfStatement *result = new (Allocate(arena, sizeof(ASTIfStatement))) ASTIfStatement;
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





