#include "AST.hpp"
#include <cstring>

void* Allocate (MemoryArena* arena, size_t size) {
  if (arena->used + size >= arena->capacity) {
    assert(false);
  } else {
    auto ptr = (U8*)arena->memory + arena->used;
    arena->used += size;
    return (void*)ptr;
  }
}

ASTBlock global_defaultGlobalScope;
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

internal ASTDefinition* CreatePrimitiveType(ASTBlock* scope, const std::string& name) {
	auto ident = CreateIdentifier(scope, name);
	auto typeDefn = new ASTDefinition;
	typeDefn->nodeType = AST_DEFINITION;
	typeDefn->identifier = ident;
	ident->node = typeDefn;
	return typeDefn;
}

void InitalizeLanguagePrimitives(ASTBlock* scope) {
	global_voidType = CreatePrimitiveType(scope, "Void");

	global_U8Type  = CreatePrimitiveType(scope, "U8");
	global_U16Type = CreatePrimitiveType(scope, "U16");
	global_U32Type = CreatePrimitiveType(scope, "U32");
	global_U64Type = CreatePrimitiveType(scope, "U64");

	global_S8Type  = CreatePrimitiveType(scope, "S8");
	global_S16Type = CreatePrimitiveType(scope, "S16");
	global_S32Type = CreatePrimitiveType(scope, "S32");
	global_S64Type = CreatePrimitiveType(scope, "S64");

	global_F16Type   = CreatePrimitiveType(scope, "F16");
	global_F32Type   = CreatePrimitiveType(scope, "F32");
	global_F64Type   = CreatePrimitiveType(scope, "F64");
	global_F128Type  = CreatePrimitiveType(scope, "F128");
}

global_variable std::unordered_map<std::string, ASTIdentifier*> global_identifierLookupMap;
global_variable ASTIdentifier global_identifiers[1024];
global_variable U32 global_identifierCount = 0;

ASTIdentifier* CreateIdentifier (const std::string& name) {
	auto result = &global_identifiers[global_identifierCount++];
	global_identifierLookupMap[name] = result;
	result->name = name;
	return result;
}

ASTIdentifier* CreateIdentifier(ASTBlock* scope, const Token& token) {
	auto result = &global_identifiers[global_identifierCount++];
	global_identifierLookupMap[token.string] = result;
	result->site = token.site;
	result->name = token.string;
	scope->identifiers[token.string] = result;
	return result;
}

ASTIdentifier* CreateIdentifier(ASTBlock* scope, const std::string& name) {
	auto result = &global_identifiers[global_identifierCount++];
	global_identifierLookupMap[name] = result;
	result->name = name;
	scope->identifiers[name] = result;
	return result;
}

ASTIdentifier* FindIdentifier(const std::string& name) {
	auto result = global_identifierLookupMap[name];
	return result;
}

ASTIdentifier* FindIdentifier(ASTBlock* block, const Token& token) {
	auto result = global_identifierLookupMap[token.string];
	return result;
}

ASTIdentifier* FindIdentifier(ASTBlock* block, const std::string& name) {
	auto result = block->identifiers[name];
	if (!result && block->parent != nullptr)
		result = FindIdentifier(block->parent, name);
	return result;
}

ASTBinaryOperation* CreateBinaryOperation(MemoryArena* arena, TokenType binop, ASTExpression* lhs, ASTExpression* rhs) {
	auto result = (ASTBinaryOperation*)Allocate(arena, sizeof(ASTBinaryOperation));
	result->nodeType = AST_BINOP;
	result->binop = binop;
	//HACK
	//Reduntant error checking for the purposes of the HACK
	if(lhs->type != rhs->type) {
		LOG_ERROR("BINOP fail");
	}
	result->type = lhs->type;
	result->lhs = lhs;
	result->rhs = rhs;
	return result;
}

ASTStruct* CreateStruct() {
	auto result = new ASTStruct;
	result->nodeType = AST_STRUCT;
	return result;
}

ASTMemberOperation* CreateMemberOperation(MemoryArena* arena, ASTVariable* structVar, Operation operation, ASTExpression* expr, U32* indices, U32 indexCount) {
	auto result = (ASTMemberOperation*)Allocate(arena, sizeof(ASTMemberOperation) + (sizeof(U32) * indexCount));
	result->nodeType = AST_MEMBER_OPERATION;
	result->structVar = structVar;
	result->operation = operation;
  result->expr = expr;
  result->indexCount = indexCount;
  auto indexptr = (U32*)(result + 1);
  memcpy(indexptr, indices, sizeof(U32) * indexCount);
	return result;
}

ASTMemberExpr* CreateMemberExpr(MemoryArena* arena, ASTVariable* structVar, AccessModifer accessMod, U32* indices, U32 indexCount) {
	auto result = (ASTMemberExpr*)Allocate(arena, (sizeof(ASTMemberExpr) + (sizeof(U32*)*indexCount)));
	result->nodeType = AST_MEMBER_EXPR;
	result->structVar = structVar;
	result->indexCount = indexCount;
  result->accessMod = accessMod;
	auto resultIndices = (U32*)(result + 1);
	memcpy(resultIndices, indices, indexCount * sizeof(U32));
	return result;
}

ASTVarExpr* CreateVarExpr (MemoryArena* arena, ASTVariable* var, AccessModifer accessMod) {
	auto result = (ASTVarExpr*)Allocate(arena, sizeof(ASTVarExpr));
	result->nodeType = AST_VAR_EXPR;
	result->var = var;
	result->type = var->type;
  result->accessMod = accessMod;
	return result;
}

S32 GetMemberIndex(ASTStruct* structDefn, const std::string& memberName) {
	for (auto i = 0; i < structDefn->memberNames.size(); i++) {
		auto& structMemberName = structDefn->memberNames[i];
		if (!structMemberName.compare(memberName)) {
			return i;
		}
	}
	return -1;
}

ASTFunctionSet* CreateFunctionSet (ASTIdentifier* ident, ASTBlock* block) {
	ASTFunctionSet* funcSet = new ASTFunctionSet;
	funcSet->nodeType = AST_FUNCTION;
	funcSet->parent = block;
	ident->node = funcSet;
	return funcSet;
}

// Perhaps in the future a CreateFunction / could take a package instead of a block
ASTFunction* CreateFunction (ASTFunctionSet* funcSet) {
	ASTFunction* function = new ASTFunction;
	function->nodeType = AST_FUNCTION;
	function->parent = funcSet->parent;
  function->llvmFunction = nullptr;
	funcSet->functions.push_back(function);
	funcSet->parent->members.push_back(function);	// HACK the hacks are real!
	return function;
}

ASTFunction* FindFunction (ASTFunctionSet* funcSet, ASTExpression** args, U32 argc) {
  for (auto i = 0; i < funcSet->functions.size(); i++) {
    auto func = funcSet->functions[i];
    if (func->args.size() == argc) {
    	if (argc > 0) {
		  bool functionMatches = true;
		  for (auto i = 0; i < argc; i++) {
			  auto arg = args[i];
			  if (func->args[i]->type != arg->type) {
				  functionMatches = false;
			  }
		  }
		  if (functionMatches)
			  return func;
    	} else {
    		return func;
    	}
    }
  }
  return nullptr;
}

ASTFunction* FindMatchingFunction(ASTIdentifier* ident, ASTFunction* function) {
	auto funcSet = (ASTFunctionSet*)ident->node;
	assert(funcSet->nodeType == AST_FUNCTION);
	for(auto func : funcSet->functions) {
		bool functionsMatch = true;
		if(func->args.size() == function->args.size()) {
			for(U32 i = 0; i < func->args.size(); i++) {
				if(func->args[i]->type != function->args[i]->type) {
					functionsMatch = false;
				}
			}
		} else functionsMatch = false;
		if(functionsMatch) {
			if(func->returnType != function->returnType) {
				LOG_ERROR("Cannot overload function return types!  Arguments must differ!");
				return nullptr;
			}
			return func;
		}
	}
	return nullptr;
}

// We are going to try and do this thing with calls where we pack the arguments at the end
// Im not sure if we need to bother with the pointer since we know they will procede the argument count
// but for now it keeps it simple so i will leave it it will be intresting to see if it actualy works.  Eventualy this will
// use an allocator to create nodes for each package.
ASTCall* CreateCall (MemoryArena* arena, ASTExpression** argList, U32 argCount) {
	ASTCall* call = (ASTCall*)Allocate(arena, sizeof(ASTCall) + ((sizeof(ASTExpression*) * argCount)));
	call->nodeType = AST_CALL;
	call->argCount = argCount;
	call->function = nullptr;
	if (argCount > 0) {
		auto callArgs = (ASTExpression**)(call + 1);
		for (auto i = 0; i < argCount; i++) {
			callArgs[i] = argList[i];
		}
	}
	return call;
}

ASTBlock* CreateBlock(ASTBlock* block) {
	auto result = new ASTBlock();
	result->depth = (block == nullptr) ? 0 : block->depth + 1;
	result->parent = block;
	result->nodeType = AST_BLOCK;
	return result;
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

ASTVariable* CreateVariable (MemoryArena* arena, ASTBlock* block, ASTExpression* initalExpr) {
	auto result = (ASTVariable*)Allocate(arena, sizeof(ASTVariable));
	result->nodeType = AST_VARIABLE;
	result->initalExpression = initalExpr;
	result->block = block;
	result->allocaInst = nullptr;
	result->isPointer = false;
	return result;
}

// This is just analogous for a store
// Why do we need to use this notation / jargon
ASTVariableOperation* CreateVariableOperation(MemoryArena* arena, ASTVariable* var, ASTExpression* expr) {
	auto result = (ASTVariableOperation*)Allocate(arena, sizeof(ASTVariableOperation));
	result->nodeType = AST_VARIABLE_OPERATION;
	result->variable = var;
	result->value = expr;
	return result;
}

// Control flow
ASTIfStatement* CreateIfStatement(ASTExpression* expr) {
	auto result = new ASTIfStatement;
	result->nodeType = AST_IF;
	result->expr = expr;
	result->ifBody = nullptr;
	result->elseBody = nullptr;
	return result;
}

ASTIter* CreateIter(ASTIdentifier* ident, ASTExpression* start, ASTExpression* end, ASTExpression* step, ASTBlock* body) {
	auto result = new ASTIter;
	result->nodeType = AST_ITER;
	result->varIdent = ident;
	result->start = start;
	result->end = end;
	result->step = step;
	result->body = body;
	return result;
}

std::string ToString(ASTNodeType nodeType) {
	switch(nodeType) {
	case AST_IDENTIFIER: return "Identifier";
	default: return "Too Lazy to implement ToString for this identifier";
	}
}
