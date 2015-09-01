#include "AST.hpp"



template<typename T, U32 initalElementCount, U32 subElementCount>
class NodeAllocator {
public:
	T* Allocate() {
		if (count < initalElementCount) {
			return elements[count++];
		} else {
			if (next == nullptr) {
				next = new NodeAllocator<T, subElementCount, subElementCount>();
			}
			return next->Allocate();
		}
	}
private:
	T elements[initalElementCount];
	U32 count;
	NodeAllocator<T, subElementCount, subElementCount>* next;
};


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

void InitalizeLanguagePrimitives(ASTBlock* scope, llvm::Module* module) {
	global_voidType = CreateType(scope, "Void", llvm::Type::getVoidTy(module->getContext()));

	global_U8Type = CreateType(scope, "U8",   llvm::IntegerType::get(module->getContext(), 8));
	global_U16Type = CreateType(scope, "U16", llvm::IntegerType::get(module->getContext(), 16));
	global_U32Type = CreateType(scope, "U32", llvm::IntegerType::get(module->getContext(), 32));
	global_U64Type = CreateType(scope, "U64", llvm::IntegerType::get(module->getContext(), 64));

	global_S8Type   = CreateType(scope, "S8", llvm::Type::getInt8Ty(module->getContext()));
	global_S16Type = CreateType(scope, "S16", llvm::Type::getInt16Ty(module->getContext()));
	global_S32Type = CreateType(scope, "S32", llvm::Type::getInt32Ty(module->getContext()));
	global_S64Type = CreateType(scope, "S64", llvm::Type::getInt64Ty(module->getContext()));

	global_F16Type   = CreateType(scope, "F16", llvm::Type::getHalfTy(module->getContext()));
	global_F32Type   = CreateType(scope, "F32", llvm::Type::getFloatTy(module->getContext()));
	global_F64Type   = CreateType(scope, "F64", llvm::Type::getDoubleTy(module->getContext()));
	global_F128Type = CreateType(scope, "F128", llvm::Type::getFP128Ty(module->getContext()));
}

ASTDefinition* CreateType(ASTBlock* scope, const std::string& name, llvm::Type* type) {
	auto ident = CreateIdentifier(scope, name);
	auto typeDefn = new ASTDefinition;
	typeDefn->nodeType = AST_DEFINITION;
	typeDefn->identifier = ident;
	typeDefn->llvmType = type;
	ident->node = typeDefn;
	return typeDefn;
}

global_variable std::unordered_map<std::string, ASTIdentifier*> global_identifierLookupMap;
global_variable ASTIdentifier global_identifiers[1024];
global_variable U32 global_identifierCount = 0;

global_variable NodeAllocator<ASTIdentifier, 1024, 64>* global_identifierAllocator;

void InitAllocators() {
	global_identifierAllocator = new NodeAllocator<ASTIdentifier, 1024, 64>();
}

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

ASTBinaryOperation* CreateBinaryOperation(TokenType binop, ASTExpression* lhs, ASTExpression* rhs) {
	auto result = new ASTBinaryOperation();
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

ASTMemberAccess* CreateMemberAccess(ASTVariable* structVar) {
	auto result = new ASTMemberAccess;
	result->nodeType = AST_MEMBER_ACCESS;
	result->structVar = structVar;
	return result;
}

ASTMemberAccess* CreateMemberAccess(ASTVariable* structVar, U32 index, AccessMode mode) {
	auto result = new ASTMemberAccess;
	result->nodeType = AST_MEMBER_ACCESS;
	result->structVar = structVar;
	result->mode = mode;
	return result;
}

ASTMemberExpr* CreateMemberExpr(ASTVariable* structVar, U32 memberIndex) {
	auto result = new ASTMemberExpr();
	result->nodeType = AST_MEMBER_EXPR;
	result->structVar = structVar;

	auto structDefn = (ASTStruct*)structVar->type;
	auto memberType = structDefn->memberTypes[memberIndex];
	result->type = memberType;
	return result;
}


ASTMemberExpr* CreateMemberExpr(ASTVariable* structVar) {
	auto result = new ASTMemberExpr();
	result->nodeType = AST_MEMBER_EXPR;
	result->structVar = structVar;
	auto structDefn = (ASTStruct*)structVar->type;
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

ASTVarExpr* CreateVarExpr(ASTVariable* var) {
	auto result = new ASTVarExpr;
	result->nodeType = AST_VAR_EXPR;
	result->var = var;
	result->type = var->type;
	return result;
}

ASTFunctionSet* CreateFunctionSet(ASTIdentifier* ident, ASTBlock* block) {
	ASTFunctionSet* funcSet = new ASTFunctionSet;
	funcSet->nodeType = AST_FUNCTION;
	funcSet->parent = block;
	ident->node = funcSet;
	return funcSet;
}

// Perhaps in the future a CreateFunction / could take a package instead of a block
ASTFunction* CreateFunction(ASTFunctionSet* funcSet) {
	ASTFunction* function = new ASTFunction;
	function->nodeType = AST_FUNCTION;
	function->parent = funcSet->parent;
	funcSet->functions.push_back(function);
	funcSet->parent->members.push_back(function);	// HACK the hacks are real!
	return function;
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


ASTCall* CreateCall() {
	ASTCall* call = new ASTCall;
	call->nodeType = AST_CALL;
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
ASTReturn* CreateReturnValue(ASTExpression* value) {
	auto result = new ASTReturn();
	result->nodeType = AST_RETURN;
	result->value = value;
	return result;
}

ASTIntegerLiteral* CreateIntegerLiteral (S64 value) {
	auto result = new ASTIntegerLiteral();
	result->nodeType = AST_INTEGER_LITERAL;
	result->type = (ASTDefinition*)global_S32Type;
	result->value = value;
	return result;
}

ASTFloatLiteral* CreateFloatLiteral(F64 value) {
	auto result = new ASTFloatLiteral();
	result->nodeType = AST_FLOAT_LITERAL;
	result->type = (ASTDefinition*)global_F32Type;
	result->value = value;
	return result;
}

ASTStringLiteral* CreateStringLiteral (const std::string& str) {
	auto result = new ASTStringLiteral ();
	result->nodeType = AST_STRING_LITERAL;
	result->type = global_U8Type;
	result->value = str;
	return result;
}

ASTVariable* CreateVariable (ASTBlock* block) {
	auto result = new ASTVariable;
	result->nodeType = AST_VARIABLE;
	result->block = block;
	result->allocaInst = nullptr;
	return result;
}

// This is just analogous for a store
// Why do we need to use this notation / jargon
ASTMutation* CreateMutation(ASTVariable* variable, ASTExpression* expr) {
	auto result = new ASTMutation();
	result->nodeType = AST_MUTATION;
	result->variable = variable;
	result->value = expr;
	return result;
}

// Control flow
ASTIfStatement* CreateIfStatement(ASTExpression* expr) {
	auto result = new ASTIfStatement;
	result->nodeType = AST_IF;
	result->expr = expr;
	result->ifBlock = nullptr;
	result->elseBlock = nullptr;
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
