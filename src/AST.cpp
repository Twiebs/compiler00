#include "AST.hpp"
ASTDefinition* typeVoid;
ASTDefinition* typeS32;
ASTDefinition* typeF32;

void InitalizeLanguagePrimitives(ASTBlock* scope, llvm::Module* module) {
	typeVoid = CreateType(scope, "Void", llvm::Type::getVoidTy(module->getContext()));

	CreateType(scope, "S8", llvm::Type::getInt8Ty(module->getContext()));
	CreateType(scope, "S16", llvm::Type::getInt16Ty(module->getContext()));
	typeS32 = CreateType(scope, "S32", llvm::Type::getInt32Ty(module->getContext()));
	CreateType(scope, "S64", llvm::Type::getInt64Ty(module->getContext()));

	CreateType(scope, "F16", llvm::Type::getHalfTy(module->getContext()));
	typeF32 = CreateType(scope, "F32", llvm::Type::getFloatTy(module->getContext()));
	CreateType(scope, "F64", llvm::Type::getDoubleTy(module->getContext()));
	CreateType(scope, "F128", llvm::Type::getFP128Ty(module->getContext()));
}

ASTDefinition* CreateType(ASTBlock* scope, std::string name, llvm::Type* type) {
	auto typeDefn = new ASTDefinition;
	typeDefn->nodeType = AST_DEFINITION;
	typeDefn->llvmType = type;
	auto identifier = CreateIdentifier(scope, name);
	identifier->node = typeDefn;
	typeDefn->identifier = identifier;
	return typeDefn;
}

//Were going to add these Create things here in order to delegate the allocation of ast nodes
ASTIdentifier* CreateIdentifier(ASTBlock* block, std::string name) {
	auto result = new ASTIdentifier;
	result->name = name;
	block->identifiers[name] = result;
	return result;
}

ASTBinaryOperation* CreateBinaryOperation(Token binop, ASTExpression* lhs, ASTExpression* rhs) {
	auto result = new ASTBinaryOperation();
	result->nodeType = AST_BINOP;
	result->binop = binop;
	result->lhs = lhs;
	result->rhs = rhs;
	return result;
}

ASTFunction* CreateFunction(ASTBlock* block) {
	ASTFunction* function = new ASTFunction;
	function->nodeType = AST_FUNCTION;
	function->parent = block;
	return function;
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

ASTReturn* CreateReturnValue(ASTExpression* value) {
	auto result = new ASTReturn();
	result->nodeType = AST_RETURN;
	result->value = value;
	return result;
}

ASTIntegerLiteral* CreateIntegerLiteral(S64 value) {
	auto result = new ASTIntegerLiteral();
	result->nodeType = AST_INTEGER_LITERAL;
	result->type = (ASTDefinition*)typeS32;
	result->value = value;
	return result;
}

ASTFloatLiteral* CreateFloatLiteral(F64 value) {
	auto result = new ASTFloatLiteral();
	result->nodeType = AST_FLOAT_LITERAL;
	result->type = (ASTDefinition*)typeF32;
	result->value = value;
	return result;
}

ASTVariable* CreateVariable(ASTBlock* block) {
	auto result = new ASTVariable;
	result->nodeType = AST_VARIABLE;
	result->block = block;
	result->allocaInst = nullptr;
	return result;
}

ASTMutation* CreateMutation(Token op, ASTVariable* variable, ASTExpression* expr) {
	auto result = new ASTMutation();
	result->nodeType = AST_MUTATION;
	result->op = op;
	result->variable = variable;
	result->value = expr;
	return result;
}

//Control flow
ASTIfStatement* CreateIfStatement(ASTExpression* expr) {
	auto result = new ASTIfStatement;
	result->nodeType = AST_IF;
	result->expr = expr;
	result->ifBlock = nullptr;
	result->elseBlock = nullptr;
	return result;
}
