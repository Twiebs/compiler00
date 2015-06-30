#include "AST.hpp"

AST::Block* AST::globalScope;

void AST::InitalizeLanguagePrimitives(llvm::Module* module) {
	// NOTE we probably should not initialize the globalScope here...
	globalScope = CreateBlock(nullptr);

	AST::CreateType(globalScope, "Void", llvm::Type::getVoidTy(module->getContext()));

	AST::CreateType(globalScope, "S8", llvm::Type::getInt8Ty(module->getContext()));
	AST::CreateType(globalScope, "S16", llvm::Type::getInt16Ty(module->getContext()));
	AST::CreateType(globalScope, "S32", llvm::Type::getInt32Ty(module->getContext()));
	AST::CreateType(globalScope, "S64", llvm::Type::getInt64Ty(module->getContext()));

	AST::CreateType(globalScope, "F16", llvm::Type::getHalfTy(module->getContext()));
	AST::CreateType(globalScope, "F32", llvm::Type::getFloatTy(module->getContext()));
	AST::CreateType(globalScope, "F64", llvm::Type::getDoubleTy(module->getContext()));
	AST::CreateType(globalScope, "F128", llvm::Type::getFP128Ty(module->getContext()));
}

void AST::CreateType(AST::Block* block, std::string name, llvm::Type* type) {
	auto typeDefn = new AST::TypeDefinition;
	typeDefn->nodeType = ASTNodeType::TypeDefinition;
	typeDefn->llvmType = type;
	auto identifier = AST::CreateIdentifier(globalScope, name);
	identifier->node = typeDefn;
	typeDefn->identifier = identifier;
}

AST::Identifier* AST::FindIdentifier(AST::Block* block, std::string name) {
	auto ident = block->identifiers[name];
	if(ident == nullptr && block->parent != nullptr)
		return FindIdentifier(block->parent, name);
	return ident;
}

//Were going to add these Create things here inorder to delegate the allocation of ast nodes
AST::Identifier* AST::CreateIdentifier(Block* block, std::string name) {
	auto result = new AST::Identifier;
	result->name = name;
	block->identifiers[name] = result;
	return result;
}

AST::BinaryOperation* AST::CreateBinaryOperation(Token binop, AST::Expression* lhs, AST::Expression* rhs) {
	auto result = new AST::BinaryOperation();
	result->nodeType = ASTNodeType::BINOP;
	result->binop = binop;
	result->lhs = lhs;
	result->rhs = rhs;
	return result;
}

AST::Function* AST::CreateFunction(AST::Identifier* ident, AST::Block* block) {
	AST::Function* function = new AST::Function;
	function->nodeType = ASTNodeType::Function;
	function->parent = block;
	if (ident->node == nullptr) {
		auto funcSet = new FunctionSet;
		funcSet->ident = ident;
		funcSet->functions.push_back(function);
		ident->node = funcSet;
	}
	return function;
}

AST::Call* AST::CreateCall() {
	AST::Call* call = new AST::Call;
	call->nodeType = ASTNodeType::Call;
	return call;
}

AST::IntegerLiteral* AST::CreateIntegerLiteral() {
	auto result = new AST::IntegerLiteral();
	result->nodeType = ASTNodeType::IntegerLiteral;
	result->intType = (AST::TypeDefinition*)AST::FindIdentifier(globalScope, "S32")->node;
	return result;
}

AST::Block* AST::CreateBlock(AST::Block* block) {
	auto result = new AST::Block();
	result->parent = block;
	result->nodeType = ASTNodeType::BLOCK;
	return result;
}

AST::ReturnValue* AST::CreateReturnValue(AST::Expression* value) {
	auto result = new AST::ReturnValue();
	result->nodeType = ASTNodeType::RETURN_VALUE;
	result->value = value;
	return result;
}

AST::IntegerLiteral* AST::CreateIntegerLiteral(int64 value) {
	auto result = new AST::IntegerLiteral();
	result->nodeType = ASTNodeType::IntegerLiteral;
	result->intType = (AST::TypeDefinition*)AST::FindIdentifier(globalScope, "S32")->node;
	result->value = value;
	return result;
}

AST::FloatLiteral* AST::CreateFloatLiteral(float64 value) {
	auto result = new AST::FloatLiteral();
	result->nodeType = ASTNodeType::FloatLiteral;
	result->floatType = (AST::TypeDefinition*)AST::FindIdentifier(globalScope, "F32")->node;
	result->value = value;
	return result;
}

AST::Variable* AST::CreateVariable(AST::Block* block) {
	auto result = new AST::Variable;
	result->nodeType = ASTNodeType::Variable;
	result->block = block;
	result->allocaInst = nullptr;
	return result;
}

AST::VariableMutation* AST::CreateVariableMutation(Token op, AST::Variable* variable, AST::Expression* expr) {
	auto result = new AST::VariableMutation();
	result->nodeType = ASTNodeType::VARIABLE_MUTATION;
	result->op = op;
	result->variable = variable;
	result->value = expr;
	return result;
}
