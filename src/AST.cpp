#include "AST.hpp"

//Create a new ASTScope and auto initialize one for the global scope
//but for now we will just get it working with variables!
std::unordered_map<std::string, AST::Identifier*> gIdentifiers;
std::unordered_map<std::string, AST::Variable*> gVariables;

void AST::InitalizeLanguagePrimitives(llvm::Module* module) {
	AST::CreateType("Void", llvm::Type::getVoidTy(module->getContext()));

	AST::CreateType("S8", llvm::Type::getInt8Ty(module->getContext()));
	AST::CreateType("S16", llvm::Type::getInt16Ty(module->getContext()));
	AST::CreateType("S32", llvm::Type::getInt32Ty(module->getContext()));
	AST::CreateType("S64", llvm::Type::getInt64Ty(module->getContext()));

	AST::CreateType("F16", llvm::Type::getHalfTy(module->getContext()));
	AST::CreateType("F32", llvm::Type::getFloatTy(module->getContext()));
	AST::CreateType("F64", llvm::Type::getDoubleTy(module->getContext()));
	AST::CreateType("F128", llvm::Type::getFP128Ty(module->getContext()));
}

void AST::CreateType(std::string name, llvm::Type* type) {
	auto typeDefn = new AST::TypeDefinition;
	typeDefn->llvmType = type;
	auto identifier = AST::CreateIdentifier(name);
	identifier->node = typeDefn;
	typeDefn->identifier = identifier;
	gIdentifiers[name] = identifier;
}

AST::Identifier* AST::FindIdentifier(std::string name) {
	return gIdentifiers[name];
}

//Were going to add these Create things here inorder to delegate the allocation of ast nodes
AST::Identifier* AST::CreateIdentifier(std::string name) {
	auto result = new AST::Identifier;
	result->name = name;
	gIdentifiers[name] = result;
	return result;
}

AST::Function* AST::CreateFunction() {
	AST::Function* function = new AST::Function;
	function->nodeType = ASTNodeType::Function;
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
	result->intType = (AST::TypeDefinition*)AST::FindIdentifier("S32")->node;
	return result;
}

AST::IntegerLiteral* AST::CreateIntegerLiteral(int64 value) {
	auto result = new AST::IntegerLiteral();
	result->nodeType = ASTNodeType::IntegerLiteral;
	result->intType = (AST::TypeDefinition*)AST::FindIdentifier("S32")->node;
	result->value = value;
	return result;
}

AST::FloatLiteral* AST::CreateFloatLiteral() {
	auto result = new AST::FloatLiteral();
	result->nodeType = ASTNodeType::FloatLiteral;
	return result;
}

AST::Variable* AST::CreateVariable() {
	auto result = new AST::Variable;
	result->nodeType = ASTNodeType::Variable;
	result->allocaInst = nullptr;
	return result;
}
