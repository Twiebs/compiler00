
#include "CodeGenerator.hpp"

CodeGenerator::CodeGenerator(llvm::Module* module) {
	this->module = module;
	builder = new llvm::IRBuilder<>(module->getContext());
}

CodeGenerator::~CodeGenerator() {
	delete builder;
}

llvm::Value* CodeGenerator::Codegen(AST::Node* node) {
	switch(node->nodeType) {
	case ASTNodeType::Variable:
		return Codegen((AST::Variable*) node);
	case ASTNodeType::Function:
		return Codegen((AST::Function*) node);
	case ASTNodeType::Call:
		return Codegen((AST::Call*) node);
	case ASTNodeType::IntegerLiteral:
		return Codegen((AST::IntegerLiteral*) node);
	case ASTNodeType::FloatLiteral:
		return Codegen((AST::FloatLiteral*) node);
	default:
		LOG_ERROR("UNHANDLED CODEGEN OF NODE");
		return nullptr;
	}
}

llvm::Value* CodeGenerator::Codegen(AST::Variable* var) {
	//If this variable already has been pushed to the stack we return that inst
	if(var->allocaInst != nullptr) {
		return builder->CreateLoad(var->allocaInst);
	}

	if(var->initalExpression == nullptr) {
		//@HACK expressions are given some stuff!
		//Interger is assumed!
		//Initialize to zero!
		auto intLiteral = AST::CreateIntegerLiteral(0);
		var->initalExpression = intLiteral;
	}

	auto allocaInst = builder->CreateAlloca(var->type->llvmType, 0, var->identifier->name);
	auto value = Codegen(var->initalExpression);
	builder->CreateStore(value, allocaInst);
	var->allocaInst = allocaInst;	//This variable has been codegened and will use this as its
	//value from now on!

	//This is probably not what i want to do!
	//Why does codegen care what is emitted?
	return allocaInst;
}



llvm::Function* CodeGenerator::Codegen(AST::Function* function) {
	LOG_VERBOSE("Codgen Function");
	//Push an array of llvm types derived from the argument list onto the stack
	std::vector<llvm::Type*> args;
	for(auto arg : function->args) {
		args.push_back(arg->type->llvmType);
	}
	llvm::FunctionType* funcType = llvm::FunctionType::get(function->returnType->llvmType, args, false);
	llvm::Function::LinkageTypes linkage = (function->body.size() == 0) ? llvm::Function::ExternalLinkage : llvm::Function::ExternalLinkage;
	llvm::Function* llvmFunc = llvm::Function::Create(funcType, linkage, function->identifier->name, module);

	if (function->body.size() > 0) {
		llvm::BasicBlock* block = llvm::BasicBlock::Create(module->getContext(), "entry", llvmFunc);
		builder->SetInsertPoint(block);

		//Create a new block insider this function and
		//Set the IRBuilders insertion point to the block

		for (AST::Node* node : function->body) {
			Codegen(node);
		}

		llvm::Value* returnValue = llvm::ConstantInt::get(function->returnType->llvmType, 1);
		if (returnValue) {
			builder->CreateRet(returnValue);
			llvm::raw_os_ostream* stream = new llvm::raw_os_ostream(std::cout);
			llvm::verifyModule(*module, stream);
			return llvmFunc;
		}
	}

	return llvmFunc;

	//There was an error reading the body of the function
	//Remove the function
//	function->eraseFromParent();
//	LOG_ERROR("Error parsing body of function");
	return nullptr;

}

llvm::Value* CodeGenerator::Codegen(AST::Call* call) {
	llvm::Function* function = module->getFunction(call->function->identifier->name);
		if (function == 0) {
			LOG_ERROR("Call to undefined function(" << call->function->identifier->name<< ")");
			return nullptr;
		}

		if (call->args.size() != function->arg_size()) {
			LOG_ERROR("Function Call contains incorrect number of arguments!");
			return nullptr;
		}

		std::vector<llvm::Value*> argsV;
		for (uint32 i = 0, e = function->arg_size(); i != e; i++) {
			argsV.push_back(Codegen(call->args[i]));
			if (argsV.back() == 0)
				return nullptr;
		}
		return builder->CreateCall(function, argsV, "calltmp");
}

llvm::Value* CodeGenerator::Codegen(AST::IntegerLiteral* intNode) {
	return llvm::ConstantInt::get(intNode->intType->llvmType, intNode->value);
}
llvm::Value* CodeGenerator::Codegen(AST::FloatLiteral* floatNode) {
	return llvm::ConstantFP::get(floatNode->floatType->llvmType, floatNode->value);
}
