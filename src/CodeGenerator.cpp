
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
	case ASTNodeType::BINOP:
		return Codegen((AST::BinaryOperation*)node);
	case ASTNodeType::Variable:
		return Codegen((AST::Variable*) node);
	case ASTNodeType::VARIABLE_MUTATION:
		return Codegen((AST::VariableMutation*)node);
	case ASTNodeType::Function:
		return Codegen((AST::Function*) node);
	case ASTNodeType::Call:
		return Codegen((AST::Call*) node);
	case ASTNodeType::IntegerLiteral:
		return Codegen((AST::IntegerLiteral*) node);
	case ASTNodeType::FloatLiteral:
		return Codegen((AST::FloatLiteral*) node);
	case ASTNodeType::RETURN_VALUE:
		return Codegen((AST::ReturnValue*)node);
	default:
		LOG_ERROR("UNHANDLED CODEGEN OF NODE");
		return nullptr;
	}
}

llvm::Value* CodeGenerator::Codegen(AST::BinaryOperation* binop)  {
	llvm::Value* lhs = Codegen(binop->lhs);
	llvm::Value* rhs = Codegen(binop->rhs);

	if(lhs == nullptr || rhs == nullptr) {
		LOG_ERROR("Failed to emit code for binary operation!");
		return nullptr;
	}

	switch(binop->binop) {
		case Token::ADD:
			return builder->CreateAdd(lhs, rhs, "addtmp");
		case Token::SUB:
			return builder->CreateSub(lhs, rhs, "subtmp");
		case Token::MUL:
			return builder->CreateMul(lhs, rhs, "multmp");
		case Token::DIV:	//Note dive might be broken because it wont do integer div or fp div
			return builder->CreateFDiv(lhs, rhs, "divtmp");
		default:
			LOG_ERROR("Invalid binary operator");
			return nullptr;
	}
}

llvm::Value* CodeGenerator::Codegen(AST::Variable* var) {
	//This variable is being declared!
	//TODO sanity check flag that ensures that this is indeed a declaration
	if(var->allocaInst == nullptr) {
		var->allocaInst = builder->CreateAlloca(var->type->llvmType, 0, var->identifier->name);
		//This variable is being allocated on the stack but does not have an expr associated with it
		//We need to generate a default initializer for it!
		if(var->initalExpression == nullptr) {
			if(var->type->llvmType->isIntegerTy()) {
				var->initalExpression = AST::CreateIntegerLiteral(0);
			} else if (var->type->llvmType->isFloatingPointTy()) {
				var->initalExpression = AST::CreateFloatLiteral(0);
			}
			auto value = Codegen(var->initalExpression);
			builder->CreateStore(value, var->allocaInst);
			var->initalExpression = nullptr;//Make sure to set the expression that the variable is storing to null so it can be reused!
		}
	}

	//This variable has been allocated on the stack allready or was newly allocated
	if(var->initalExpression != nullptr) {
		auto value = Codegen(var->initalExpression);
		builder->CreateStore(value, var->allocaInst);
		var->initalExpression = nullptr;
		return var->allocaInst;
		//If the expression that this var is not null then we need to store the value of that expr
		//This should never be used anywhere since its an assignment
		//NOTE this might not be the case when we and increment opperators as those are used inline as they are
		//assigned values so we might want to emit a load for this variable however for now we will assume that it will not
		//be used and might actually catch some extra errors that way!
	} else {
		//The expr is null so this is just a plain-old-load
		return builder->CreateLoad(var->allocaInst);
	}

	LOG_ERROR("Failed to emit code for varaible: " << var->identifier->name);
	return nullptr;
}

llvm::Value* CodeGenerator::Codegen(AST::VariableMutation* mut) {
	if(mut->variable->allocaInst == nullptr) {
		LOG_ERROR("Cannot assign a value to an unitialized variable!");
		return nullptr;
	}
	//TODO optional load flag!
	auto value = Codegen(mut->value);
	if(value == nullptr) {
		LOG_ERROR("Could not emit code for expression when assigning value to " << mut->variable->identifier);
	}
	builder->CreateStore(value, mut->variable->allocaInst);
	return mut->variable->allocaInst;
}

llvm::Value* CodeGenerator::Codegen(AST::ReturnValue* retVal) {
	auto value = Codegen(retVal->value);
	if (value != nullptr) {
		builder->CreateRet(value);
		return value;
	}
	LOG_ERROR("Unable to create return value for function ....");
	return nullptr;
}


llvm::Function* CodeGenerator::Codegen(AST::Function* function) {
	LOG_VERBOSE("Codgen Function");
	//Push an array of llvm types derived from the argument list onto the stack
	std::vector<llvm::Type*> args;
	for(auto arg : function->args) {
		args.push_back(arg->type->llvmType);
	}

	llvm::FunctionType* funcType = llvm::FunctionType::get(function->returnType->llvmType, args, false);
	llvm::Function::LinkageTypes linkage = (function->members.size() == 0) ? llvm::Function::ExternalLinkage : llvm::Function::ExternalLinkage;
	llvm::Function* llvmFunc = llvm::Function::Create(funcType, linkage, function->ident->name, module);

	if(function->members.size() > 0) {
		llvm::BasicBlock* block = llvm::BasicBlock::Create(module->getContext(), "entry", llvmFunc);
		builder->SetInsertPoint(block);
	}

	// Create the allocas for our arguments!
	uint32 i = 0;
	for(auto iter = llvmFunc->arg_begin(); i != args.size(); iter++, i++){
		iter->setName(function->args[i]->identifier->name);

		//Only emit code for function arguments if the function is not foregin!
		if(function->members.size() > 0) {
			function->args[i]->allocaInst = builder->CreateAlloca(iter->getType(), iter, function->args[i]->identifier->name);
			builder->CreateStore(iter, function->args[i]->allocaInst);
		}
	}

	//The function must always do something...
	if (function->members.size() > 0) {
		//TODO we need to make sure that a return value is specified for the function!
		//(if it has one...)
		for(uint32 i = 0; i < function->members.size(); i++) {
			auto node = function->members[i];
			Codegen(node);
		}
	}
	//TODO sanity check to make sure this function was foregin if it did not have a body
	//Also do a sainy check to make sure that it has created return values for all flow paths

	return llvmFunc;
}

llvm::Value* CodeGenerator::Codegen(AST::Call* call) {
	llvm::Function* function = module->getFunction(call->function->ident->name);
		if (function == 0) {
			LOG_ERROR("Call to undefined function(" << call->function->ident->name<< ")");
			return nullptr;
		}

		if (call->args.size() != function->arg_size()) {
			LOG_ERROR("Function Call to '" << call->function->ident->name << "' contains incorrect number of arguments!");
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
