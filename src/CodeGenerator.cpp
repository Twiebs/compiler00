
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
	case ASTNodeType::IF:
		LOG_ERROR("Attemping to genericly code gen an if! Note this is very very bad");
		return nullptr;
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
			function->args[i]->allocaInst = builder->CreateAlloca(iter->getType(), 0, function->args[i]->identifier->name);
			builder->CreateStore(iter, function->args[i]->allocaInst);
		}
	}

	//The function must always do something...
	bool returnInstructionSeen = false;
	if (function->members.size() > 0) {
		for(uint32 i = 0; i < function->members.size(); i++) {
			auto node = function->members[i];
			if(node->nodeType == ASTNodeType::IF) {
				auto mergeBlock = llvm::BasicBlock::Create(module->getContext(), "merge", llvmFunc);
				Codegen((AST::IfStatement*)node, mergeBlock, llvmFunc);
				builder->SetInsertPoint(mergeBlock);
				continue;
			}

			if(node->nodeType == ASTNodeType::RETURN_VALUE) {
				returnInstructionSeen = true;
			}
			Codegen(node);
		}
	}

	auto voidType = (AST::TypeDefinition*)AST::FindIdentifier(AST::globalScope, "Void")->node;
	if(!returnInstructionSeen) {
		if(function->returnType == voidType) {
			builder->CreateRetVoid();
		} else if (function->members.size() > 0){
			LOG_ERROR("Non-void functions must have a return statement!");
		}
	}

	//TODO sanity check to make sure this function was foreign if it did not have a body
	//Also do a sainy check to make sure that it has created return values for all flow paths

	function->code = llvmFunc;
	return llvmFunc;
}

llvm::Value* CodeGenerator::Codegen(AST::Call* call) {
	auto func = call->function->code;
	if (func == 0) {
		LOG_ERROR("Call to undefined function(" << call->ident->name<< ")");
		return nullptr;
	}

	std::vector<llvm::Value*> argsV;
	for (uint32 i = 0, e = func->arg_size(); i != e; i++) {
		auto argV = Codegen(call->args[i]);
		if(argV == nullptr) {
			LOG_DEBUG("Failed to emit code for call argument!");
			return nullptr;
		}
		argsV.push_back(argV);
	}

	auto voidType = (AST::TypeDefinition*)AST::FindIdentifier(AST::globalScope, "Void")->node;
	if(call->function->returnType != voidType) {
		return builder->CreateCall(func, argsV, "calltmp");
	} else {
		return builder->CreateCall(func, argsV);
	}
}

llvm::Value* CodeGenerator::Codegen(AST::IfStatement* ifStatement, llvm::BasicBlock* mergeBlock, llvm::Function* function) {
	auto condV = Codegen(ifStatement->expr);
	if(condV == nullptr) {
		LOG_ERROR("Could not emit code for if statement expression");
		return 0;
	}
	auto compare = builder->CreateICmpEQ(condV, llvm::ConstantInt::getTrue(module->getContext()), "ifcond");
	auto ifBlock = llvm::BasicBlock::Create(module->getContext(), "if", function);
	auto elseBlock = mergeBlock;
	if(ifStatement->elseBlock != nullptr) {
		elseBlock = llvm::BasicBlock::Create(module->getContext(), "else", function);
	}
	builder->CreateCondBr(compare, ifBlock, elseBlock);
	builder->SetInsertPoint(ifBlock);
	for(auto node : ifStatement->ifBlock->members) {
		if(node->nodeType == ASTNodeType::IF) {
			ifBlock = llvm::BasicBlock::Create(module->getContext(), "merge", function);
			Codegen((AST::IfStatement*)node, ifBlock, function);
			builder->SetInsertPoint(ifBlock);
			continue;
		}
		auto value = Codegen(node);
		if(!value) {
			LOG_DEBUG("Failed to emit code for value in body of ifstatement!");
		}
	}

	builder->CreateBr(mergeBlock);

	if(ifStatement->elseBlock != nullptr) {
		builder->SetInsertPoint(elseBlock);
		if(ifStatement->elseBlock->nodeType == ASTNodeType::IF) {
			auto ifElse = (AST::IfStatement*)ifStatement->elseBlock;
			auto condition = Codegen(ifElse->expr);
			auto comp = builder->CreateICmpEQ(condition, llvm::ConstantInt::getTrue(module->getContext()), "elseifcond");
			auto elseIfBlock = llvm::BasicBlock::Create(module->getContext(), "elseif", function);
			if(ifElse->elseBlock != nullptr) {

			}
		}


		for(auto node : ifStatement->elseBlock->members) {
			if(node->nodeType == ASTNodeType::IF) {
				elseBlock = llvm::BasicBlock::Create(module->getContext(), "merge", function);
				Codegen((AST::IfStatement*)node, elseBlock, function);
				builder->SetInsertPoint(elseBlock);
				continue;
			}
			auto value = Codegen(node);
			if(!value) {
				LOG_DEBUG("Failed to emit code for value in body of ifstatement!");
			}
		}
		builder->CreateBr(mergeBlock);
	}
	return mergeBlock;
}

llvm::Value* CodeGenerator::Codegen(AST::IntegerLiteral* intNode) {
	return llvm::ConstantInt::get(intNode->type->llvmType, intNode->value);
}
llvm::Value* CodeGenerator::Codegen(AST::FloatLiteral* floatNode) {
	return llvm::ConstantFP::get(floatNode->type->llvmType, floatNode->value);
}
