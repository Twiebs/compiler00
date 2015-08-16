#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Module.h"

#include "Codegen.hpp"
#include "Common.hpp"

void Codegen(Package* package, const BuildContext& context) {
	for(ASTNode* node : package->globalScope.members) {
		Codegen(node, context);
	}
}

llvm::Value* Codegen(ASTNode* node, const BuildContext& context) {
	switch(node->nodeType) {
	case AST_BINOP:
		return Codegen((ASTBinaryOperation*)node, context);
	case AST_VARIABLE:
		return Codegen((ASTVariable*)node, context);
	case AST_MUTATION:
		return Codegen((ASTMutation*)node, context);
	case AST_FUNCTION:
		return Codegen((ASTFunction*) node, context);
	case AST_CALL:
		return Codegen((ASTCall*)node, context);
	case AST_IF:
		LOG_ERROR("Attemping to genericly code gen an if! Note this is very very bad");
		return nullptr;
	case AST_INTEGER_LITERAL:
		return Codegen((ASTIntegerLiteral*) node, context);
	case AST_FLOAT_LITERAL:
		return Codegen((ASTFloatLiteral*)node, context);
	case AST_RETURN:
		return Codegen((ASTReturn*)node, context);
	default:
		assert(!"UNHANDELED CODEGEN OF UNKOWN NODE");
		LOG_ERROR("UNHANDLED CODEGEN OF NODE");
		return nullptr;
	}
}

llvm::Value* Codegen(ASTBinaryOperation* binop, const BuildContext& context)  {
	auto builder = context.builder;
	llvm::Value* lhs = Codegen(binop->lhs, context);
	llvm::Value* rhs = Codegen(binop->rhs, context);

	if(lhs == nullptr || rhs == nullptr) {
		LOG_ERROR("Failed to emit code for binary operation!");
		return nullptr;
	}

	// NOTE DIV instructions are more complex then expected
	// Checking types is important when doing this
	// Perhaps to simplifiy the language no implict casts will be allowed
	switch(binop->binop) {
		case TOKEN_ADD: return builder->CreateAdd(lhs, rhs, "addtmp");
		case TOKEN_SUB: return builder->CreateSub(lhs, rhs, "subtmp");
		case TOKEN_MUL: return builder->CreateMul(lhs, rhs, "multmp");
		case TOKEN_DIV: return builder->CreateSDiv(lhs, rhs, "divtmp");


		default:
			LOG_ERROR("Invalid binary operator");
			return nullptr;
	}
}

llvm::Value* Codegen(ASTVariable* var, const BuildContext& context) {
	auto builder = context.builder;

	//This variable is being declared!
	//TODO sanity check flag that ensures that this is indeed a declaration
	if(var->allocaInst == nullptr) {
		var->allocaInst = builder->CreateAlloca(var->type->llvmType, 0, var->identifier->name);
		//This variable is being allocated on the stack but does not have an expr associated with it
		//We need to generate a default initializer for it!
		if(var->initalExpression == nullptr) {
			if(var->type->llvmType->isIntegerTy()) {
				var->initalExpression = CreateIntegerLiteral(0);
			} else if (var->type->llvmType->isFloatingPointTy()) {
				var->initalExpression = CreateFloatLiteral(0);
			}
			auto value = Codegen(var->initalExpression, context);
			builder->CreateStore(value, var->allocaInst);
			var->initalExpression = nullptr;//Make sure to set the expression that the variable is storing to null so it can be reused!
		}
	}

	//This variable has been allocated on the stack allready or was newly allocated
	if(var->initalExpression != nullptr) {
		auto value = Codegen(var->initalExpression, context);
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

llvm::Value* Codegen(ASTMutation* mut, const BuildContext& context) {
	auto builder = context.builder;

	if(mut->variable->allocaInst == nullptr) {
		LOG_ERROR("Cannot assign a value to an unitialized variable!");
		return nullptr;
	}
	//TODO optional load flag!
	auto value = Codegen(mut->value, context);
	if(value == nullptr) {
		LOG_ERROR("Could not emit code for expression when assigning value to " << mut->variable->identifier);
	}
	builder->CreateStore(value, mut->variable->allocaInst);
	return mut->variable->allocaInst;
}

llvm::Value* Codegen(ASTReturn* retVal, const BuildContext& context) {
	auto builder = context.builder;

	auto value = Codegen(retVal->value, context);
	if (value != nullptr) {
		builder->CreateRet(value);
		return value;
	}
	LOG_ERROR("Unable to create return value for function ....");
	return nullptr;
}


llvm::Function* Codegen(ASTFunction* function, const BuildContext& context) {
	auto builder = context.builder;

	LOG_VERBOSE("Codgen Function");
	//Push an array of llvm types derived from the argument list onto the stack
	std::vector<llvm::Type*> args;
	for(auto arg : function->args) {
		args.push_back(arg->type->llvmType);
	}

	llvm::FunctionType* funcType = llvm::FunctionType::get(function->returnType->llvmType, args, false);
	llvm::Function::LinkageTypes linkage = (function->members.size() == 0) ? llvm::Function::ExternalLinkage : llvm::Function::ExternalLinkage;
	llvm::Function* llvmFunc = llvm::Function::Create(funcType, linkage, function->ident->name, context.currentPackage->module);

	if(function->members.size() > 0) {
		llvm::BasicBlock* block = llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", llvmFunc);
		builder->SetInsertPoint(block);
	}

	// Create the allocas for our arguments!
	U32 i = 0;
	for(auto iter = llvmFunc->arg_begin(); i != args.size(); iter++, i++){
		iter->setName(function->args[i]->identifier->name);

		// Only emit code for function arguments if the function is not foregin!
		// For now this works by checking to see if the function has a body since we do not allow
		// For foward declarations
		if(function->members.size() > 0) {
			function->args[i]->allocaInst = builder->CreateAlloca(iter->getType(), 0, function->args[i]->identifier->name);
			builder->CreateStore(iter, function->args[i]->allocaInst);
		}
	}

	//The function must always do something...
	bool returnInstructionSeen = false;
	if (function->members.size() > 0) {
		for(U32 i = 0; i < function->members.size(); i++) {
			auto node = function->members[i];
			if(node->nodeType == AST_IF) {
				auto mergeBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "merge", llvmFunc);
				Codegen((ASTIfStatement*)node, mergeBlock, llvmFunc, context);
				builder->SetInsertPoint(mergeBlock);
				continue;
			}

			if(node->nodeType == AST_RETURN) {
				returnInstructionSeen = true;
			}
			Codegen(node, context);
		}
	}

	if(!returnInstructionSeen) {
		if(function->returnType == global_voidType) {
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

llvm::Value* Codegen(ASTCall* call, const BuildContext& context) {
	auto builder = context.builder;

	auto func = call->function->code;
	if (func == 0) {
		LOG_ERROR("Call to undefined function(" << call->ident->name<< ")");
		return nullptr;
	}

	std::vector<llvm::Value*> argsV;
	for (U32 i = 0, e = func->arg_size(); i != e; i++) {
		auto argV = Codegen(call->args[i], context);
		if(argV == nullptr) {
			LOG_DEBUG("Failed to emit code for call argument!");
			return nullptr;
		}
		argsV.push_back(argV);
	}

	if(call->function->returnType != global_voidType) {
		return builder->CreateCall(func, argsV, "calltmp");
	} else {
		return builder->CreateCall(func, argsV);
	}
}

llvm::Value* Codegen(ASTIfStatement* ifStatement, llvm::BasicBlock* mergeBlock, llvm::Function* function, const BuildContext& context) {
	auto builder = context.builder;

	auto condV = Codegen(ifStatement->expr, context);
	if(condV == nullptr) {
		LOG_ERROR("Could not emit code for if statement expression");
		return 0;
	}
	auto compare = builder->CreateICmpEQ(condV, llvm::ConstantInt::getTrue(llvm::getGlobalContext()), "ifcond");
	auto ifBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "if", function);
	auto elseBlock = mergeBlock;
	if(ifStatement->elseBlock != nullptr) {
		elseBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "else", function);
	}
	builder->CreateCondBr(compare, ifBlock, elseBlock);
	builder->SetInsertPoint(ifBlock);
	for(auto node : ifStatement->ifBlock->members) {
		if(node->nodeType == AST_IF) {
			ifBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "merge", function);
			Codegen((ASTIfStatement*)node, ifBlock, function, context);
			builder->SetInsertPoint(ifBlock);
			continue;
		}
		auto value = Codegen(node, context);
		if(!value) {
			LOG_DEBUG("Failed to emit code for value in body of ifstatement!");
		}
	}

	builder->CreateBr(mergeBlock);

	if(ifStatement->elseBlock != nullptr) {
		builder->SetInsertPoint(elseBlock);
		if(ifStatement->elseBlock->nodeType == AST_IF) {
			auto ifElse = (ASTIfStatement*)ifStatement->elseBlock;
			auto condition = Codegen(ifElse->expr, context);
			auto comp = builder->CreateICmpEQ(condition, llvm::ConstantInt::getTrue(llvm::getGlobalContext()), "elseifcond");
			auto elseIfBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "elseif", function);
			if(ifElse->elseBlock != nullptr) {

			}
		}


		for(auto node : ifStatement->elseBlock->members) {
			if(node->nodeType == AST_IF) {
				elseBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "merge", function);
				Codegen((ASTIfStatement*)node, elseBlock, function, context);
				builder->SetInsertPoint(elseBlock);
				continue;
			}
			auto value = Codegen(node, context);
			if(!value) {
				LOG_DEBUG("Failed to emit code for value in body of ifstatement!");
			}
		}
		builder->CreateBr(mergeBlock);
	}
	return mergeBlock;
}

llvm::Value* Codegen(ASTIter* iter, const BuildContext& context) {
	auto builder = context.builder;


	auto startValue = Codegen(iter.start);
	if(!startValue) return nullptr;

	auto parentBlock = builder->GetInsertBlock()->getParent();
	auto loopBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "loop", parentBlock);

	builder->CreateBr(loopBlock);
	builder->SetInsertPoint(lookBlock);


}

llvm::Value* Codegen(ASTIntegerLiteral* intNode, const BuildContext& context) {
	return llvm::ConstantInt::get(intNode->type->llvmType, intNode->value);
}
llvm::Value* Codegen(ASTFloatLiteral* floatNode, const BuildContext& context) {
	return llvm::ConstantFP::get(floatNode->type->llvmType, floatNode->value);
}
