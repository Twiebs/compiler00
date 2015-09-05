#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Module.h"

#include "Codegen.hpp"
#include "Common.hpp"

llvm::Value* Codegen(ASTStringLiteral* str, const BuildContext& context);

void Codegen(Package* package, const BuildContext& context) {
 	for(ASTNode* node : package->globalScope.members) {
		Codegen(node, context);
	}
}

void CodegenStatement(ASTNode* node, const BuildContext& context) {
  switch(node->nodeType) {
    case AST_FUNCTION:
      Codegen((ASTFunction*) node, context);
      break;
    case AST_ITER:
      Codegen((ASTIter*)node, context);
      break;
    case AST_RETURN:
      Codegen((ASTReturn*)node, context);
      break;
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
	case AST_MEMBER_ACCESS:
		return Codegen((ASTMemberAccess*)node, context);
	case AST_MEMBER_EXPR:
		return Codegen((ASTMemberExpr*)node, context);
	case AST_VAR_EXPR:
		return Codegen((ASTVarExpr*)node, context);
	case AST_FUNCTION:
		return Codegen((ASTFunction*) node, context);
	case AST_CALL:
		return Codegen((ASTCall*)node, context);
	case AST_IF:
		LOG_ERROR("Attemping to genericly code gen an if! Note this is very very bad");
		return nullptr;
	case AST_ITER:
		Codegen((ASTIter*)node, context);
		return nullptr;
		break;
	case AST_INTEGER_LITERAL:
		return Codegen((ASTIntegerLiteral*) node, context);
	case AST_FLOAT_LITERAL:
		return Codegen((ASTFloatLiteral*)node, context);
  case AST_STRING_LITERAL:
    return Codegen((ASTStringLiteral*)node, context);
	case AST_RETURN:
		return Codegen((ASTReturn*)node, context);
	default:
		assert(!"UNHANDELED CODEGEN OF UNKOWN NODE");
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
	if (var->allocaInst == nullptr) {
		auto type = var->type->llvmType;
		if(var->isPointer)
				type = llvm::PointerType::get(type, 0);
		var->allocaInst = builder->CreateAlloca(type, 0, var->identifier->name);
		// This variable is being allocated on the stack but does not have an expr associated with it
		// We need to generate a default initializer for it!
		if(var->initalExpression == nullptr) {
			if (var->type->llvmType->isIntegerTy()) {
				var->initalExpression = CreateIntegerLiteral(0);
			} else if (var->type->llvmType->isFloatingPointTy()) {
				var->initalExpression = CreateFloatLiteral(0);
			}
		}

		if (var->initalExpression != nullptr) {
			auto value = Codegen(var->initalExpression, context);
			builder->CreateStore(value, var->allocaInst);
			var->initalExpression = nullptr;
		}
		return var->allocaInst;
	}

	// This variable has been allocated on the stack allready or was newly allocated
	if (var->initalExpression != nullptr) {
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
	//HACK to skip function codegen if the function has allready been resolved
	if (function->code != nullptr) return nullptr; // We never should have to return anything with these statements
	auto builder = context.builder;
	LOG_VERBOSE("Codgen Function");
	std::vector<llvm::Type*> args(function->args.size());
	for (auto i = 0; i < args.size(); i++) {
		auto& arg = function->args[i];
		auto type = arg->type->llvmType;
		if (arg->isPointer) type = llvm::PointerType::get(type, 0);
		args[i] = type;
	}

	// Create the llvm function
	llvm::FunctionType* funcType = llvm::FunctionType::get(function->returnType->llvmType, args, false);
	llvm::Function::LinkageTypes linkage = (function->members.size() == 0) ? llvm::Function::ExternalLinkage : llvm::Function::ExternalLinkage;
	llvm::Function* llvmFunc = llvm::Function::Create(funcType, linkage, function->ident->name, context.currentPackage->module);

	//TODO arguments are created even if the function has no members!
	if(function->members.size() > 0) {
		llvm::BasicBlock* block = llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", llvmFunc);
		builder->SetInsertPoint(block);
	}

	// Create the allocas for our arguments!
	U32 i = 0;
	for(auto iter = llvmFunc->arg_begin(); i != args.size(); iter++, i++){
		auto& name = function->args[i]->identifier->name;
		iter->setName(name);
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

	if(!returnInstructionSeen && function->members.size() > 0) {
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

	if (!call->function->code) {
		auto lastInsertBlock = builder->GetInsertBlock();
		Codegen((ASTFunction*)call->function, context);
		builder->SetInsertPoint(lastInsertBlock);
	}
	auto llvmfunc = call->function->code;

	std::vector<llvm::Value*> argsV;
	auto argList = (ASTExpression**)(call + 1);
	for (U32 i = 0, e = llvmfunc->arg_size(); i != e; i++) {
		auto arg = argList[i];
		auto argV = Codegen(arg, context);

		if (argV == nullptr) {
			LOG_DEBUG("Failed to emit code for call argument!");
			return nullptr;
		} else {
		  if (arg->nodeType == AST_STRING_LITERAL) {
			auto zeroVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0, true);
			std::vector<llvm::Value*> indices;
			indices.push_back(zeroVal);
			indices.push_back(zeroVal); // This is insane there has to be a better way
			argV = llvm::GetElementPtrInst::Create(argV, indices, "str", builder->GetInsertBlock());
		  }
		  argsV.push_back(argV);
		}
	}

	if(call->function->returnType != global_voidType) {
		return builder->CreateCall(llvmfunc, argsV, "calltmp");
	} else {
		return builder->CreateCall(llvmfunc, argsV);
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

 void Codegen(ASTIter* iter, const BuildContext& context) {
	auto builder = context.builder;
	auto var = (ASTVariable*)iter->varIdent->node;

	// Set the inital expr of the variable to the start node of the iter
	// This should have allready been done inthe parsing phase but we will allow it for now
	var->initalExpression = iter->start;
	Codegen(var, context); // This creates the variable with an alloca inst.
	// Also does a store from the inital expr to the alloca

	// We emit some code for the value of the end expression
	// AKA the end integer
	auto endValue = Codegen(iter->end, context);

	auto parentBlock = builder->GetInsertBlock()->getParent();

	// We create a new basic block where we will emit our loop body into
	// We then create a branch from the currentBlock into the loopBlock
	// and move the insertion point of the builder to the loopblock
	// We create the two blocks that code will be emitted into
	auto loopBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "loop", parentBlock);
	auto exitBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "loopexit", parentBlock);

	builder->CreateBr(loopBlock);
	builder->SetInsertPoint(loopBlock);

	// Emit the iter body
	for (auto node : iter->body->members) {
		Codegen(node, context);
	}

	//We now create a instruction to increment the iterator
	auto stepValue = llvm::ConstantInt::get(llvm::IntegerType::getInt32Ty(llvm::getGlobalContext()), 1);
	auto currentValue = builder->CreateLoad(var->allocaInst);
	auto nextValue = builder->CreateAdd(currentValue, stepValue, "increment");
	builder->CreateStore(nextValue, var->allocaInst);

	auto condLoad = builder->CreateLoad(var->allocaInst);
	auto loopCond = builder->CreateICmpSLE(condLoad, endValue, "loopcond");	// I dont think this will work properly we need to do another load

	builder->CreateCondBr(loopCond, loopBlock, exitBlock);
	builder->SetInsertPoint(exitBlock);	//Any codegenerated after this loop must be emitted into the exit block
}

llvm::Value* Codegen (ASTMemberAccess* access, const BuildContext& context) {
	auto builder = context.builder;
	auto structAlloca = access->structVar->allocaInst;

	std::vector<llvm::Value*> indices;
	auto arrayIndex = llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0, true);
	indices.push_back(arrayIndex);	// Array indices allways are 0 for now because we dont have array support!
	for (auto& memberIndex : access->memberIndices) {
		auto indexValue = llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), memberIndex, true);
		indices.push_back(indexValue);
	}

	llvm::Value* value_ptr = structAlloca;
	if (access->structVar->isPointer) value_ptr = builder->CreateLoad(structAlloca);
	auto gep = llvm::GetElementPtrInst::Create(value_ptr, indices, "access", builder->GetInsertBlock());

	switch(access->mode) {
	case ACCESS_ASSIGN:
		auto expr = Codegen(access->expr, context);
		builder->CreateStore(expr, gep);
		return nullptr;	// If this is a store operation we dont consider this an expression and the
		//Nullptr value should be thrown out *hopefully*
	}

	return gep;
}

llvm::Value* Codegen(ASTMemberExpr* expr, const BuildContext& context) {
	auto builder = context.builder;
	auto structAlloca = expr->structVar->allocaInst;

	std::vector<llvm::Value*> indices;
	auto arrayIndex = llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0, true);
	indices.push_back(arrayIndex);	// Array indices allways are 0 for now because we dont have array support!

	for(auto& memberIndex : expr->memberIndices) {
		auto indexValue = llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), memberIndex, true);
		indices.push_back(indexValue);
	}

	llvm::Value* value_ptr = structAlloca;
	if(expr->structVar->isPointer) value_ptr = builder->CreateLoad(structAlloca);
	auto gep = llvm::GetElementPtrInst::Create(value_ptr, indices, "access", builder->GetInsertBlock());
	auto load = builder->CreateLoad(gep);
	return load;
}

llvm::Value* Codegen (ASTVarExpr* expr, const BuildContext& context) {
	auto builder = context.builder;
	auto varAlloca = expr->var->allocaInst;
	llvm::Value* value = nullptr;

	switch(expr->accessMode) {
	case EXPR_LOAD:
		value = builder->CreateLoad(varAlloca);
		break;
	case EXPR_POINTER:
		value = varAlloca;
		break;
	case EXPR_DEREF:
		value = builder->CreateLoad(varAlloca);
		break;
	}
	return value;
}

llvm::Value* Codegen (ASTIntegerLiteral* intNode, const BuildContext& context) {
	return llvm::ConstantInt::get(intNode->type->llvmType, intNode->value);
}
llvm::Value* Codegen (ASTFloatLiteral* floatNode, const BuildContext& context) {
	return llvm::ConstantFP::get(floatNode->type->llvmType, floatNode->value);
}

// For now we are not going to give a shit weather or not string literals
// are duplicated, also it may be benifical to push them on the stack rather than
// creating them as global constants but for now i will do what llvm does.
llvm::Value* Codegen (ASTStringLiteral* str, const BuildContext& context) {
}
