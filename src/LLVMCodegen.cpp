#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/raw_os_ostream.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/PluginLoader.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/ToolOutputFile.h"

#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetSubtargetInfo.h"

#include "llvm/Bitcode/ReaderWriter.h"

#include "llvm/CodeGen/CommandFlags.h"
#include "llvm/CodeGen/LinkAllAsmWriterComponents.h"
#include "llvm/CodeGen/LinkAllCodegenComponents.h"

#include "llvm/IRReader/IRReader.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/CodeGen/MIRParser/MIRParser.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/PassManager.h"

#include "AST.hpp"
#include "Analysis.hpp"
#include "Build.hpp"

static llvm::LLVMContext llvmContext;
static llvm::IRBuilder<> *builder;
static llvm::Module *global_module;

static void CodegenStatement (ASTNode* node);
static void Codegen (ASTFunction* function, llvm::Module* module);
static void Codegen (ASTStruct* structDefn);
static void Codegen (ASTVariable* var);
static void Codegen (ASTVariableOperation* varOp);
static void Codegen (ASTMemberOperation* memberOp);
static void Codegen (ASTReturn* retVal);
static inline void Codegen(ASTIfStatement* ifStatment, llvm::BasicBlock* mergeBlock, llvm::Function* function);
static inline void Codegen(ASTIter* iter);

llvm::Value* CodegenExpr (ASTNode* expr);
static llvm::Value* Codegen (ASTIntegerLiteral* intLiteral);
static llvm::Value* Codegen (ASTFloatLiteral* floatLiteral);
static llvm::Value* Codegen (ASTBinaryOperation* binop);
static llvm::Value* Codegen (ASTMemberExpr* expr);
static llvm::Value* Codegen (ASTVarExpr* expr);
static llvm::Value* Codegen (ASTStringLiteral* str);
static llvm::Value* Codegen (ASTCall* call);
static llvm::Value* Codegen (ASTCast* cast);

static int WriteIR (llvm::Module* module, BuildSettings* settings);
static int WriteNativeObject (llvm::Module* module, BuildSettings* settings);
static int WriteExecutable (BuildSettings* settings);

void Codegen(Compiler *compiler) {
  global_voidType->llvmType = llvm::Type::getVoidTy(g_llvmContext);

	global_U8Type->llvmType = llvm::IntegerType::get(g_llvmContext, 8);
	global_U16Type->llvmType = llvm::IntegerType::get(g_llvmContext, 16);
	global_U32Type->llvmType = llvm::IntegerType::get(g_llvmContext, 32);
	global_U64Type->llvmType = llvm::IntegerType::get(g_llvmContext, 64);

	global_S8Type->llvmType = llvm::Type::getInt8Ty(g_llvmContext);
	global_S16Type->llvmType = llvm::Type::getInt16Ty(g_llvmContext);
	global_S32Type->llvmType = llvm::Type::getInt32Ty(g_llvmContext);
	global_S64Type->llvmType = llvm::Type::getInt64Ty(g_llvmContext);

	global_F16Type->llvmType =	llvm::Type::getHalfTy(g_llvmContext);
	global_F32Type->llvmType =	llvm::Type::getFloatTy(g_llvmContext);
	global_F64Type->llvmType =	llvm::Type::getDoubleTy(g_llvmContext);
	global_F128Type->llvmType = llvm::Type::getFP128Ty(g_llvmContext);

  global_module = new llvm::Module("Compiler00", g_llvmContext);
  builder = new llvm::IRBuilder<>();

	// HACK codegenerate the structs rootBlock
	for (ASTNode* node : compiler->globalBlock.members) {
		if (node->nodeType == AST_STRUCT) {
			Codegen((ASTStruct*)node);
		}
	}

  for (ASTNode* node : compiler->globalBlock.members) {
		switch (node->nodeType) {
			case AST_FUNCTION:
				Codegen((ASTFunction*)node, global_module);
				break;
			case AST_STRUCT:
				Codegen((ASTStruct*)node);
				break;
			default:
				assert(!"A node was in the global scope that is not a function or a struct defn");
				break;
		}
	}

	if (settings->outputFile == "") {
		auto inputBase = settings->inputFile.substr(0, settings->inputFile.find(".") + 1);
		settings->outputFile = settings->rootDir + inputBase + "o";
	}

	if (settings->emitIR)
		WriteIR(global_module, settings);

	llvm::raw_os_ostream stream(std::cout);
	if (llvm::verifyModule(*global_module, &stream)) {
		std::cout << "\n";
		LOG_ERROR("llvm::Module verification failed!");
		LOG_ERROR("Build incomplete!	Skipping executable creation");
	} else {
		if (settings->emitNativeOBJ)
			WriteNativeObject(global_module, settings);
		if (settings->emitExecutable)
			WriteExecutable(settings);
		LOG_INFO("Sucuessfuly created executable there were 0 errors");
	}
}

void Codegen (ASTStruct* structDefn) {
	std::vector<llvm::Type*> memberTypes;
	for (U32 i = 0; i < structDefn->memberCount; i++) {
		auto& type = structDefn->members[i].type;
		if (type->llvmType == nullptr) {
			assert(type->nodeType = AST_STRUCT);
			Codegen((ASTStruct*)type);
		}

		auto llvmType = structDefn->members[i].isPointer ? llvm::PointerType::get((llvm::Type*)type->llvmType, 0) : (llvm::Type*)type->llvmType;
		memberTypes.push_back(llvmType);
	}
	auto& structName = structDefn->name;
	structDefn->llvmType = llvm::StructType::create(memberTypes, structName);
}

static inline llvm::Type* GetIndirectionType(ASTVariable* variable) {
	auto llvmType = (llvm::Type*)variable->type->llvmType;
	for (auto i = 0; i < variable->indirectionLevel; i++)
		llvmType = llvm::PointerType::get(llvmType, 0);
	return llvmType;
}

void Codegen (ASTFunction* function, llvm::Module* module) {
	// HACK to skip function codegen if the function has allready been resolved
	if (function->llvmFunction != nullptr) return;

	std::vector<llvm::Type*> argllvmTypes(function->args.size());
	for (U32 i = 0; i < argllvmTypes.size(); i++) {
		auto& arg = function->args[i];
		auto llvmType = GetIndirectionType(arg);
		argllvmTypes[i] = llvmType;
	}

  //TODO HACK Better handling of uppercase main function! We should be able to call it whatever
  llvm::FunctionType* funcType = llvm::FunctionType::get((llvm::Type*)function->returnType->llvmType, argllvmTypes, function->isVarArgs);
	llvm::Function::LinkageTypes linkage = (function->members.size() == 0) ? llvm::Function::ExternalLinkage : llvm::Function::ExternalLinkage;
  llvm::Function* llvmFunc = llvm::Function::Create(funcType, linkage, strcmp(function->name, "Main") ? function->name : std::string("main"), global_module); 
  
	//If the function has zero members thats how we determine that it was declared FORIGEN
	//A normal function with no members will be a parsing error
	auto lastInsertBlock = builder->GetInsertBlock();
	if (function->members.size() > 0) {
		llvm::BasicBlock* block = llvm::BasicBlock::Create(g_llvmContext, "entry", llvmFunc);
		builder->SetInsertPoint(block);
	}

	U32 i = 0;
	for (auto iter = llvmFunc->arg_begin(); i != argllvmTypes.size(); iter++, i++) {
		auto name = (const char*)(function->args[i] + 1);
		iter->setName(name);
		if(function->members.size() > 0) {
			function->args[i]->allocaInst = builder->CreateAlloca(iter->getType(), 0, name);
			//builder->CreateStore(, (llvm::AllocaInst*)function->args[i]->allocaInst);
		}
	}

	//The function must always do something...
	bool returnInstructionSeen = false;
	if (function->members.size() > 0) {
		for (U32 i = 0; i < function->members.size(); i++) {
			auto node = function->members[i];
			if (node->nodeType == AST_IF) {
				auto mergeBlock = llvm::BasicBlock::Create(g_llvmContext, "merge", llvmFunc);
				Codegen((ASTIfStatement*)node, mergeBlock, llvmFunc);
				builder->SetInsertPoint(mergeBlock);
				continue;
			}

			if (node->nodeType == AST_RETURN) {
				returnInstructionSeen = true;
			}
			CodegenStatement(node);
		}
	}

	if(!returnInstructionSeen && function->members.size() > 0) {
		if(function->returnType == global_voidType) {
			builder->CreateRetVoid();
		} else if (function->members.size() > 0){
			LOG_ERROR("Non-void functions must have a return statement!");
		}
	}

	// TODO sanity check to make sure this function was foreign if it did not have a body
	// Also do a sainy check to make sure that it has created return values for all flow paths

	function->llvmFunction = llvmFunc;
	if (lastInsertBlock != nullptr) {
		builder->SetInsertPoint(lastInsertBlock);
	} else {
		builder->ClearInsertionPoint();
	}
}

void CodegenStatement(ASTNode* node) {
	switch(node->nodeType) {
		case AST_VARIABLE: Codegen((ASTVariable*)node); break;
		case AST_MEMBER_OPERATION: Codegen((ASTMemberOperation*)node); break;
		case AST_VARIABLE_OPERATION: Codegen((ASTVariableOperation*)node); break;
		case AST_CALL: Codegen((ASTCall*)node); break;
		case AST_ITER: Codegen((ASTIter*)node); break;
		case AST_RETURN: Codegen((ASTReturn*)node); break;
		case AST_STRUCT: Codegen((ASTStruct*)node); break;
		case AST_FUNCTION: Codegen((ASTFunction*)node, global_module); break;
		default: assert(!"A top level node was not a statement"); break;
	}
}

static void Codegen (ASTVariable* var) {
	assert(var->allocaInst == nullptr);	// Variable codegens are variable decl statements
	assert(var->type != nullptr && "Variable must have type resolved during anaysis");
	
	auto llvmType = GetIndirectionType(var);
	var->allocaInst = builder->CreateAlloca(llvmType, 0, var->name);

	llvm::Value* expr = nullptr;
	if (var->initalExpression != nullptr) {
		expr = CodegenExpr(var->initalExpression);
	} else if (((llvm::Type*)(var->type->llvmType))->isIntegerTy()) {
		expr = llvm::ConstantInt::get(llvm::Type::getInt32Ty(g_llvmContext), 0);
	} else if (((llvm::Type*)(var->type->llvmType))->isFloatingPointTy()) {
		expr = llvm::ConstantFP::get(llvm::Type::getFloatTy(g_llvmContext), 0);
	} else if (var->type->nodeType == AST_STRUCT) {
		// TODO default values inside of structs
		return;
	}

	assert(expr != nullptr);
	builder->CreateStore(expr, (llvm::AllocaInst*)var->allocaInst);
}

static void Codegen (ASTVariableOperation* varOp) {
	assert((llvm::AllocaInst*)varOp->variable->allocaInst != nullptr);
	auto exprValue = CodegenExpr(varOp->expr);
	switch (varOp->operation) {
		case OPERATION_ASSIGN:
			builder->CreateStore(exprValue, (llvm::AllocaInst*)varOp->variable->allocaInst);
			break;
		case OPERATION_ADD: {
      auto alloca = (llvm::AllocaInst*)varOp->variable->allocaInst;
			auto load = builder->CreateLoad(alloca);
			auto operation = builder->CreateAdd(load, exprValue);
			builder->CreateStore(operation, alloca);
    } break;
		case OPERATION_SUB: {
      auto alloca = (llvm::AllocaInst*)varOp->variable->allocaInst;
			auto load = builder->CreateLoad(alloca);
			auto operation = builder->CreateSub(load, exprValue);
			builder->CreateStore(operation, alloca);
    } break;
		case OPERATION_MUL: {
		auto alloca = (llvm::AllocaInst*)varOp->variable->allocaInst;
		auto load = builder->CreateLoad(alloca);

		llvm::Value* operation;
		if (isFloatingPoint(varOp->expr->type)) {
			operation = builder->CreateFMul(load, exprValue);
		} else {
			operation = builder->CreateMul(load, exprValue);

		}

		builder->CreateStore(operation, alloca);

    } break;
		case OPERATION_DIV: {
      auto alloca = (llvm::AllocaInst*)varOp->variable->allocaInst;
      auto load = builder->CreateLoad(alloca);
			auto llvmType = (llvm::Type*)varOp->variable->type->llvmType;
			if (llvmType->isIntegerTy()) {
					auto operation = builder->CreateSDiv(load, exprValue);
					builder->CreateStore(operation, alloca);
			} else if (llvmType->isFloatingPointTy()) {
				auto operation = builder->CreateFDiv(load, exprValue);
				builder->CreateStore(operation, alloca);
			}
    } break;
	}


}

llvm::Value* CodegenExpr (ASTNode* node) {
	assert(node != nullptr);
	switch(node->nodeType) {
	case AST_BINARY_OPERATION: return Codegen((ASTBinaryOperation*)node);
	case AST_MEMBER_EXPR: return Codegen((ASTMemberExpr*)node);
	case AST_VAR_EXPR: return Codegen((ASTVarExpr*)node);
	case AST_CALL: return Codegen((ASTCall*)node);
	case AST_INTEGER_LITERAL: return Codegen((ASTIntegerLiteral*) node);
	case AST_FLOAT_LITERAL: return Codegen((ASTFloatLiteral*)node);
	case AST_STRING_LITERAL: return Codegen((ASTStringLiteral*)node);
    case AST_CAST: return Codegen((ASTCast*)node);
	default:
		assert(!"ASTNode is not an expression!");
		return nullptr;
	}
}

static llvm::Value* Codegen (ASTBinaryOperation* binop)	{
	llvm::Value* lhs = CodegenExpr(binop->lhs);
	llvm::Value* rhs = CodegenExpr(binop->rhs);
	assert(lhs && rhs);

    enum CmpBits {
        EQUAL       = 1 << 0, // 0 0 0 1
        GREATER     = 1 << 1, // 0 0 1 0
        LESS        = 1 << 2, // 0 1 0 0
        UNORDERED   = 1 << 3, // 1 0 0 0
        SIGNED      = 1 << 3, // 1 0 0 0
    };

    auto createCompare = [binop, lhs, rhs](U32 predicateBits) -> llvm::Value* {
        if (isFloatingPoint(binop->lhs->type)) {
            return builder->CreateFCmp((llvm::CmpInst::Predicate)predicateBits, lhs, rhs);
        } else if (isUnsignedInteger(binop->lhs->type)) {
            return builder->CreateICmp((llvm::CmpInst::Predicate)(predicateBits + 32), lhs, rhs);
        } else if (isSignedInteger(binop->lhs->type)) {
            return builder->CreateICmp((llvm::CmpInst::Predicate)(predicateBits + 36), lhs, rhs);
		} else {
			assert(false);
			return nullptr;
		}
    };

	auto createMul = [binop, lhs, rhs]() -> llvm::Value* {
		if (isFloatingPoint(binop->lhs->type)) {
			return builder->CreateFMul(lhs, rhs);
		} else {
			return builder->CreateMul(lhs, rhs);
		}
	};

	switch (binop->operation) {
		case OPERATION_ADD: return builder->CreateAdd(lhs, rhs, "addtmp");
		case OPERATION_SUB: return builder->CreateSub(lhs, rhs, "subtmp");
		case OPERATION_MUL: return createMul();
		case OPERATION_DIV: return builder->CreateSDiv(lhs, rhs, "divtmp");
        case OPERATION_GT:  return createCompare(GREATER);  // TODO we can setup the operations so that bitor does not even need  to happen here we can just pass the operation as a parameter
        case OPERATION_GTE: return createCompare(GREATER | EQUAL);
        case OPERATION_LT:  return createCompare(LESS);
        case OPERATION_LTE: return createCompare(LESS | EQUAL);
		default: assert(false && "Did not implement codegen for this binary operation"); return nullptr;
	}
}

static void Codegen (ASTReturn* retVal) {
	auto value = CodegenExpr(retVal->value);
	assert(value != nullptr);
	builder->CreateRet(value);
}

static llvm::Value* Codegen (ASTCall* call) {
	assert(call->function);	// A call should always have a function resolved when here
	if (!call->function->llvmFunction) {
		auto lastInsertBlock = builder->GetInsertBlock();
		Codegen((ASTFunction*)call->function, global_module);
		builder->SetInsertPoint(lastInsertBlock);
	}

	auto llvmfunc = (llvm::Function*)call->function->llvmFunction;

	std::vector<llvm::Value*> argsV;
	auto argList = call->args;
	for (U32 i = 0; i < call->argCount; i++) {
		auto arg = argList[i];
		auto arg_value = CodegenExpr(arg);
		assert(arg_value != nullptr);
        argsV.push_back(arg_value);
	}

	if (call->function->returnType != global_voidType) {
		return builder->CreateCall(llvmfunc, argsV, "calltmp");
	} else {
		return builder->CreateCall(llvmfunc, argsV);
	}
}

static inline void Codegen(ASTIfStatement* ifStatement, llvm::BasicBlock* mergeBlock, llvm::Function* function) {
	auto ifBlock = llvm::BasicBlock::Create(g_llvmContext, "if", function);
	auto elseBlock = mergeBlock;
	if (ifStatement->elseBody != nullptr) {
		elseBlock = llvm::BasicBlock::Create(g_llvmContext, "else", function);
	}

	auto exprValue = CodegenExpr(ifStatement->expr);
	assert(exprValue != nullptr);

	if (ifStatement->expr->nodeType != AST_BINARY_OPERATION) {
		auto zeroValue = llvm::ConstantInt::get(llvm::IntegerType::getInt32Ty(g_llvmContext), 0);
		auto cmp = builder->CreateICmpNE(exprValue, zeroValue, "ifcmp");
		builder->CreateCondBr(cmp, ifBlock, elseBlock);
	} else {
        builder->CreateCondBr(exprValue, ifBlock, elseBlock);
    }


	builder->SetInsertPoint(ifBlock);

	if (ifStatement->ifBody->nodeType == AST_BLOCK) {
		auto block = (ASTBlock*)ifStatement->ifBody;
		for(auto node : block->members) {
			if (node->nodeType == AST_IF) {
				ifBlock = llvm::BasicBlock::Create(g_llvmContext, "merge", function);
				Codegen((ASTIfStatement*)node, ifBlock, function);
				builder->SetInsertPoint(ifBlock);
				continue;
			}
			auto value = CodegenExpr(node);
			if(!value) {
				LOG_DEBUG("Failed to emit code for value in body of ifstatement!");
			}
		}
	} else {
		CodegenStatement(ifStatement->ifBody);
	}

	builder->CreateBr(mergeBlock);

	if(ifStatement->elseBody != nullptr) {
		builder->SetInsertPoint(elseBlock);
		if(ifStatement->elseBody->nodeType == AST_IF) {
			auto ifElse = (ASTIfStatement*)ifStatement->elseBody;
			auto condition = CodegenExpr(ifElse->expr);
			auto comp = builder->CreateICmpEQ(condition, llvm::ConstantInt::getTrue(g_llvmContext), "elseifcond");
			auto elseIfBlock = llvm::BasicBlock::Create(g_llvmContext, "elseif", function);
		}

		if (ifStatement->elseBody->nodeType == AST_BLOCK) {
			auto block = (ASTBlock*)ifStatement->elseBody;
			for(auto node : block->members) {
				if(node->nodeType == AST_IF) {
					elseBlock = llvm::BasicBlock::Create(g_llvmContext, "merge", function);
					Codegen((ASTIfStatement*)node, elseBlock, function);
					builder->SetInsertPoint(elseBlock);
					continue;
				}
				auto value = CodegenExpr(node);
				assert(value);
		}
	} else {
		CodegenStatement(ifStatement->elseBody);
	}

	builder->CreateBr(mergeBlock);
	}
}

 static inline void Codegen (ASTIter* iter) {
	auto var = iter->var;

	// Set the inital expr of the variable to the start node of the iter
	// This should have allready been done inthe parsing phase but we will allow it for now
	var->initalExpression = iter->start;
	Codegen(var); // This creates the variable with an alloca inst.
	// Also does a store from the inital expr to the alloca

	// We emit some code for the value of the end expression
	// AKA the end integer
	auto endValue = CodegenExpr(iter->end);
	auto stepValue = llvm::ConstantInt::get(llvm::IntegerType::getInt32Ty(g_llvmContext), 1);

	auto parentBlock = builder->GetInsertBlock()->getParent();
	auto loopBlock = llvm::BasicBlock::Create(g_llvmContext, "loop", parentBlock);
	auto exitBlock = llvm::BasicBlock::Create(g_llvmContext, "loopexit", parentBlock);

	builder->CreateBr(loopBlock);
	builder->SetInsertPoint(loopBlock);

	// Emit the body for the iterstatement
	for (auto node : iter->body->members) {
		CodegenStatement(node);
	}

	// Create an instruction to increment the iterator
	auto currentValue = builder->CreateLoad((llvm::AllocaInst*)var->allocaInst);
	auto nextValue = builder->CreateAdd(currentValue, stepValue, "increment");
	builder->CreateStore(nextValue, (llvm::AllocaInst*)var->allocaInst);

	auto condLoad = builder->CreateLoad((llvm::AllocaInst*)var->allocaInst);
	auto loopCond = builder->CreateICmpSLE(condLoad, endValue, "loopcond");

	builder->CreateCondBr(loopCond, loopBlock, exitBlock);
	builder->SetInsertPoint(exitBlock);
}


// TODO make sure that a memberOperation or a variable operation only has a single level of indirection
// or no indirection at all when doing an operation
static void Codegen(ASTMemberOperation* memberOp) {
	auto structAlloca = static_cast<llvm::AllocaInst*>(memberOp->structVar->allocaInst);
	auto structVar = memberOp->structVar;

	std::vector<llvm::Value*> indices;
	indices.reserve(memberOp->access.memberCount + 1);
	auto arrayIndex = llvm::ConstantInt::get(llvm::Type::getInt32Ty(g_llvmContext), 0, true);
	indices.emplace_back(arrayIndex);	// Array indices allways are 0 for now because we dont have array support!

	for (U32 i = 0; i < memberOp->access.memberCount; i++) {
		auto& memberIndex = memberOp->access.indices[i];
		auto indexValue = llvm::ConstantInt::get(llvm::Type::getInt32Ty(g_llvmContext), memberIndex, true);
		indices.emplace_back(indexValue);
	}


	llvm::Value* variableValue = structAlloca;
	auto elementPtr = builder->CreateGEP(variableValue, indices, "access");
	auto exprValue = CodegenExpr(memberOp->expr);
	assert(exprValue != nullptr);

	switch (memberOp->operation) {
	case OPERATION_ASSIGN:
		builder->CreateStore(exprValue, elementPtr);
		return;
	}

}


static llvm::Value* Codegen(ASTCast* cast) {
	auto exprValue = CodegenExpr(cast->expr);
	assert(exprValue);

	// TODO much much muach more roboust casting
	// Does not handle signed / unsigned mistmatch

	auto floatingPointCastDirection = [](ASTDefinition* castFrom, ASTDefinition* castTo) {
		if (castFrom == global_F32Type && castTo == global_F64Type) return  1;
		if (castFrom == global_F64Type && castTo == global_F32Type) return -1;
		return 0;
	};

	llvm::Instruction::CastOps castOp;



	if (isFloatingPoint(cast->expr->type)) {
		if (isFloatingPoint(cast->type)) {
			if (floatingPointCastDirection(cast->expr->type, cast->type) == 1) castOp = llvm::Instruction::CastOps::FPExt;
			else if (floatingPointCastDirection(cast->expr->type, cast->type) == -1) castOp = llvm::Instruction::CastOps::FPTrunc;
		} else {
			if (isSignedInteger(cast->type)) castOp = llvm::Instruction::CastOps::FPToSI;
			else if (isUnsignedInteger(cast->type)) castOp = llvm::Instruction::CastOps::FPToUI;
		}
	} 
	
	else if (isInteger(cast->expr->type)) {
		if (isFloatingPoint(cast->type)) {
			castOp = llvm::Instruction::CastOps::SIToFP;
		} else {
			return builder->CreateIntCast(exprValue, (llvm::Type*)cast->type->llvmType, isSignedInteger(cast->expr->type));
		}
	}

	auto castValue = builder->CreateCast(castOp, exprValue, (llvm::Type*)cast->type->llvmType);
	return castValue;
}


static inline llvm::LoadInst* GetLoadInstForIndirectionLevel(ASTVariable* variable) {
	llvm::Value* value_ptr = (llvm::Value*)variable->allocaInst;
	for (auto i = 0; i < variable->indirectionLevel; i++) {
		value_ptr = builder->CreateLoad(value_ptr);
	}

	return static_cast<llvm::LoadInst*>(value_ptr);
}


llvm::Value* Codegen (ASTMemberExpr* expr) {
	assert(expr->structVar->allocaInst);

	std::vector<llvm::Value*> indices;
	auto arrayIndex = llvm::ConstantInt::get(llvm::Type::getInt32Ty(g_llvmContext), 0, true);
	indices.emplace_back(arrayIndex);	// Array indices allways are 0 for now because we dont have array support!
	for (U32 i = 0; i < expr->access.memberCount; i++) {
		auto& memberIndex = expr->access.indices[i];
		auto indexValue = llvm::ConstantInt::get(llvm::Type::getInt32Ty(g_llvmContext), memberIndex, true);
		indices.emplace_back(indexValue);
	}

	llvm::Value* value_ptr = GetLoadInstForIndirectionLevel(expr->structVar);
	auto pointer_type = (llvm::Type*)expr->structVar->type->llvmType;


	auto gep = llvm::GetElementPtrInst::Create(pointer_type, value_ptr, indices, "access", builder->GetInsertBlock());
	return gep;
}

static inline ASTExpression* GetVariableExpressionFromUnaryOperation(ASTUnaryOp* unary) {
	auto result = unary->expr;
	while (result->nodeType == AST_UNARY_OPERATION) {
		result = GetVariableExpressionFromUnaryOperation(static_cast<ASTUnaryOp*>(result));
	}
	return result;
}

static inline llvm::Value* CodegenIndirectionUnaryOperation(ASTUnaryOp* unaryOperation, llvm::Value* exprValue) {
	auto indirectionLevel = unaryOperation->indirectionLevel;
	if (indirectionLevel == 0) {
		return builder->CreateLoad(exprValue);
	} else if (indirectionLevel == 1) {
		return exprValue;
	} else {
		auto result = exprValue;
		auto zeroValue = llvm::ConstantInt::get(llvm::IntegerType::getInt32Ty(g_llvmContext), 0);
		for (auto i = 2; i < indirectionLevel; i++) {
			result = builder->CreateGEP(result, zeroValue);
		}
		return result;
	}

	assert(false);
	return nullptr;
}

llvm::Value* Codegen(ASTUnaryOp* unaryOperation) {
	assert(unaryOperation->expr != nullptr);
	auto exprValue = CodegenExpr(unaryOperation->expr);

	switch (unaryOperation->expr->nodeType) {
	case UNARY_INDIRECTION:
		return CodegenIndirectionUnaryOperation(unaryOperation, exprValue);
	case UNARY_NOT:
		auto load = builder->CreateLoad(exprValue);
		auto zeroValue = llvm::ConstantInt::get(llvm::IntegerType::getInt32Ty(g_llvmContext), 0);
		auto cmp = builder->CreateICmpEQ(load, zeroValue);
		exprValue = builder->CreateZExt(cmp, llvm::IntegerType::getInt32Ty(g_llvmContext));
		return exprValue;
	}

	assert(false);
	return nullptr;
}

llvm::Value* Codegen (ASTVarExpr* expr) {
	assert(expr->var->allocaInst);
	auto varAlloca = (llvm::AllocaInst*)expr->var->allocaInst;
	return varAlloca;
}

llvm::Value* Codegen (ASTIntegerLiteral* intNode) {
	return llvm::ConstantInt::get((llvm::Type*)intNode->type->llvmType, intNode->value);
}

llvm::Value* Codegen (ASTFloatLiteral* floatNode) {
	return llvm::ConstantFP::get((llvm::Type*)floatNode->type->llvmType, floatNode->value);
}

llvm::Value* Codegen (ASTStringLiteral* str) {
	auto str_value = builder->CreateGlobalStringPtr((char*)str->value, "str");
	return str_value;
}


int WriteNativeObject(llvm::Module* module, BuildSettings* settings) {
	llvm::InitializeAllTargets();
	llvm::InitializeAllTargetMCs();
	llvm::InitializeAllAsmPrinters();
	llvm::InitializeAllAsmParsers();

	llvm::PassRegistry* registry = llvm::PassRegistry::getPassRegistry();
	llvm::initializeCore(*registry);
	llvm::initializeCodeGen(*registry);
	llvm::initializeLowerIntrinsicsPass(*registry);
	llvm::initializeLoopStrengthReducePass(*registry);
	//llvm::initializeUnreachableBlockElimPass(*registry);

	llvm::cl::AddExtraVersionPrinter(llvm::TargetRegistry::printRegisteredTargetsForVersion);

	// Load the module to be compiled...
	llvm::SMDiagnostic err;
	if (MCPU == "native") MCPU = sys::getHostCPUName();
	llvm::Triple triple;
	triple.setTriple(sys::getDefaultTargetTriple());
	triple.setArch(llvm::Triple::x86_64);
	

	// Get the target specific parser.
	std::string errorString;
	const Target *TheTarget = TargetRegistry::lookupTarget(MArch, triple, errorString);
	if (!TheTarget) {
		return 1;
	}
	// Package up features to be passed to target/subtarget
	std::string featuresStr;
	if (MAttrs.size()) {
		SubtargetFeatures Features;
		for (unsigned i = 0; i != MAttrs.size(); ++i)
			Features.AddFeature(MAttrs[i]);
		featuresStr = Features.getString();
	}

	//TODO add optimization levels
	CodeGenOpt::Level OLvl = CodeGenOpt::None;
//	switch (OptLevel) {
//		default:
//			errs() << argv[0] << ": invalid optimization level.\n";
//			return 1;
//		case ' ': break;
//		case '0': OLvl = CodeGenOpt::None; break;
//		case '1': OLvl = CodeGenOpt::Less; break;
//		case '2': OLvl = CodeGenOpt::Default; break;
//		case '3': OLvl = CodeGenOpt::Aggressive; break;
//	}

	llvm::TargetOptions Options = InitTargetOptionsFromCodeGenFlags();
	//TODO options are defaulted for now...
//	Options.DisableIntegratedAS = NoIntegratedAssembler;
//	Options.MCOptions.ShowMCEncoding = ShowMCEncoding;
//	Options.MCOptions.MCUseDwarfDirectory = EnableDwarfDirectory;
//	Options.MCOptions.AsmVerbose = AsmVerbose;

	std::unique_ptr<TargetMachine> Target(TheTarget->createTargetMachine(triple.getTriple(), MCPU, featuresStr, Options, getRelocModel(), CMModel, OLvl));
	assert(Target && "Could not allocate target machine!");

	// TODO add options for viewing asm as text!
	bool binary = true;
	sys::fs::OpenFlags openFlags = sys::fs::F_None;
	if (!binary) {
		openFlags |= sys::fs::F_Text;
	}

	std::error_code errorCode;
	auto fileOut = std::make_unique<tool_output_file>(settings->outputFile, errorCode, openFlags);
	if (errorCode) {
		LOG_ERROR(errorCode.message());
		return -1;
	}


	// Build up all of the passes that we want to do to the module.
	module->setDataLayout(Target->createDataLayout());
	llvm::legacy::PassManager PM;

//	// Add an appropriate TargetLibraryInfo pass for the module's triple.
//	llvm::TargetLibraryInfo *TLI = new llvm::TargetLibraryInfo(triple);
////	if (DisableSimplifyLibCalls)
////		TLI->disableAllFunctions();
//	PM.add(TLI);


	{
		formatted_raw_ostream FOS(fileOut->os());

		AnalysisID StartAfterID = nullptr;
		AnalysisID StopAfterID = nullptr;
		const PassRegistry *PR = PassRegistry::getPassRegistry();
		if (!StartAfter.empty()) {
			const PassInfo *PI = PR->getPassInfo(StartAfter);
			if (!PI) {
				LOG_ERROR("start-after pass is not registered");
				return 1;
			}
			StartAfterID = PI->getTypeInfo();
		}
		if (!StopAfter.empty()) {
			const PassInfo *PI = PR->getPassInfo(StopAfter);
			if (!PI) {
				LOG_ERROR("stop-after pass is not registered.\n");
				return 1;
			}
			StopAfterID = PI->getTypeInfo();
		}

		// Ask the target to add backend passes as necessary.
		//Verification is on for now
		// NOTE we may want to remove it since we are allready verifiying at Codegen
		auto fileType = TargetMachine::CGFT_ObjectFile;
		if (Target->addPassesToEmitFile(PM, fileOut->os(), fileType, true, StartAfterID, StopAfterID)) {
			LOG_ERROR("target does not support generation of this filetype");
			return 1;
		}

		// Before executing passes, print the final values of the LLVM options.
		cl::PrintOptionValues();

		PM.run(*module);
	}

	// Declare success.
	fileOut->keep();	// NOTE What ass-fuckery is this?
	return 0;
}

int WriteIR(llvm::Module* module, BuildSettings* settings) {
	std::ofstream filestream;
	auto filename = settings->rootDir + settings->packageName + ".ll";
	filestream.open(filename);
	if (!filestream.is_open()) {
		LOG_ERROR("Could not open file: " << filename);
	}
	llvm::raw_os_ostream ostream(filestream);
	module->print(ostream, nullptr);
	return 0;
}

int WriteExecutable(BuildSettings* settings) {
	std::string allLibs;
	
	//for (auto& dir : settings->libDirs)
	//	allLibs.append("-L" + settings->rootDir + dir + " ");
	//for (auto& lib : settings->libNames)
	//	allLibs.append("-l" + lib + " ");

#ifndef _WIN32
	std::string cmd = "clang++ " + settings->outputFile + " " + allLibs + " -o " + settings->rootDir + "app";
#else
	std::string cmd = "clang++ " + settings->outputFile + " " + allLibs + " -o " + settings->rootDir + "app.exe";
#endif
	LOG_INFO("Writing Executable: " << cmd);
	system(cmd.c_str());
	return 0;
}
