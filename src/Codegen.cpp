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
#include "llvm/Target/TargetLibraryInfo.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"

#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetSubtargetInfo.h"

#include "llvm/Bitcode/ReaderWriter.h"

#include "llvm/CodeGen/CommandFlags.h"
#include "llvm/CodeGen/LinkAllAsmWriterComponents.h"
#include "llvm/CodeGen/LinkAllCodegenComponents.h"

#include "llvm/PassManager.h"

#include "llvm/IRReader/IRReader.h"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Module.h"

#include "AST.hpp"
#include "Build.hpp"
#include "Common.hpp"

global_variable llvm::IRBuilder<>* builder = new llvm::IRBuilder<>(llvm::getGlobalContext());
global_variable llvm::Module* global_module;	// HACK HACK HACK HACK

internal int WriteIR (llvm::Module* module, BuildSettings* settings);
internal int WriteNativeObject (llvm::Module* module, BuildSettings* settings);
internal int WriteExecutable (BuildSettings* settings);

// Top level statements
// Eventualy we might consider allowing structs and functions to be declared localy at block level
internal void CodegenPrimitiveTypes();
void Codegen(ASTStruct* structDefn);
void Codegen(ASTFunction* function, llvm::Module* module);



// Statements
void CodegenStatement(ASTNode* node);
void Codegen(ASTVariable* var);
void Codegen(ASTVariableOperation* varOp);
void Codegen(ASTMemberOperation* memberOp);
inline internal void Codegen(ASTIfStatement* ifStatment, llvm::BasicBlock* mergeBlock, llvm::Function* function);
inline internal void Codegen(ASTIter* iter);

internal void Codegen(ASTReturn* retVal);

// Expressions
llvm::Value* CodegenExpr (ASTNode* expr);
llvm::Value* Codegen (ASTIntegerLiteral* intLiteral);
llvm::Value* Codegen (ASTFloatLiteral* floatLiteral);
static llvm::Value* Codegen (ASTBinaryOperation* binop);
llvm::Value* Codegen (ASTMemberExpr* expr);
llvm::Value* Codegen (ASTVarExpr* expr);
llvm::Value* Codegen (ASTStringLiteral* str);
llvm::Value* Codegen (ASTCall* call);

internal void CodegenPrimitiveTypes() {
	global_voidType->llvmType = llvm::Type::getVoidTy(llvm::getGlobalContext());

	global_U8Type->llvmType = llvm::IntegerType::get(llvm::getGlobalContext(), 8);
	global_U16Type->llvmType = llvm::IntegerType::get(llvm::getGlobalContext(), 16);
	global_U32Type->llvmType = llvm::IntegerType::get(llvm::getGlobalContext(), 32);
	global_U64Type->llvmType = llvm::IntegerType::get(llvm::getGlobalContext(), 64);

	global_S8Type->llvmType = llvm::Type::getInt8Ty(llvm::getGlobalContext());
	global_S16Type->llvmType = llvm::Type::getInt16Ty(llvm::getGlobalContext());
	global_S32Type->llvmType = llvm::Type::getInt32Ty(llvm::getGlobalContext());
	global_S64Type->llvmType = llvm::Type::getInt64Ty(llvm::getGlobalContext());

	global_F16Type->llvmType =	llvm::Type::getHalfTy(llvm::getGlobalContext());
	global_F32Type->llvmType =	llvm::Type::getFloatTy(llvm::getGlobalContext());
	global_F64Type->llvmType =	llvm::Type::getDoubleTy(llvm::getGlobalContext());
	global_F128Type->llvmType = llvm::Type::getFP128Ty(llvm::getGlobalContext());
}

void CodegenPackage (Package* package, BuildSettings* settings) {
	CodegenPrimitiveTypes();
	global_module = new llvm::Module("BangCompiler", llvm::getGlobalContext());

 	for (ASTNode* node : package->globalScope.members) {
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

void Codegen(ASTStruct* structDefn) {
	std::vector<llvm::Type*> memberTypes;
	for (auto i = 0; i < structDefn->memberCount; i++) {
		auto& type = structDefn->members[i].type;
		auto llvmType = structDefn->members[i].isPointer ? llvm::PointerType::get((llvm::Type*)type->llvmType, 0) : (llvm::Type*)type->llvmType;
		memberTypes.push_back(llvmType);
	}
	auto& structName = structDefn->identifier->name;
	structDefn->llvmType = llvm::StructType::create(memberTypes, structName);
}

void Codegen(ASTFunction* function, llvm::Module* module) {
	// HACK to skip function codegen if the function has allready been resolved
	if (function->llvmFunction != nullptr) return;

	std::vector<llvm::Type*> args(function->args.size());
	for (auto i = 0; i < args.size(); i++) {
		auto& arg = function->args[i];
		auto type = (llvm::Type*)arg->type->llvmType;
		if (arg->isPointer) type = llvm::PointerType::get(type, 0);
		args[i] = type;
	}

	// Create the llvm function
	llvm::FunctionType* funcType = llvm::FunctionType::get((llvm::Type*)function->returnType->llvmType, args, function->isVarArgs);
	llvm::Function::LinkageTypes linkage = (function->members.size() == 0) ? llvm::Function::ExternalLinkage : llvm::Function::ExternalLinkage;
	llvm::Function* llvmFunc = llvm::Function::Create(funcType, linkage, function->ident->name, global_module);

	// TODO arguments are created even if the function has no members!
	if (function->members.size() > 0) {
		llvm::BasicBlock* block = llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", llvmFunc);
		builder->SetInsertPoint(block);
	}

	// Create the allocas for our arguments!
	U32 i = 0;
	for (auto iter = llvmFunc->arg_begin(); i != args.size(); iter++, i++) {
		auto name = (const char*)(function->args[i] + 1);
		iter->setName(name);
		if(function->members.size() > 0) {
			function->args[i]->allocaInst = builder->CreateAlloca(iter->getType(), 0, name);
			builder->CreateStore(iter, (llvm::AllocaInst*)function->args[i]->allocaInst);
		}
	}

	//The function must always do something...
	bool returnInstructionSeen = false;
	if (function->members.size() > 0) {
		for (U32 i = 0; i < function->members.size(); i++) {
			auto node = function->members[i];
			if (node->nodeType == AST_IF) {
				auto mergeBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "merge", llvmFunc);
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
}

void CodegenStatement (ASTNode* node) {
	switch(node->nodeType) {
		case AST_VARIABLE: Codegen((ASTVariable*)node); break;
		case AST_MEMBER_OPERATION: Codegen((ASTMemberOperation*)node); break;
		case AST_VARIABLE_OPERATION: Codegen((ASTVariableOperation*)node); break;
		case AST_CALL: Codegen((ASTCall*)node); break;
		case AST_ITER: Codegen((ASTIter*)node); break;
		case AST_RETURN: Codegen((ASTReturn*)node); break;
		default: assert(!"A top level node was not a statement"); break;
	}
}

void Codegen(ASTVariable* var) {
	assert(var->allocaInst == nullptr);	// Variable codegens are variable decl statements
	assert(var->type != nullptr && "Variable must have type resolved during anaysis");
	auto llvmType = (llvm::Type*)var->type->llvmType;
	if (var->isPointer) llvmType = llvm::PointerType::get(llvmType, 0);
	auto name = (const char*)(var + 1);
	var->allocaInst = builder->CreateAlloca(llvmType, 0, name);

	// TODO This is silly and should not be determined at this phase of the compiler state... maybe???
	llvm::Value* expr = nullptr;
	if (var->initalExpression != nullptr) {
		expr = CodegenExpr(var->initalExpression);
	} else if (((llvm::Type*)(var->type->llvmType))->isIntegerTy()) {
		expr = llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0);
	} else if (((llvm::Type*)(var->type->llvmType))->isFloatingPointTy()) {
		expr = llvm::ConstantFP::get(llvm::Type::getFloatTy(llvm::getGlobalContext()), 0);
	} else if (var->type->nodeType == AST_STRUCT) {
		// TODO default values inside of structs
		return;
	}

	assert(expr != nullptr);
	builder->CreateStore(expr, (llvm::AllocaInst*)var->allocaInst);
}

void Codegen(ASTVariableOperation* varOp) {
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
      auto operation = builder->CreateMul(load, exprValue);
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

llvm::Value* CodegenExpr(ASTNode* node) {
	assert(node != nullptr);
	switch(node->nodeType) {
	case AST_BINOP:
		return Codegen((ASTBinaryOperation*)node);
	case AST_MEMBER_EXPR:
		return Codegen((ASTMemberExpr*)node);
	case AST_VAR_EXPR:
		return Codegen((ASTVarExpr*)node);
	case AST_CALL:
		return Codegen((ASTCall*)node);
	case AST_INTEGER_LITERAL:
		return Codegen((ASTIntegerLiteral*) node);
	case AST_FLOAT_LITERAL:
		return Codegen((ASTFloatLiteral*)node);
	case AST_STRING_LITERAL:
		return Codegen((ASTStringLiteral*)node);
	default:
		assert(!"ASTNode is not an expression!");
		return nullptr;
	}
}

static llvm::Value* Codegen (ASTBinaryOperation* binop)	{
	llvm::Value* lhs = CodegenExpr(binop->lhs);
	llvm::Value* rhs = CodegenExpr(binop->rhs);
	assert(lhs && rhs);

	switch (binop->binop) {
		case TOKEN_ADD: return builder->CreateAdd(lhs, rhs, "addtmp");
		case TOKEN_SUB: return builder->CreateSub(lhs, rhs, "subtmp");
		case TOKEN_MUL: return builder->CreateMul(lhs, rhs, "multmp");
		case TOKEN_DIV: return builder->CreateSDiv(lhs, rhs, "divtmp");
		default: assert(false); return nullptr;
	}
}

static void Codegen (ASTReturn* retVal) {
	auto value = CodegenExpr(retVal->value);
	assert(value != nullptr);
	builder->CreateRet(value);
}

llvm::Value* Codegen(ASTCall* call) {
	assert(call->function);	// A call should always have a function resolved when here
	if (!call->function->llvmFunction) {
		auto lastInsertBlock = builder->GetInsertBlock();
		Codegen((ASTFunction*)call->function, global_module);
		builder->SetInsertPoint(lastInsertBlock);
	}

	auto llvmfunc = (llvm::Function*)call->function->llvmFunction;

	std::vector<llvm::Value*> argsV;
	auto argList = (ASTExpression**)(call + 1);
	for (auto i = 0; i < call->argCount; i++) {
		auto arg = argList[i];
		auto arg_value = CodegenExpr(arg);
		assert(arg_value != nullptr);
        argsV.push_back(arg_value);
	}

	if(call->function->returnType != global_voidType) {
		return builder->CreateCall(llvmfunc, argsV, "calltmp");
	} else {
		return builder->CreateCall(llvmfunc, argsV);
	}
}

static inline void Codegen (ASTIfStatement* ifStatement, llvm::BasicBlock* mergeBlock, llvm::Function* function) {
	auto ifBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "if", function);
	auto elseBlock = mergeBlock;
	if (ifStatement->elseBody != nullptr) {
		elseBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "else", function);
	}

	auto exprValue = CodegenExpr(ifStatement->expr);
	assert(exprValue != nullptr);

	if (ifStatement->expr->nodeType != AST_BINOP) {
		auto zeroValue = llvm::ConstantInt::get(llvm::IntegerType::getInt32Ty(llvm::getGlobalContext()), 0);
		auto cmp = builder->CreateICmpNE(exprValue, zeroValue, "ifcmp");
		builder->CreateCondBr(cmp, ifBlock, elseBlock);
	}


	builder->SetInsertPoint(ifBlock);

	if (ifStatement->ifBody->nodeType == AST_BLOCK) {
		auto block = (ASTBlock*)ifStatement->ifBody;
		for(auto node : block->members) {
			if (node->nodeType == AST_IF) {
				ifBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "merge", function);
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
			auto comp = builder->CreateICmpEQ(condition, llvm::ConstantInt::getTrue(llvm::getGlobalContext()), "elseifcond");
			auto elseIfBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "elseif", function);
		}

		if (ifStatement->elseBody->nodeType == AST_BLOCK) {
			auto block = (ASTBlock*)ifStatement->elseBody;
			for(auto node : block->members) {
				if(node->nodeType == AST_IF) {
					elseBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "merge", function);
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
	auto stepValue = llvm::ConstantInt::get(llvm::IntegerType::getInt32Ty(llvm::getGlobalContext()), 1);

	auto parentBlock = builder->GetInsertBlock()->getParent();
	auto loopBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "loop", parentBlock);
	auto exitBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "loopexit", parentBlock);

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

void Codegen(ASTMemberOperation* memberOp) {
	auto structAlloca = (llvm::AllocaInst*)memberOp->structVar->allocaInst;

	std::vector<llvm::Value*> indices;
	auto arrayIndex = llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0, true);
	indices.push_back(arrayIndex);	// Array indices allways are 0 for now because we dont have array support!

	auto memberIndices = (U32*)(memberOp + 1);
	for (auto i = 0; i < memberOp->indexCount; i++) {
		auto& memberIndex = memberIndices[i];
		auto indexValue = llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), memberIndex, true);
		indices.push_back(indexValue);
	}

	llvm::Value* value_ptr = structAlloca;
	if (memberOp->structVar->isPointer)
		value_ptr = builder->CreateLoad(structAlloca);
	auto gep = builder->CreateGEP(value_ptr, indices, "access");
	switch (memberOp->operation) {
	case OPERATION_ASSIGN:
		auto expr = CodegenExpr(memberOp->expr);
		builder->CreateStore(expr, gep);
		return;
	}
}

llvm::Value* Codegen(ASTMemberExpr* expr) {
	assert(expr->structVar->allocaInst);
	auto structAlloca = (llvm::AllocaInst*)expr->structVar->allocaInst;

	std::vector<llvm::Value*> indices;
	auto arrayIndex = llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0, true);
	indices.push_back(arrayIndex);	// Array indices allways are 0 for now because we dont have array support!

	auto memberIndices = (U32*)(expr + 1);
	for (auto i = 0; i < expr->indexCount; i++) {
		auto& memberIndex = memberIndices[i];
		auto indexValue = llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), memberIndex, true);
		indices.push_back(indexValue);
	}

	llvm::Value* value_ptr = structAlloca;
	if(expr->structVar->isPointer) value_ptr = builder->CreateLoad(structAlloca);
	auto gep = llvm::GetElementPtrInst::Create(value_ptr, indices, "access", builder->GetInsertBlock());
	if (expr->accessMod == UNARY_ADDRESS) {
		return gep;
	} else {
		auto load = builder->CreateLoad(gep);
		return load;
	}
}

llvm::Value* Codegen (ASTVarExpr* expr) {
	assert(expr->var->allocaInst);
	auto varAlloca = (llvm::AllocaInst*)expr->var->allocaInst;

	llvm::Value* value = nullptr;
	switch(expr->accessMod) {
	case UNARY_LOAD:
		value = builder->CreateLoad(varAlloca);
		break;
	case UNARY_ADDRESS:
		value = varAlloca;
		break;
	case UNARY_VALUE:
		value = builder->CreateLoad(varAlloca);
		break;
	case UNARY_NOT:
		auto load = builder->CreateLoad(varAlloca);
		auto zeroValue = llvm::ConstantInt::get(llvm::IntegerType::getInt32Ty(llvm::getGlobalContext()), 0);
		auto cmp = value = builder->CreateICmpEQ(load, zeroValue);
		value = builder->CreateZExt(cmp, llvm::IntegerType::getInt32Ty(llvm::getGlobalContext()));
		break;
	}
	return value;
}

llvm::Value* Codegen (ASTIntegerLiteral* intNode) {
	return llvm::ConstantInt::get((llvm::Type*)intNode->type->llvmType, intNode->value);
}

llvm::Value* Codegen (ASTFloatLiteral* floatNode) {
	return llvm::ConstantFP::get((llvm::Type*)floatNode->type->llvmType, floatNode->value);
}

llvm::Value* Codegen (ASTStringLiteral* str) {
	auto str_ptr = (const char*)(str + 1);
	auto str_value = builder->CreateGlobalStringPtr(str_ptr, "str");
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
	llvm::initializeUnreachableBlockElimPass(*registry);

	llvm::cl::AddExtraVersionPrinter(llvm::TargetRegistry::printRegisteredTargetsForVersion);

	// Load the module to be compiled...
	llvm::SMDiagnostic err;
	llvm::Triple triple;
	if (MCPU == "native") MCPU = sys::getHostCPUName();
	triple.setTriple(sys::getDefaultTargetTriple());

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

	std::unique_ptr<TargetMachine> Target(TheTarget->createTargetMachine(triple.getTriple(), MCPU, featuresStr, Options, RelocModel, CMModel, OLvl));
	assert(Target && "Could not allocate target machine!");

	if (GenerateSoftFloatCalls)
		FloatABIForCalls = FloatABI::Soft;

	// TODO add options for viewing asm as text!
	bool binary = true;
	sys::fs::OpenFlags openFlags = sys::fs::F_None;
	if (!binary) {
		openFlags |= sys::fs::F_Text;
	}

	std::error_code errorCode;
	auto fileOut = llvm::make_unique<tool_output_file>(settings->outputFile, errorCode, openFlags);
	if (errorCode) {
		LOG_ERROR(errorCode.message());
		return -1;
	}


	// Build up all of the passes that we want to do to the module.
	llvm::PassManager PM;

	// Add an appropriate TargetLibraryInfo pass for the module's triple.
	llvm::TargetLibraryInfo *TLI = new llvm::TargetLibraryInfo(triple);
//	if (DisableSimplifyLibCalls)
//		TLI->disableAllFunctions();
	PM.add(TLI);

	// Add the target data from the target machine, if it exists, or the module.
	if (const DataLayout *DL = Target->getSubtargetImpl()->getDataLayout())
		module->setDataLayout(DL);
	PM.add(new DataLayoutPass());


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
		if (Target->addPassesToEmitFile(PM, FOS, fileType, true, StartAfterID, StopAfterID)) {
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
}

int WriteExecutable(BuildSettings* settings) {
	std::string allLibs;
	for(auto& dir : settings->libDirs)
		allLibs.append("-L" + settings->rootDir + dir + " ");
	for(auto& lib : settings->libNames)
		allLibs.append("-l" + lib + " ");

	std::string cmd = "clang++ " + settings->outputFile + " " + allLibs + " -o " + settings->rootDir + "app";
	LOG_INFO("Writing Executable: " << cmd);
	system(cmd.c_str());
	return 0;
}
