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

// TODO
// Consider removing the need to have a buildContext passed arround
// It litteraly serves no purpose whatsoever except the meerly exist
// Instead we can just create a global builder inside of the cpp file
// and use that to codegen the packages...  If we need to thread the
// codegeneration phase.... which would operate on different llvm modules anyway
// then we can move them into some type of LLVMWorker struct the holds the module
// and the IRBuilder associated with that package.  This would allow some seperation
// between my code and llvm nonsense...  We could get it to a point where llvm code only lives inside
// of this codegen file  It would make build times 10000% faster (literaly)

global_variable llvm::IRBuilder<>* builder = new llvm::IRBuilder<>(llvm::getGlobalContext());

//HACK HACK HACK HACK!!!!! THIS IS A MASSIVE HACK!!!
global_variable Package* global_package;

// Top level statements
// Eventualy we might consider allowing structs and functions to be declared localy at block level
void Codegen(ASTStruct* structDefn);
void Codegen(ASTFunction* function, llvm::Module* module);

// TODO
// I think that this and a mutation are essentialy the exact same thing
// Member access just needs to hold some indicies to how deep its reaching into is constituaint members
// With an extra U32 inside of the struct we can keep track of the indicies of the acces and just combine
// the member access with a variable mutation... or we could just rename this to somthing better like
// ASTMemberOperation and ASTVariableOperation which i think may be a much better alternative
// These two things would be statements and would not require any return values

// Statements
void CodegenStatement(ASTNode* node);
void Codegen(ASTVariable* var);
void Codegen(ASTVariableOperation* varOp);
void Codegen(ASTMemberOperation* memberOp);
void Codegen(ASTIfStatement* ifStatment, llvm::BasicBlock* mergeBlock, llvm::Function* function);
void Codegen(ASTIter* iter);
void Codegen(ASTReturn* retVal);

// Expressions
llvm::Value* CodegenExpr(ASTNode* expr);
llvm::Value* Codegen(ASTIntegerLiteral* intLiteral);
llvm::Value* Codegen(ASTFloatLiteral* floatLiteral);
llvm::Value* Codegen(ASTBinaryOperation* binop);
llvm::Value* Codegen(ASTMemberExpr* expr);
llvm::Value* Codegen(ASTVarExpr* expr);
llvm::Value* Codegen(ASTStringLiteral* str);
llvm::Value* Codegen(ASTCall* call);

void CodegenPackage (Package* package, const BuildContext& context) {
  global_package = package;

 	for (ASTNode* node : package->globalScope.members) {
    switch (node->nodeType) {
      case AST_FUNCTION:
        Codegen((ASTFunction*)node, package->module);
        break;
      case AST_STRUCT:
        Codegen((ASTStruct*)node);
        break;
      default:
        assert(!"A node was in the global scope that is not a function or a struct defn");
        break;
    }
  }

  llvm::raw_os_ostream stream(std::cout);
  if (llvm::verifyModule(*package->module, &stream)) {
    LOG_ERROR("llvm::Module verification failed!");
    LOG_ERROR("Build incomplete!  Skipping executable creation");
  }


}

void Codegen(ASTStruct* structDefn) {
  std::vector<llvm::Type*> memberTypes;
  for(auto type : structDefn->memberTypes)
    memberTypes.push_back(type->llvmType);
  auto& structName = structDefn->identifier->name;
  structDefn->llvmType = llvm::StructType::create(memberTypes, structName);
}

void Codegen(ASTFunction* function, llvm::Module* module) {
	//HACK to skip function codegen if the function has allready been resolved
	if (function->code != nullptr) return; // We never should have to return anything with these statements

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
	llvm::Function* llvmFunc = llvm::Function::Create(funcType, linkage, function->ident->name, global_package->module);

	// TODO arguments are created even if the function has no members!
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
				Codegen((ASTIfStatement*)node, mergeBlock, llvmFunc);
				builder->SetInsertPoint(mergeBlock);
				continue;
			}

			if(node->nodeType == AST_RETURN) {
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

	//TODO sanity check to make sure this function was foreign if it did not have a body
	//Also do a sainy check to make sure that it has created return values for all flow paths

	function->code = llvmFunc;
}

void CodegenStatement(ASTNode* node) {
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

  assert(var->allocaInst == nullptr);
  auto type = var->type->llvmType;
  if(var->isPointer) type = llvm::PointerType::get(type, 0);
  var->allocaInst = builder->CreateAlloca(type, 0, var->identifier->name);

  if (var->initalExpression == nullptr) {
    if (var->type->llvmType->isIntegerTy()) {
      var->initalExpression = CreateIntegerLiteral(0);
    } else if (var->type->llvmType->isFloatingPointTy()) {
      var->initalExpression = CreateFloatLiteral(0);
    }
  }

  if (var->initalExpression != nullptr) {
	  auto value = CodegenExpr(var->initalExpression);
	  builder->CreateStore(value, var->allocaInst);
  }
}

void Codegen(ASTVariableOperation* varOp) {
	assert(varOp->variable->allocaInst != nullptr);
	auto value = CodegenExpr(varOp->value);
  assert(value);
	builder->CreateStore(value, varOp->variable->allocaInst);
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

llvm::Value* Codegen(ASTBinaryOperation* binop)  {

	llvm::Value* lhs = CodegenExpr(binop->lhs);
	llvm::Value* rhs = CodegenExpr(binop->rhs);

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

void Codegen(ASTReturn* retVal) {

	auto value = CodegenExpr(retVal->value);
	if (value != nullptr) {
		builder->CreateRet(value);
	} else {
    LOG_ERROR("Failed to emmit code for return value");
  }
}

llvm::Value* Codegen(ASTCall* call) {
	if (!call->function->code) {
		auto lastInsertBlock = builder->GetInsertBlock();
		Codegen((ASTFunction*)call->function, global_package->module);
		builder->SetInsertPoint(lastInsertBlock);
	}
	auto llvmfunc = call->function->code;

	std::vector<llvm::Value*> argsV;
	auto argList = (ASTExpression**)(call + 1);
	for (auto i = 0; i < call->function->args.size(); i++) {
    auto arg = argList[i];
    auto arg_value = CodegenExpr(arg);
	  assert(arg_value != nullptr);
	  if (arg->nodeType == AST_STRING_LITERAL) {
  		auto zeroVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0, true);
  		std::vector<llvm::Value*> indices;
  		indices.push_back(zeroVal);
  		indices.push_back(zeroVal); // This is insane there has to be a better way
  		arg_value = builder->CreateGEP(arg_value, indices, "strgep");
	  }
	  argsV.push_back(arg_value);
	}

	if(call->function->returnType != global_voidType) {
		return builder->CreateCall(llvmfunc, argsV, "calltmp");
	} else {
		return builder->CreateCall(llvmfunc, argsV);
	}
}

void Codegen(ASTIfStatement* ifStatement, llvm::BasicBlock* mergeBlock, llvm::Function* function) {
	auto condV = CodegenExpr(ifStatement->expr);
	if (condV == nullptr) {
		LOG_ERROR("Could not emit code for if statement expression");
		return;
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
			Codegen((ASTIfStatement*)node, ifBlock, function);
			builder->SetInsertPoint(ifBlock);
			continue;
		}
		auto value = CodegenExpr(node);
		if(!value) {
			LOG_DEBUG("Failed to emit code for value in body of ifstatement!");
		}
	}

	builder->CreateBr(mergeBlock);

	if(ifStatement->elseBlock != nullptr) {
		builder->SetInsertPoint(elseBlock);
		if(ifStatement->elseBlock->nodeType == AST_IF) {
			auto ifElse = (ASTIfStatement*)ifStatement->elseBlock;
			auto condition = CodegenExpr(ifElse->expr);
			auto comp = builder->CreateICmpEQ(condition, llvm::ConstantInt::getTrue(llvm::getGlobalContext()), "elseifcond");
			auto elseIfBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "elseif", function);
			if(ifElse->elseBlock != nullptr) {

		}
			}


		for(auto node : ifStatement->elseBlock->members) {
			if(node->nodeType == AST_IF) {
				elseBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "merge", function);
				Codegen((ASTIfStatement*)node, elseBlock, function);
				builder->SetInsertPoint(elseBlock);
				continue;
			}
			auto value = CodegenExpr(node);
			if(!value) {
				LOG_DEBUG("Failed to emit code for value in body of ifstatement!");
			}
		}
		builder->CreateBr(mergeBlock);
	}
}

 void Codegen(ASTIter* iter) {
	auto var = (ASTVariable*)iter->varIdent->node;

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
	auto currentValue = builder->CreateLoad(var->allocaInst);
	auto nextValue = builder->CreateAdd(currentValue, stepValue, "increment");
	builder->CreateStore(nextValue, var->allocaInst);

	auto condLoad = builder->CreateLoad(var->allocaInst);
	auto loopCond = builder->CreateICmpSLE(condLoad, endValue, "loopcond");

	builder->CreateCondBr(loopCond, loopBlock, exitBlock);
	builder->SetInsertPoint(exitBlock);
}

void Codegen(ASTMemberOperation* memberOp) {
	auto structAlloca = memberOp->structVar->allocaInst;

	std::vector<llvm::Value*> indices;
	auto arrayIndex = llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0, true);
	indices.push_back(arrayIndex);	// Array indices allways are 0 for now because we dont have array support!
	for (auto& memberIndex : memberOp->memberIndices) {
		auto indexValue = llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), memberIndex, true);
		indices.push_back(indexValue);
	}

	llvm::Value* value_ptr = structAlloca;
	if (memberOp->structVar->isPointer) value_ptr = builder->CreateLoad(structAlloca);
	auto gep = builder->CreateGEP(value_ptr, indices, "access");
	switch (memberOp->mode) {
	case ACCESS_ASSIGN:
		auto expr = CodegenExpr(memberOp->expr);
		builder->CreateStore(expr, gep);
    return;
	}
}

llvm::Value* Codegen(ASTMemberExpr* expr) {

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

llvm::Value* Codegen (ASTVarExpr* expr) {

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

llvm::Value* Codegen (ASTIntegerLiteral* intNode) {
	return llvm::ConstantInt::get(intNode->type->llvmType, intNode->value);
}

llvm::Value* Codegen (ASTFloatLiteral* floatNode) {
	return llvm::ConstantFP::get(floatNode->type->llvmType, floatNode->value);
}

llvm::Value* Codegen (ASTStringLiteral* str) {

	auto str_value = builder->CreateGlobalStringPtr(str->value);
	return str_value;
}

int WriteNativeObject(llvm::Module* module, const BuildSettings& settings) {
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
	auto fileOut = llvm::make_unique<tool_output_file>(settings.outputFile, errorCode, openFlags);
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
