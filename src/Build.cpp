#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/raw_os_ostream.h"

#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetSubtargetInfo.h"

#include "llvm/Bitcode/ReaderWriter.h"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"

#include "llvm/ADT/Triple.h"
#include "llvm/CodeGen/CommandFlags.h"
#include "llvm/CodeGen/LinkAllAsmWriterComponents.h"
#include "llvm/CodeGen/LinkAllCodegenComponents.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Pass.h"
#include "llvm/PassManager.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PluginLoader.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Target/TargetLibraryInfo.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetSubtargetInfo.h"

#include "Build.hpp"
#include "Parser.hpp"

using namespace llvm;

void WriteBitcode(llvm::Module* module, const std::string& outputFile);
int WriteNativeObject(llvm::Module* module, const BuildSettings& settings);
int WriteExecutable(BuildSettings& settings);

int PreBuild(const BuildContext& context, const BuildSettings& settings) {
	return 0;
}

int Build(BuildContext& context, BuildSettings& settings) {
	// Package Must Persist pass this stage
	auto package = new Package;
	package->module = new llvm::Module("LLVMLang Compiler (TODO PackageName)", llvm::getGlobalContext());
	InitalizeLanguagePrimitives(&package->globalScope, package->module);
	context.packages.push_back(package);
	context.currentPackage = package;

	//The parseState is set to emit AST identifiers into the global scope of our new package
	ParseState parseState;
	parseState.currentScope = &package->globalScope;

	// The Lex state is set to lex tokens from the inputfile of the package...
	// Perhaps BuildTakes a package pointer instead of context / build settings
	// And we consider the build step on packages instead of the entier project
	// We could seperate into two functions
	// BuildProject() and BuildPackage();
	// That sounds like a good idea
	// The only thing that is needed is the llvmModule
	// But wait!!!!! That would be package independent anyway!

	Lexer lex;
	lex.stream.open(settings.rootDir + settings.inputFile);
	if(!lex.stream.is_open()) {
		LOG_ERROR("Could not open file" + settings.rootDir + settings.inputFile);
	}
	lex.nextChar = lex.stream.get();

	ParseFile(parseState, lex);

	//TODO Codegen no-longer is a step in the build process
	// We need to walk the tree first and then codegen after the AST has been fully resolved!
	// It doesnt happen at all anymore
	//WARN the program *should* crash when we get here!

	if ((parseState.flags & PACKAGE_INVALID)) {
		LOG_ERROR("There were errors parsing the file.  Bypassing Codegeneration");
		return -1;
	}

	if (parseState.errorCount == 0) {
		// TODO Return some error code if codegen fails
		Codegen (package, context);
		if (settings.logModuleDump) {
			package->module->dump();
		}

		if (settings.outputFile == "") {
			auto inputBase = settings.inputFile.substr(0, settings.inputFile.find(".") + 1);
			settings.outputFile = settings.rootDir + inputBase + "o";
		}

		llvm::raw_os_ostream stream(std::cout);
		if (llvm::verifyModule(*package->module, &stream)) {
			LOG_ERROR("llvm::Module verification failed!");
			LOG_ERROR("Build incomplete!  Skipping executable creation");
			return -1;
		}


		if (settings.emitNativeOBJ)
			WriteNativeObject(package->module, settings);
		if (settings.emitExecutable)
			WriteExecutable(settings);
	} else {
		LOG_ERROR("There were errors building the package");
		return -1;

	}
	return 0;
}

int PostBuild(const BuildContext& context, const BuildSettings& settings) {
	return 0;
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

int WriteExecutable(BuildSettings& settings) {
	std::string allLibs;

	for(auto& dir : settings.libDirs)
		allLibs.append("-L" + settings.rootDir + dir + " ");
	for(auto& lib : settings.libNames)
		allLibs.append("-l" + lib + " ");

	std::string cmd = "clang++ " + settings.outputFile + " " + allLibs + " -o " + settings.rootDir + "app";
	LOG_INFO("Writing Executable: " << cmd);
	system(cmd.c_str());
	return 0;
}

void WriteBitcode(llvm::Module* module, const std::string& outputFile) {
	std::error_code errorCode;
	llvm::raw_fd_ostream ostream(outputFile, errorCode, llvm::sys::fs::F_None);
	llvm::WriteBitcodeToFile(module, ostream);
}
