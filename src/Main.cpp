// NOTE all of this stuff was created on June18 if you end of caring about that sort of thing!

#include <string>
#include <iostream>
#include <fstream>
#include <system_error>
#include <unistd.h>

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FormattedStream.h"

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

#include "Lexer.hpp"
#include "Parser.hpp"

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
#include <memory>
using namespace llvm;

//TODO move build settings into their own seperate file
//Consider using a STU build
// Create a MemoryManager
//MemoryManager::CreateNode(nodeType);
// Memory::RemoveNode()
// Memory::AddNode()
// Add node is really interesting because it implies that a node would be added to the scope / package
//That you sepcificy... I think that is a really good way to go.. However; it does not imply that memory is actualy
//being allocated and freed which might be confusing

//TODO: Set a flag to stop Codegen from happening if the BuildManager reports errors

struct BuildSettings {
	std::string inputFile; // For now this is just a singular thing for simplicity
	std::vector<std::string> importDirs;
	std::vector<std::string> libDirs;
	std::vector<std::string> libs;
	bool emitBitcode;
	bool emitAsm;
	bool emitNativeOBJ;
	bool createExecutable;
};

struct BuildContext {
	llvm::Module* llvmModule;
	Package* currentPackage;
	std::vector<Package*> packages;	//We probably dont need to store somthing like this because it will
	std::string rootDir;
	//end up being iside the memory manager
	ParseState parseState;
	U32 errorCount;
};

//TODO setup language bultins here.
//or divert to acustom user tool with an api to do some preprocessor stuff or whatever
// build tool they want to use
//The Pre build and post build will return ints inorder to check for error codes
int PreBuild() {

}

//TODO we can also do this exact same thing here but after the first build has been run
// I think that this is arelly good idea nad can provide a really great exensibility
int PostBuild() {

}

//When we build we allways parse so this is where the parsing will kick off
//After the parsing is complete we check someflags in the buildsettings to determine what to do with
//The created llvm::Module
void Build(const BuildSettings& settings) {
	BuildContext context;
	context.llvmModule = new llvm::Module("LLVMLang Compiler", llvm::getGlobalContext());
	ParseFile(settings.inputFile, context);

	//Build the llvm::Module

	if (settings.emitNativeOBJ) {
		WriteNativeObject(context->module, )
	}

}

int WriteNativeObject(llvm::Module* module, const std::string& outFile, const BuildSettings& settings) {
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

	cl::AddExtraVersionPrinter(TargetRegistry::printRegisteredTargetsForVersion);

	// Load the module to be compiled...
	SMDiagnostic err;
	Triple triple;
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

	TargetOptions Options = InitTargetOptionsFromCodeGenFlags();
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
	auto fileOut = llvm::make_unique<tool_output_file>(outFile, errorCode, openFlags);
	if (errorCode) {
		LOG_ERROR(errorCode.message());
		return -1;
	}


	// Build up all of the passes that we want to do to the module.
	PassManager PM;

	// Add an appropriate TargetLibraryInfo pass for the module's triple.
	TargetLibraryInfo *TLI = new TargetLibraryInfo(triple);
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

	std::string libs;
	for(auto& dir : settings.libDirs) {
		libs.append("-L" + dir + " ");
	}

	for(auto& lib : settings.libs) {
		libs.append("-l" + lib + " ");
	}
	std::string cmd = "clang++ test.o " + libs + " -o app";
	system(cmd.c_str());
	return 0;

}

void WriteBitcode(llvm::Module* module, const std::string& outputFile) {
	std::error_code errorCode;
	llvm::raw_fd_ostream ostream(outputFile, errorCode, llvm::sys::fs::F_None);
	llvm::WriteBitcodeToFile(module, ostream);
}

int main(int argc, char** argv) {
	BuildSettings settings;
	settings.libDirs.push_back("build/libcpp");
	settings.libs.push_back("std");
	settings.libs.push_back("SDL");
	settings.libs.push_back("GL");

	BuildContext context;

	//Parse LLVM Command Line Options
	static llvm::cl::opt<std::string> inputFile(llvm::cl::Positional, llvm::cl::desc("<input file>"));
	static llvm::cl::opt<std::string> outputFile("o", llvm::cl::desc("Output filename"), llvm::cl::value_desc("filename"));
	llvm::cl::ParseCommandLineOptions(argc, argv);


	if (inputFile == "") {
		std::cout << "Error: You must specify the filename of the program to "
		"be compiled.  Use --help to see the options.\n";
		abort();
	}

	std::string input = inputFile;
	auto lastSlash = input.find_last_of("/");
	if(lastSlash == std::string::npos) {
		context.rootDir = "";
	} else {
		context.rootDir = input.substr(0, lastSlash + 1);
		input.erase(0, lastSlash);
	}

	if(outputFile == "") {
		std::string base = inputFile;
		base = base.substr(0, base.size() - 3);
		outputFile = base + "bc";
	}

	llvm::LLVMContext& llvmContext = llvm::getGlobalContext();
	llvm::Module* module = new llvm::Module("LLVMLang Compiler", llvmContext);

	std::vector<std::string> importDirectories;
	importDirectories.push_back("");

	CodeGenerator codeGenerator(module);
	Parser parser(importDirectories, module, &codeGenerator);
	parser.ParseFile(input);

	std::cout << "\x1b[31m" << "\n";
	llvm::raw_os_ostream stream(std::cout);
	if (llvm::verifyModule(*module, &stream)) {
		LOG_ERROR("LLVMModule verification failed!");
	}
	std::cout << "\x1b[33m" << "\n";
	module->dump();

	// if(gErrorCount > 0) {
	// 	LOG_ERROR("There were " << gErrorCount - 1<< " errors!");
	// } else {
	// 	LOG_INFO("No Errors were reported!  Have a \x1b[31mW\x1b[32mo\x1b[33mn\x1b[34md\x1b[35me\x1b[36mr\x1b[31mf\x1b[32mu\x1b[33ml\x1b[34ml \x1b[39mday!");
	// }

	// LOG_INFO("Root directory is " << rootDir);

	std::string base = input;
	base = base.substr(0, base.find_last_of("."));
	outputFile = base + ".o";
}
