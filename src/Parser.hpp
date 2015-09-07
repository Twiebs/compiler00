#pragma once

#include "AST.hpp"
#include "Build.hpp"
#include "Lexer.hpp"

//Considering that we dont need to preserve the tokenString here perhaps we can simply return it or somthging?
// Do we need some sort of internal lex_state so that we can keep track of those things without any overhea?
//For now ill just put it inside the package state and see what happens

enum ParseFlags {
	PARSE_INVALID = 1 << 0,
};

//The current state of a parse as it walks a file
//This is much better
//And have the lext state contain only the information it needs to sucuessfuly lex the file...
//This makes a lot of sense	//We can possibly return a token struct or somthing like that?
//Mabye pass a pointer to a tokenstruct that gets initalized when the lexer lexes a file and then the result of the lexing
//Is stored inside ofthat token struct!
struct ParseState {
	ASTBlock* currentScope = nullptr;
	U32 errorCount = 0;
	U32 flags = 0;
	std::vector<std::string> importedFiles;
	BuildSettings* settings;
	MemoryArena arena;
};

void ParseFile(ParseState& parseState, const std::string& rootDir, const std::string& filename);
void ReportError(ParseState& parseState, FileSite& site, const std::string& msg);
