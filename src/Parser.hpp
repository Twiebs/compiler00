#pragma once

#include <string>

#include "AST.hpp"
#include "Lexer.hpp"

struct Worker;

bool ParseFile(Worker *worker, const std::string& filename);