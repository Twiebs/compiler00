#pragma once

#include "llvm/IR/IRBuilder.h"
#include "AST.hpp"
#include "Build.hpp"

// Is it possible to compleatly segergate Statements and Expressions
// From eachother?
// Why do we actualy need an ASTNode?  Thats such an abstract thing for no reason
// What purpose does it actualy serve?
// We could write an ASTCast Macro that does some hacks to cast it to the right type

void CodegenPackage(Package* package, const BuildContext& context);

//Statements
