/*
 * AST.hpp
 *
 *  Created on: Jun 18, 2015
 *      Author: Torin Wiebelt
 */

#ifndef AST_HPP_
#define AST_HPP_

#include <vector>
#include "llvm/IR/Value.h"
#include "Lexer.hpp"


//Base node all members of the AST inherit from:
//Archetype of:
//ASTExpression
//ASTFunction
//ASTType
class ASTNode {
public:
friend class CodeGenerator;
	virtual ~ASTNode() { }
protected:
};


class ASTType : public ASTNode {
public:
	ASTType(std::string name, llvm::Type* llvmType) : name(name), llvmType(llvmType) { }
	~ASTType() { }
	std::string name;
	llvm::Type* llvmType;
private:

};


//Represents an expression in the AST
class ASTExpression : public ASTNode {
public:
	virtual ~ASTExpression() { }
};



//Represents a prototype of a function in the AST
//TODO This is probably completly unessecary
class ASTPrototype : public ASTNode {
friend class CodeGenerator;
public:
	ASTPrototype(std::string& name, std::vector<Type> args, bool foreign) :
		name(name), args(args), isForegin(foreign) { }

private:
	bool isForegin = false;
	std::string name;
	std::vector<Type> args;
};

//A Node representing a function definition in the AST
class ASTFunction : public ASTNode {
friend class CodeGenerator;

public:
	ASTFunction(std::string& name, std::vector<Type> args) : name(name), args(args) { }
	~ASTFunction() { }
	std::vector<ASTNode*> body;
	ASTType* returnType;
private:
	std::string name;
	std::vector<Type> args;

};


//Represents a variable of some value and type
class ASTVariable : public ASTExpression {
public:
	ASTVariable(const std::string& name, ASTType* type) :
			name(name), type(type) {
		//Blank Constructor
	}
private:
	std::string name;
	ASTType* type;
};

class ASTNumber : public ASTExpression {
public:
	ASTNumber() { }
};



//Represents a call to a function in the AST
class ASTCall : public ASTNode {
friend class CodeGenerator;
public:
	ASTCall(std::string& functionName, std::vector<ASTExpression*> args) :
		functionName(functionName), args(args) { }
private:
	std::string functionName;
	std::vector<ASTExpression*> args;
};


#endif /* AST_HPP_ */
