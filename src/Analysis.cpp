// TODO error callbacks when a node reports an error so that users of the
// language can suscribe to errors and write their own custom handlers

// TODO Analysis cares to much aboout the stucture of the AST here
// but it shouldn't.  We can store the nodes of the tree better and traverse them
// linearly here.  The tree can be optimized inorder to try and do as much work as posible
// linearly here rather than be dependant on the structure of the tree.  In theory it should be possibe
// to only care about it during Codegen (and parsing naturaly)

#include "Build.hpp"
#include "Analysis.hpp"

void ReportError (Worker* worker, FileSite& site, const std::string& msg);
void ReportError (Worker* worker, FileSite* site, const char* msg, ...);
void ReportError(Worker* worker, const char* msg, ...);
void ReportError (Worker* worker, const std::string& msg);

internal inline void AnalyzeBlock(ASTBlock* block);
internal void AnalyzeIfStatement (ASTIfStatement* ifStatement, ASTBlock* currentBlock);
internal void AnalyzeCall(ASTCall* call, ASTBlock* currentBlock);

internal inline bool TypeCheck (ASTExpression* expr, ASTDefinition* typedefn);
internal inline bool TypeCompareExplicit (ASTExpression* exprA, ASTExpression* exprB);
internal inline bool TypeCompareImplicit (ASTExpression* exprA, ASTExpression* exprB);

ASTFunction* FindFunction (ASTFunctionSet* funcSet, ASTExpression** args, U32 argc) {
    for (auto i = 0; i < funcSet->functions.size(); i++) {
        auto func = funcSet->functions[i];
        if (func->args.size() == argc) {
            if (argc > 0) {
                bool functionMatches = true;
                for (auto i = 0; i < argc; i++) {
                    auto arg = args[i];
                    if (func->args[i]->type != arg->type) {
                        functionMatches = false;
                    }
                }
                if (functionMatches)
                    return func;
            } else {
                return func;
            }
        } else if (func->isVarArgs) {
            bool funcMatches = true;
            for(auto i = 0; i < func->args.size(); i++) {
                auto arg = args[i];
                if (!TypeCompareExplicit(func->args[i], arg)) {
                    funcMatches = false;
                }
            }
            if (funcMatches)
                return func;
        }
    }

	if (funcSet->parent != nullptr)
		return FindFunction(funcSet->parent, args, argc);
    return nullptr;
}

internal void AnalyzeCall (ASTCall* call, ASTBlock* currentBlock) {
	assert(call->nodeType == AST_CALL);
    auto funcSet = (ASTFunctionSet*)FindNodeWithIdent(currentBlock, call->name);
	if (funcSet == nullptr) {
		// ReportError(worker, "Could not find any function matching the identifier" + std::string(call->name));
		return;
	} else if (funcSet->nodeType != AST_FUNCTION_SET) {
        // ReportError(worker, "Call to xxx does not name a function");
        return;
    }

	for (auto i = 0; i < call->argCount; i++) {
		AnalyzeExpr(call->args[i], currentBlock);
	}

	call->function = FindFunction(funcSet, call->args, call->argCount);
	if (call->function == nullptr) {
		// ReportError(worker, "A function named %s exists but its signiture does not match the provided arguments!", call->name);
	} else {
		call->type = call->function->returnType;
	}
}


ASTDefinition* ResolveMemberAccess(ASTMemberAccess* access, ASTStruct* structType) {
	for (U32 i = 0; i < access->memberCount; i++) {
		auto memberName = access->memberNames[i];
		auto memberIndex = GetMemberIndex(structType, memberName);
		if (memberIndex == - 1) {
			ReportError() << "Struct " << structType->name << "does not contain any member named " << memberName;
		} else if (structType->members[memberIndex].type->nodeType == AST_STRUCT) {
			structType = (ASTStruct *) structType->members[memberIndex].type;
		} else {
			ReportError() << "Member " << memberName << "in member expr is not a struct type!";
		}
		access->indices[i] = memberIndex;
	}

	return structType->members[access->memberCount - 1].type;
}

void AnalyzeExpr (ASTExpression* expr, ASTBlock* currentBlock) {
	if (expr->nodeType == AST_CALL) {
		auto call = (ASTCall*)expr;
		AnalyzeCall(call, currentBlock);
	}

	switch (expr->nodeType) {


	case AST_VAR_EXPR: {
		auto varexpr = (ASTVarExpr*)expr;
		varexpr->type = varexpr->var->type;
	} break;

	// Consider making the assignment operator
	// a binary operator
	case AST_BINARY_OPERATION: {
		auto binop = (ASTBinaryOperation*)expr;
		AnalyzeExpr(binop->lhs, currentBlock);
		AnalyzeExpr(binop->rhs, currentBlock);
		if (TypeCompareImplicit(binop->lhs, binop->rhs)) {
			binop->type = binop->lhs->type;
		} else {
			// ReportError(worker, "Type mismatch in binary operation (%s) %s (%s)", binop->lhs->type->name, ToString(binop->operation).c_str(), binop->rhs->type->name);
		}
	} break;
	case AST_MEMBER_EXPR: {
		auto memberExpr = (ASTMemberExpr*)expr;
		assert(memberExpr->access.memberCount > 0 && (memberExpr->access.indices != nullptr || memberExpr->access.memberNames != nullptr));

		if (memberExpr->access.memberNames != nullptr)
			memberExpr->type = ResolveMemberAccess(&memberExpr->access, (ASTStruct*)memberExpr->structVar->type);

	} break;


	case AST_CAST: {
		auto cast = (ASTCast*)expr;
		AnalyzeExpr(cast->expr, currentBlock);
	} break;


	}
}

static inline bool TypeCompareExplicit (ASTExpression* exprA, ASTExpression* exprB) {
	if (exprA->type != exprB->type)
		return false;
	return true;
}

// TODO implement implicit casting only for temporary constants in expressions
static inline bool TypeCompareImplicit (ASTExpression* exprA, ASTExpression* exprB) {
    if (exprA->type != exprB->type)
        return false;
    return true;
}

static inline bool TypeCompareExplicit(ASTDefinition* typeDefn, ASTExpression* expr) {
    if (typeDefn != expr->type)
        return false;
    return true;
}

static inline bool TypeCheck (ASTExpression* expr, ASTDefinition* typedefn) {
	if (expr->type != typedefn)
		return false;
	return true;
}

// NOTE
// Analyzing blocks is really stupid.  There is no reason to include them in the analysis
// phase.  There should be a linearized version of the AST that does not care about the
// structure of the tree and just hits the nodes independently rather than reaching through
// its pointer stored in the block.  In theory the blocks should only really mater for codegeneration and
// parsing.  Otherwise we can hit the nodes in any order whatsoever during the anayalsis because it doesnt
// mater and will be faster to traverse them linearly rather then this tree structure
static inline void AnalyzeBlock (ASTBlock* block) {
	for (auto i = 0; i < block->members.size(); i++) {
		auto node = block->members[i];
		AnalyzeStatement(node, block);
	}
}

S8 GetAbsoluteIndirectionLevelForExpression(ASTExpression* expr) {
	S8 result;
	if (expr->nodeType == AST_VAR_EXPR) {
		auto variableExpr = static_cast<ASTVarExpr*>(expr);
		result += variableExpr->var->indirectionLevel;
	} else if (expr->nodeType == AST_MEMBER_EXPR) {
		auto memberExpr = static_cast<ASTMemberExpr*>(expr);
		result += memberExpr->structVar->indirectionLevel;
	} else if (expr->nodeType == AST_UNARY_OPERATION) {
		auto unaryOperation = static_cast<ASTUnaryOp*>(expr);
		result += GetAbsoluteIndirectionLevelForExpression(unaryOperation->expr);
		result += unaryOperation->indirectionLevel;
	}

	return result;
}


static inline void InferVariableTypeFromExpression(ASTVariable* variable) {
	assert(variable->initalExpression != nullptr);
	variable->indirectionLevel = GetAbsoluteIndirectionLevelForExpression(variable->initalExpression);
	assert(variable->indirectionLevel >= 0 && "Infered variable types must have an indirection greater than 0");
	variable->type = variable->initalExpression->type;
}

static inline ASTNode* FindNodeWithIndentAndExpectType(ASTBlock* block, char* name, ASTNodeType expectedNodeType) { 
	auto node = FindNodeWithIdent(block, name);
	if (node->nodeType != expectedNodeType) {
		assert(false);
		// Report an error here
	}

	return node;
}

static inline void AnalyzeVariableDecleration (ASTVariable* variable, ASTBlock* currentBlock) {
	if (variable->type == nullptr && variable->typeName != nullptr) {
		variable->type = static_cast<ASTDefinition*>(FindNodeWithIndentAndExpectType(currentBlock, variable->typeName, AST_DEFINITION));
		if (variable->type == nullptr) {
			// ReportError(worker, "Variable(%s) could not be declared with type(%s): Type does not exist!", var->name, var->typeName);
			return;
		}
	}
	
	if (variable->initalExpression != nullptr) {
		AnalyzeExpr(variable->initalExpression, currentBlock);
		if (variable->type == nullptr) {
			InferVariableTypeFromExpression(variable);
		}
	}



	if (variable->initalExpression != nullptr) {
		if (!TypeCheck(variable->initalExpression, variable->type)) {
			if (variable->initalExpression->type == global_F32Type && variable->type == global_F64Type) {
				variable->initalExpression->type = global_F64Type;
			} else {
				// ReportError(worker, "Type mismatch!  Variable (%s : %s) does not match inital expression (%s)", var->name, var->type->name, var->initalExpression->type->name);
			}
		}
	}
}


static inline ASTStructMember* GetStructMember(ASTStruct* structDefn, ASTMemberAccess* access) {
	ASTStruct* currentStructDefn = structDefn;
	for (auto i = 0; i < access->memberCount - 1; i++) {
		currentStructDefn = static_cast<ASTStruct*>(structDefn->members[access->indices[i]].type);
		assert(currentStructDefn->nodeType == AST_STRUCT);
	}

	U32 memberIndex = access->indices[access->memberCount - 1];
	auto result = &currentStructDefn->members[memberIndex];
	return result;
}


void AnalyzeStatement (ASTNode* node, ASTBlock* currentBlock) {
	switch(node->nodeType) {

	case AST_BLOCK:
		AnalyzeBlock((ASTBlock*)node);
		break;

	case AST_STRUCT: {
		// TODO struct internal type dependencies
	} break;


	case AST_IF:
		AnalyzeIfStatement((ASTIfStatement*)node, currentBlock);
		break;
	case AST_ITER: {
		auto iter = (ASTIter*)node;
		assert(iter->var);
		assert(iter->start);
		AnalyzeExpr(iter->var, currentBlock);
		AnalyzeExpr(iter->start, currentBlock);
		if (iter->end != nullptr) {
			AnalyzeExpr(iter->end, currentBlock);
			if (!TypeCompareExplicit(iter->start, iter->end)) {
				// ReportError(worker, "Cannot iterate between two different types");
			}
		}

		AnalyzeBlock(iter->body);
	} break;

	case AST_CALL:
		AnalyzeCall((ASTCall*)node, currentBlock);
		break;

	case AST_VARIABLE: {
		auto var = (ASTVariable*)node;


	} break;
	case AST_VARIABLE_OPERATION: {
		auto varOp = (ASTVariableOperation*)node;
		AnalyzeExpr(varOp->expr, currentBlock);
        auto variable = varOp->variable;
        if (!TypeCompareExplicit(variable->type, varOp->expr)) {
           // ReportError(worker, "Type mismatch! Variable %s of type %s does not match rhs expression of type %s",
            //            variable->name, variable->type->name, varOp->expr->type->name);
        }
	} break;

    case AST_MEMBER_OPERATION: {
        auto memberOp = (ASTMemberOperation*)node;
		AnalyzeExpr(memberOp->expr, currentBlock);

		auto currentStruct = (ASTStruct*)memberOp->structVar->type;
		assert(currentStruct->nodeType = AST_STRUCT);

		if (memberOp->access.memberNames != nullptr) {
			auto memberType = ResolveMemberAccess(&memberOp->access, (ASTStruct*)memberOp->structVar->type);
		}

		ASTStruct* structDefn = static_cast<ASTStruct*>(memberOp->structVar->type);
		auto structMember = GetStructMember(structDefn, &memberOp->access);
		if (!TypeCompareExplicit(structMember->type, memberOp->expr)) {
			ReportError() << "Type mismatch! Struct member(" << structMember->name << " : " << structMember->type->name << ") does not match expression("
				<< memberOp->expr->type->name << ")\n";
		}


    } break;

	case AST_RETURN: {
		auto retval = (ASTReturn*)node;
		AnalyzeExpr(retval->value, currentBlock);
		retval->type = retval->value->type;
		auto function = (ASTFunction*)currentBlock;
		if (!TypeCheck(retval->value, function->returnType)) {
			// ReportError(worker, &retval->site, "Return type(%s) does not match function return type(%s) in function(%s)", retval->type->name, function->returnType->name, function->name);
		}

	} break;


	case AST_FUNCTION_SET:{
		// Let this one just pass through
	} break;

	case AST_FUNCTION : {
		auto function = (ASTFunction*)node;
		AnalyzeBlock(function);

		// XXX we need to create some notion of a ASTTypeRefrence or somthing that will handle keeping the names of the types
		// that we require if the tree is unable to find what they are looking for.  For instance the return type of thsi function may
		// be a struct type that is declared in another file that we have not parsed yet and have no idea what it is.

	} break;


	default:
		assert(false && "Unhandled statement resolution!!!!");
		break;
	}
}

internal void AnalyzeIfStatement (ASTIfStatement* ifStatement, ASTBlock* currentBlock) {
	assert(ifStatement->expr != nullptr);
	AnalyzeExpr(ifStatement->expr, currentBlock);
	AnalyzeStatement(ifStatement->ifBody, currentBlock);
	if (ifStatement->elseBody != nullptr) {
		AnalyzeStatement(ifStatement->elseBody, currentBlock);
	}
}

// In here we would be hitting structDefns as well as functions
// Since we know all the structs take care of themselves it might make sense
// To keep these seperatly but for now i will keep them the same
void AnalyzeAST (Worker* worker) {
	for (auto i = 0; i < worker->currentBlock->members.size(); i++) {
		auto node = worker->currentBlock->members[i];
		if (node->nodeType == AST_FUNCTION) {
			auto function = (ASTFunction*)node;
			AnalyzeBlock(function);
		}
	}
}
