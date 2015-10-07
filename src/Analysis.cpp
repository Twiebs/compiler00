// TODO error callbacks when a node reports an error so that users of the
// language can suscribe to errors and write their own custom handlers

// TODO Analysis cares to much aboout the stucture of the AST here
// but it shouldn't.  We can store the nodes of the tree better and traverse them
// linearly here.  The tree can be optimized inorder to try and do as much work as posible
// linearly here rather than be dependant on the structure of the tree.  In theory it should be possibe
// to only care about it during Codegen (and parsing naturaly)

#include "Build.hpp"

void ReportError (Worker* worker, FileSite& site, const std::string& msg);
void ReportError(Worker* worker, const char* msg, ...);
void ReportError (Worker* worker, const std::string& msg);

internal void AnalyzeStatement(Worker* worker, ASTNode* node);
internal void AnalyzeExpr(Worker* worker, ASTExpression* expr);

internal inline void AnalyzeBlock(Worker* worker, ASTBlock* block);
internal void AnalyzeIfStatement (Worker* worker, ASTIfStatement* ifStatement);
internal void AnalyzeCall(Worker* worker, ASTCall* call);

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
    return nullptr;
}

internal void AnalyzeCall(Worker* worker, ASTCall* call) {
	assert(call->nodeType == AST_CALL);
    auto funcSet = (ASTFunctionSet*)FindNodeWithIdent(worker->currentBlock, call->name);
	if (funcSet == nullptr) {
		ReportError(worker, "Could not find any function matching the identifier" + std::string(call->name));
		return;
	} else if (funcSet->nodeType != AST_FUNCTION) {
        ReportError(worker, "Call to xxx does not name a function");
        return;
    }

	for (auto i = 0; i < call->argCount; i++) {
		AnalyzeExpr(worker, call->args[i]);
	}

	call->function = FindFunction(funcSet, call->args, call->argCount);
	if (!call->function) {
		ReportError(worker, "Could not match provided arguments to any function named " + std::string(call->name) +
				" types given: " + call->args[0]->type->name);
	} else {
		LOG_DEBUG("Resolved call to function " + std::string(name));
	}
}

internal void AnalyzeExpr (Worker* worker, ASTExpression* expr) {
	if (expr->nodeType == AST_CALL) {
		auto call = (ASTCall*)expr;
		AnalyzeCall(worker, call);
	}

	switch (expr->nodeType) {
	case AST_VAR_EXPR: {
		auto varexpr = (ASTVarExpr*)expr;
		varexpr->type = varexpr->var->type;
	} break;
	case AST_BINARY_OPERATION: {
		auto binop = (ASTBinaryOperation*)expr;
		AnalyzeExpr(worker, binop->lhs);
		AnalyzeExpr(worker, binop->rhs);
		if (TypeCompareImplicit(binop->lhs, binop->rhs)) {
			binop->type = binop->lhs->type;
		} else {
			ReportError(worker, "Type mismatch in binary operation (%s) %s (%s)", binop->lhs->type->name, ToString(binop->operation).c_str(), binop->rhs->type->name);
		}
	} break;
	case AST_MEMBER_EXPR: {
		auto memberExpr = (ASTMemberExpr*)expr;
		auto currentStruct = (ASTStruct*)memberExpr ->structVar->type;
		assert(currentStruct->nodeType = AST_STRUCT);
		for (U32 i = 0; i < memberExpr->memberCount; i++) {
			auto memberName = memberExpr ->memberNames[i];
			auto memberIndex = GetMemberIndex(currentStruct, memberName);
			if (memberIndex == - 1) {
				ReportError(worker, "Struct %s does not contain any member named %s", currentStruct->name, memberName);
			} else if (currentStruct->members[memberIndex].type->nodeType == AST_STRUCT) {
				currentStruct = (ASTStruct*)currentStruct->members[memberIndex].type;
			}
			memberExpr ->indices[i] = memberIndex;
		}

		auto lastIndex = memberExpr->indices[memberExpr->memberCount - 1];
		if (lastIndex != -1) {
			memberExpr->type = currentStruct->members[lastIndex].type;
		} else {
			// No error required we should already know what happened
		}


	} break;


	}
}



internal inline bool TypeCompareExplicit (ASTExpression* exprA, ASTExpression* exprB) {
	if (exprA->type != exprB->type)
		return false;
	return true;
}

// TODO implement implicit casting only for temporary constants in expressions
internal inline bool TypeCompareImplicit (ASTExpression* exprA, ASTExpression* exprB) {
    if (exprA->type != exprB->type)
        return false;
    return true;
}

internal inline bool TypeCompareExplicit(ASTDefinition* typeDefn, ASTExpression* expr) {
    if (typeDefn != expr->type)
        return false;
    return true;
}

internal inline bool TypeCheck (ASTExpression* expr, ASTDefinition* typedefn) {
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
internal inline void AnalyzeBlock (Worker* worker, ASTBlock* block) {
	for(auto i = 0; i < block->members.size(); i++) {
		auto node = block->members[i];
		AnalyzeStatement(worker, node);
	}
}

internal void AnalyzeStatement (Worker* worker, ASTNode* node) {
	switch(node->nodeType) {
	case AST_BLOCK:
		AnalyzeBlock(worker, (ASTBlock*)node);
		break;

	case AST_IF:
		AnalyzeIfStatement(worker, (ASTIfStatement*)node);
		break;
	case AST_ITER: {
		auto iter = (ASTIter*)node;
		assert(iter->var);
		assert(iter->start);
		AnalyzeExpr(worker, iter->var);
		AnalyzeExpr(worker, iter->start);
		if (iter->end != nullptr) {
			AnalyzeExpr(worker, iter->end);
			if (!TypeCompareExplicit(iter->start, iter->end)) {
				ReportError(worker, "Cannot iterate between two different types");
			}
		}

		AnalyzeBlock(worker, iter->body);
	} break;

	case AST_CALL:
		AnalyzeCall(worker, (ASTCall*)node);
		break;

	case AST_VARIABLE: {
		auto var = (ASTVariable*)node;
		if (var->initalExpression != nullptr) {
			AnalyzeExpr(worker, var->initalExpression);
		}

		if (var->type == nullptr) {
			if (var->typeName != nullptr) {
				var->type = (ASTDefinition*)FindNodeWithIdent(worker->currentBlock, var->typeName);
				assert(var->type->nodeType != AST_DEFINITION && "Primitive types should always be resolved already");
				if (var->type->nodeType != AST_STRUCT) {
					ReportError(worker, "Variable '%s' could not be declared with type '%s': '%s' does not represent a struct" , var->name, var->typeName, var->typeName);
				}
			} else if (var->initalExpression != nullptr) {
				var->type = var->initalExpression->type;
				if (var->initalExpression->nodeType == AST_VAR_EXPR) {
					auto varExpr = (ASTVarExpr*)var->initalExpression;
					var->isPointer = varExpr->accessMod == UNARY_ADDRESS;
				} else if (var->initalExpression->nodeType == AST_MEMBER_EXPR) {
					auto memberExpr = (ASTMemberExpr*)var->initalExpression;
					var->isPointer = memberExpr->accessMod == UNARY_ADDRESS;
				}
			}
		}

		if (var->initalExpression != nullptr){
			if (!TypeCheck(var->initalExpression, var->type)) {
				ReportError(worker, "Type mismatch!  Variable (%s : %s) does not match inital expression (%s)", var->name, var->type->name, var->initalExpression->type->name);
			}
		}

	} break;
	case AST_VARIABLE_OPERATION: {
		auto varOp = (ASTVariableOperation*)node;
		AnalyzeExpr(worker, varOp->expr);
        auto variable = varOp->variable;
        if (!TypeCompareExplicit(variable->type, varOp->expr)) {
            ReportError(worker, "Type mismatch! Variable %s of type %s does not match rhs expression of type %s",
                        variable->name, variable->type->name, varOp->expr->type->name);
        }
	} break;

    case AST_MEMBER_OPERATION: {
        auto memberOp = (ASTMemberOperation*)node;
		AnalyzeExpr(worker, memberOp->expr);

		auto currentStruct = (ASTStruct*)memberOp->structVar->type;
		assert(currentStruct->nodeType = AST_STRUCT);
		for (U32 i = 0; i < memberOp->memberCount; i++) {
			auto memberName = memberOp->memberNames[i];
			auto memberIndex = GetMemberIndex(currentStruct, memberName);
			if (memberIndex == - 1) {
				ReportError(worker, "Struct %s does not contain any member named %s", currentStruct->name, memberName);
			} else if (currentStruct->members[memberIndex].type->nodeType == AST_STRUCT) {
				currentStruct = (ASTStruct*)currentStruct->members[memberIndex].type;
			}
			memberOp->indices[i] = memberIndex;
		}
    } break;

	case AST_RETURN: {
		auto retval = (ASTReturn*)node;
		AnalyzeExpr(worker, retval->value);
		auto function = (ASTFunction*)worker->currentBlock;
		if (!TypeCheck(retval->value, function->returnType)) {
			ReportError(worker, "Return type does not match function return type in function: ");
		}

	} break;

	default:
		assert(false && "Unhandled statement resolution!!!!");
		break;
	}
}

internal void AnalyzeIfStatement (Worker* worker, ASTIfStatement* ifStatement) {
	assert(ifStatement->expr != nullptr);
	AnalyzeExpr(worker, ifStatement->expr);
	AnalyzeStatement(worker, ifStatement->ifBody);
	if (ifStatement->elseBody != nullptr) {
		AnalyzeStatement(worker, ifStatement->elseBody);
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
			auto previousBlock = worker->currentBlock;
			worker->currentBlock = function;
			AnalyzeBlock(worker, function);
			worker->currentBlock = previousBlock;
		}
	}
}
