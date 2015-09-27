// TODO Analysis cares to much aboout the stucture of the AST here
// but it shouldn't.  We can store the nodes of the tree better and traverse them
// linearly here.  The tree can be optimized inorder to try and do as much work as posible
// linearly here rather than be dependant on the structure of the tree.  In theory it should be possibe
// to only care about it during Codegen (and parsing naturaly)

void ReportError (Worker* worker, FileSite& site, const std::string& msg);
void ReportError (Worker* worker, const std::string& msg);

internal void AnalyzeStatement(Worker* worker, ASTNode* node);
internal void AnalyzeExpr(Worker* worker, ASTExpression* expr);

internal inline void AnalyzeBlock(Worker* worker, ASTBlock* block);
internal void AnalyzeIfStatement (Worker* worker, ASTIfStatement* ifStatement);
internal void AnalyzeCall(Worker* worker, ASTCall* call);

internal inline bool TypeCompare(ASTExpression* exprA, ASTExpression* exprB);
internal inline bool TypeCheck(ASTExpression* expr, ASTDefinition* typedefn);

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
                if (!TypeCompare(func->args[i], arg)) {
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
	auto name = (const char*)(((U8*)(call + 1)) + ((sizeof(ASTExpression*) * call->argCount)));
	auto ident = FindIdentifier(worker->currentScope, name);
	if (!ident) {
		ReportError(worker, "Could not find any function matching the identifier" + std::string(name));
		return;
	}
	auto args = (ASTExpression**)(call + 1);
	for (auto i = 0; i < call->argCount; i++) {
		AnalyzeExpr(worker, args[i]);
	}
	auto funcSet = (ASTFunctionSet*)ident->node;
	assert(funcSet->nodeType = AST_FUNCTION);
	call->function = FindFunction(funcSet, args, call->argCount);
	if (!call->function) {
		ReportError(worker, "Could not match argument types to any function named: " + std::string(name) +
				"types given: " + args[0]->type->identifier->name);
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
	case AST_BINOP: {
		auto binop = (ASTBinaryOperation*)expr;
		AnalyzeExpr(worker, binop->lhs);
		AnalyzeExpr(worker, binop->rhs);
		if (TypeCompare(binop->lhs, binop->rhs)) {
			binop->type = binop->lhs->type;
		} else {
			ReportError(worker, "Type mismatch in binary operation: " + binop->lhs->type->identifier->name + ", " + binop->rhs->type->identifier->name);
		}
	} break;
	}
}

// This is where we can implement implict castinging between ints / floats / whatever
// TODO implement implicit casting
// TODO create a diffrent function for comparing temporary expressions in compound
// expersions rather than using this function which should be soley for top level expressions
// THis way the expresions can have implicit casting between temporary values and very strict
// casting rules when talking about statements.
internal inline bool TypeCompare (ASTExpression* exprA, ASTExpression* exprB) {
	if (exprA->type != exprB->type)
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
			if (!TypeCompare(iter->start, iter->end)) {
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

		// Variables type will be null if variables type is being infered
		if (var->type == nullptr) {
			var->type = var->initalExpression->type;
			if (var->initalExpression->nodeType == AST_VAR_EXPR) {
				auto varExpr = (ASTVarExpr*)var->initalExpression;
				var->isPointer = varExpr->accessMod == UNARY_ADDRESS;
			} else if (var->initalExpression->nodeType == AST_MEMBER_EXPR) {
				auto memberExpr = (ASTMemberExpr*)var->initalExpression;
				var->isPointer = memberExpr->accessMod == UNARY_ADDRESS;
			}
		}

		else if (var->initalExpression != nullptr){
			if (!TypeCheck(var->initalExpression, var->type)) {
				ReportError(worker, "Type mismatch!  Variable type does not match inital expression");
			}
		}

		else {
			// Nothing to do here this is just a declaration without an expression which has a type and is auto initialized
		}
	} break;
	case AST_VARIABLE_OPERATION: {
		auto varOp = (ASTVariableOperation*)node;
		AnalyzeExpr(worker, varOp->expr);
	} break;

    case AST_MEMBER_OPERATION: {
        auto memberOp = (ASTMemberOperation*)node;
        AnalyzeExpr(worker, memberOp->expr);
    } break;

	case AST_RETURN: {
		auto retval = (ASTReturn*)node;
		AnalyzeExpr(worker, retval->value);
		auto function = (ASTFunction*)worker->currentScope;
		if (!TypeCheck(retval->value, function->returnType)) {
			ReportError(worker, "Return type does not match function return type in function: " + function->ident->name);
		}

	} break;

	default:
		assert(false && "Unhandled statement resolution!!!!");
		break;
	}
}

static void AnalyzeIfStatement (Worker* worker, ASTIfStatement* ifStatement) {
	assert (ifStatement->expr != nullptr);
	AnalyzeExpr(worker, ifStatement->expr);
	AnalyzeStatement(worker, ifStatement->ifBody);
	if (ifStatement->elseBody != nullptr) {
		AnalyzeStatement(worker, ifStatement->elseBody);
	}
}

// NOTE / TODO / WOOF
// Should we maintain some list of epxressions for each worker
// this list of expressions can be used to do things followed by stuff
// except this become much more complicated than expected because you need to consider
// mutliple compound expressions and how they related to their coresponding statements!
// Bollocks!

void AnalyzeAST (Worker* worker) {
	for (auto i = 0; i < worker->currentScope->members.size(); i++) {
		auto node = worker->currentScope->members[i];
		if (node->nodeType == AST_FUNCTION) {
			auto function = (ASTFunction*)node;
			auto previousBlock = worker->currentScope;
			worker->currentScope = function;
			AnalyzeBlock(worker, function);
			worker->currentScope = previousBlock;
		}
	}
}
