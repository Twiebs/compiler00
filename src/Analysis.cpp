void ReportError (Worker* worker, FileSite& site, const std::string& msg);
void ReportError (Worker* worker, const std::string& msg);

internal void AnalyzeStatement(Worker* worker, ASTNode* node);
internal void AnalyzeExpr(Worker* worker, ASTExpression* expr);

internal inline void AnalyzeBlock(Worker* worker, ASTBlock* block);
internal void AnalyzeIfStatement (Worker* worker, ASTIfStatement* ifStatement);
internal void AnalyzeCall(Worker* worker, ASTCall* call);

internal inline bool TypeCompare(ASTExpression* exprA, ASTExpression* exprB);
internal inline bool TypeCheck(ASTExpression* expr, ASTDefinition* typedefn);

// TODO these resolve call functions should not be required to take a worker
// except mabye they do?
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
internal inline bool TypeCompare(ASTExpression* exprA, ASTExpression* exprB) {
	if (exprA->type != exprB->type)
		return false;
	return true;
}

internal inline bool TypeCheck(ASTExpression* expr, ASTDefinition* typedefn) {
	if (expr->type != typedefn)
		return false;
	return true;
}

internal inline void AnalyzeBlock(Worker* worker, ASTBlock* block) {
	for(auto i = 0; i < block->members.size(); i++) {
		auto node = block->members[i];
		AnalyzeStatement(worker, node);
	}
}

internal void AnalyzeStatement(Worker* worker, ASTNode* node) {
	switch(node->nodeType) {
	case AST_BLOCK:
		AnalyzeBlock(worker, (ASTBlock*)node);
		break;

	case AST_IF:
		AnalyzeIfStatement(worker, (ASTIfStatement*)node);
		break;
	case AST_CALL:
		AnalyzeCall(worker, (ASTCall*)node);
		break;

	case AST_VARIABLE: {
		auto var = static_cast<ASTVariable*>(node);
		if (var->initalExpression != nullptr) {
			AnalyzeExpr(worker, var->initalExpression);
		}

		if (var->type == nullptr) {
			var->type = var->initalExpression->type;
		} else if (var->initalExpression != nullptr){
			if(!TypeCheck(var->initalExpression, var->type)) {
				ReportError(worker, "Type mismatch...: (insert clever and useful error here)");
			}
		} else {
			// Nothing to do here this is just a declaration without an expression which has a type and is auto initialized
		}
	} break;
	case AST_VARIABLE_OPERATION: {
		auto varop = (ASTVariableOperation*)node;
		AnalyzeExpr(worker, varop->value);
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

internal void AnalyzeIfStatement (Worker* worker, ASTIfStatement* ifStatement) {
	assert(ifStatement->expr != nullptr);
	AnalyzeExpr(worker, ifStatement->expr);
	AnalyzeStatement(worker, ifStatement->ifBody);
	AnalyzeStatement(worker, ifStatement->elseBody);
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
      worker->currentScope = function;
			AnalyzeBlock(worker, function);
		}
	}
}
