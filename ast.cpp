#include "ast.hpp"

std::unique_ptr<LLVMContext> TheContext;
std::unique_ptr<Module> TheModule;
std::unique_ptr<IRBuilder<>> Builder;
std::map<std::string, Value *> NamedValues;

vector<ExternalDeclaration *> external_declarations;

void CompoundStatement::add(BlockItem *blockItem) { blockItems.push_back(blockItem); }

void CompoundStatement::print(int depth) {
    for (int i = 0; i < depth; i++)
        cout << TABBING;
    cout << "CompoundStatement";
    cout << '\n';
    for (auto blockItem : blockItems)
        blockItem->print(depth + 1);
}

Value *CompoundStatement::codegen() {
    Value *last = nullptr;
    for (auto blockItem : blockItems)
        last = blockItem->codegen();
    return last;
}

void SelectionStatement::print(int depth) {
    for (int i = 0; i < depth; i++)
        cout << TABBING;
    cout << "SelectionStatement";
    cout << '\n';
    switch (type) {
    case 1:
        if (iteExpressionStatement != nullptr)
            iteExpressionStatement->print(depth + 1);
        if (ifStatement != nullptr)
            ifStatement->print(depth + 1);
        if (elseStatement != nullptr)
            elseStatement->print(depth + 1);
        break;
    case 2:
        if (switchExpressionStatement != nullptr)
            switchExpressionStatement->print(depth + 1);
        if (switchStatement != nullptr)
            switchStatement->print(depth + 1);
        break;
    default:
        break;
    }
}

Value *SelectionStatement::codegen() {
    if (type == 1) {
        Value *condV = iteExpressionStatement->codegen();
        if (!condV)
            return nullptr;

        Function *function = Builder->GetInsertBlock()->getParent();

        BasicBlock *thenBB = BasicBlock::Create(*TheContext, "then", function);
        BasicBlock *elseBB = BasicBlock::Create(*TheContext, "else");
        BasicBlock *mergeBB = BasicBlock::Create(*TheContext, "ifcont");

        Builder->CreateCondBr(condV, thenBB, elseBB);

        Builder->SetInsertPoint(thenBB);

        Value *thenV = ifStatement->codegen();
        if (!thenV)
            return nullptr;

        Builder->CreateBr(mergeBB);

        function->getBasicBlockList().push_back(elseBB);
        Builder->SetInsertPoint(elseBB);

        Value *elseV;
        if (elseStatement != nullptr) {
            elseV = elseStatement->codegen();
            if (!elseV)
                return nullptr;
        }

        Builder->CreateBr(mergeBB);
        elseBB = Builder->GetInsertBlock();

        function->getBasicBlockList().push_back(mergeBB);
        Builder->SetInsertPoint(mergeBB);

        return mergeBB->getTerminator();
    } else
        return nullptr;
}

void IterationStatement::print(int depth) {
    for (int i = 0; i < depth; i++)
        cout << TABBING;
    cout << "IterationStatement";
    cout << '\n';
    switch (type) {
    case 1:
        if (whileExpressionStatement != nullptr)
            whileExpressionStatement->print(depth + 1);
        if (whileStatement != nullptr)
            whileStatement->print(depth + 1);
        break;
    case 2:
        if (doExpressionStatement != nullptr)
            doExpressionStatement->print(depth + 1);
        if (doStatement != nullptr)
            doStatement->print(depth + 1);
        break;
    case 3:
    case 4:
        if (type == 3) {
            if (forDeclaration != nullptr)
                forDeclaration->print(depth + 1);
        } else {
            if (forExpressionStatement != nullptr)
                forExpressionStatement->print(depth + 1);
        }
        if (forConditional != nullptr)
            forConditional->print(depth + 1);
        if (forIncrement != nullptr)
            forIncrement->print(depth + 1);
        if (forStatement != nullptr)
            forStatement->print(depth + 1);
        break;
    default:
        break;
    }
}

Value *IterationStatement::codegen() {
    if (type == 1) {
        Function *function = Builder->GetInsertBlock()->getParent();

        BasicBlock *loopBB = BasicBlock::Create(*TheContext, "loop", function);
        BasicBlock *execBB = BasicBlock::Create(*TheContext, "exec");
        BasicBlock *afterBB = BasicBlock::Create(*TheContext, "afterloop");

        Builder->SetInsertPoint(loopBB);

        Value *condV = whileExpressionStatement->codegen();
        if (!condV)
            return nullptr;
        Builder->CreateCondBr(condV, execBB, afterBB);

        function->getBasicBlockList().push_back(execBB);
        Builder->SetInsertPoint(execBB);

        Value *execV = whileStatement->codegen();
        if (!execV)
            return nullptr;
        Builder->CreateBr(loopBB);

        function->getBasicBlockList().push_back(afterBB);
        Builder->SetInsertPoint(afterBB);
        return afterBB->getTerminator();
    } else
        return nullptr;
}

void ParameterDeclaration::print(int depth) {
    for (int i = 0; i < depth; i++)
        cout << TABBING;
    cout << "ParameterDeclaration";
    cout << '\n';
    if (declarationSpecifiers != nullptr)
        declarationSpecifiers->print(depth + 1);
    switch (type) {
    case 1:
        if (declarator != nullptr)
            declarator->print(depth + 1);
        break;
    case 2:
        if (abstractDeclarator != nullptr)
            abstractDeclarator->print(depth + 1);
        break;
    default:
        break;
    }
}

string ParameterDeclaration::getIdentifier() {
    if (type != 1)
        return "";
    return declarator->getDirectDeclarator()->getName();
}

Type *ParameterDeclaration::getType() {
    if (type == 1)
        return declarator->getType(declarationSpecifiers->getType());
    else
        return nullptr;
}

void DirectDeclarator::print(int depth) {
    for (int i = 0; i < depth; i++)
        cout << TABBING;
    cout << "DirectDeclarator";
    cout << '\n';
    switch (type) {
    case 1:
        if (stringType != nullptr)
            stringType->print(depth + 1);
        break;
    case 2:
        if (declarator != nullptr)
            declarator->print(depth + 1);
        break;
    case 3:
        if (directDeclarator != nullptr)
            directDeclarator->print(depth + 1);
        if (ddUtil != nullptr)
            ddUtil->print(depth + 1);
        break;
    default:
        break;
    }
}

void GenericAssociation::print(int depth) {
    for (int i = 0; i < depth; i++)
        cout << TABBING;
    cout << "GenericAssociation";
    cout << '\n';
    if (typeName != nullptr)
        typeName->print(depth + 1);
    if (assignmentExpression != nullptr)
        assignmentExpression->print(depth + 1);
}

void GenericSelection::print(int depth) {
    for (int i = 0; i < depth; i++)
        cout << TABBING;
    cout << "GenericSelection";
    cout << '\n';
    if (assignmentExpression != nullptr)
        assignmentExpression->print(depth + 1);
    if (genericAssocList != nullptr)
        genericAssocList->print(depth + 1);
}

void PrimaryExpression::print(int depth) {
    for (int i = 0; i < depth; i++)
        cout << TABBING;
    cout << "PrimaryExpression";
    cout << '\n';
    switch (type) {
    case 1:
        if (stringType != nullptr)
            stringType->print(depth + 1);
        break;
    case 2:
        if (expressionStatement != nullptr)
            expressionStatement->print(depth + 1);
        break;
    case 3:
        if (genericSelection != nullptr)
            genericSelection->print(depth + 1);
        break;
    default:
        break;
    }
}

Value *PrimaryExpression::codegen() {
    if (type == 1)
        return NamedValues[stringType->getValue()];
    else if (type == 2)
        return ConstantInt::get(*TheContext, APInt(32, stoi(stringType->getValue())));
    else if (type == 4)
        return expressionStatement->codegen();
    else
        return nullptr;

}

void ArgumentExpressionList::print(int depth) {
    for (int i = 0; i < depth; i++)
        cout << TABBING;
    cout << "ArgumentExpressionList";
    cout << '\n';
    for (auto assignmentExpression : assignmentExpressions)
        assignmentExpression->print(depth + 1);
}

Value *PostfixExpression::codegen() {
    if (type == 1)
        return primaryExpression->codegen();
    else if (type == 2) {
        Function *calleeFunction = TheModule->getFunction(postfixExpression->getName());
        if (calleeFunction == nullptr) {
            cout << "Unknown function referenced";
            return nullptr;
        } else if (calleeFunction->arg_size() != argumentExpressionList->getSize()) {
            cout << "Incorrect # arguments passed";
            return nullptr;
        }

        vector<Value *> argsV;
        for (auto &arg : argumentExpressionList->getAssignmentExpressions()) {
            argsV.push_back(arg->codegen());
            if (argsV.back() == nullptr)
                return nullptr;
        }
        
        return Builder->CreateCall(calleeFunction, argsV, "calltmp");
    } else
        return nullptr;
}

void UnaryExpression::print(int depth) {
    for (int i = 0; i < depth; i++)
        cout << TABBING;
    cout << "UnaryExpression";
    cout << '\n';
    switch (type) {
    case 1:
        if (postfixExpression != nullptr)
            postfixExpression->print(depth + 1);
        break;
    case 2:
        if (unaryOp != nullptr)
            unaryOp->print(depth + 1);
        if (unaryExpression != nullptr)
            unaryExpression->print(depth + 1);
        break;
    case 3:
        if (castOp != nullptr)
            castOp->print(depth + 1);
        if (castExpression != nullptr)
            castExpression->print(depth + 1);
        break;
    case 4:
        if (qualifier != nullptr)
            qualifier->print(depth + 1);
        if (typeName != nullptr)
            typeName->print(depth + 1);
        break;
    default:
        break;
    }
}

void ConditionalExpression::print(int depth) {
    for (int i = 0; i < depth; i++)
        cout << TABBING;
    cout << "ConditionalExpression";
    cout << '\n';
    if (binaryExpression != nullptr)
        binaryExpression->print(depth + 1);
    if (expressionStatement != nullptr)
        expressionStatement->print(depth + 1);
    if (conditionalExpression != nullptr)
        conditionalExpression->print(depth + 1);
}

Value *ConditionalExpression::codegen() {
    if (expressionStatement == nullptr && conditionalExpression == nullptr)
        return binaryExpression->codegen();

    Value *cond = binaryExpression->codegen();
    if (cond == nullptr)
        return nullptr;
    Function *function = Builder->GetInsertBlock()->getParent();
    BasicBlock *thenBB = BasicBlock::Create(*TheContext, "then", function);
    BasicBlock *elseBB = BasicBlock::Create(*TheContext, "else");
    BasicBlock *mergeBB = BasicBlock::Create(*TheContext, "ifcont");
    Builder->CreateCondBr(cond, thenBB, elseBB);
    Builder->SetInsertPoint(thenBB);
    Value *thenValue = expressionStatement->codegen();
    if (thenValue == nullptr)
        return nullptr;
    Builder->CreateBr(mergeBB);
    thenBB = Builder->GetInsertBlock();
    function->getBasicBlockList().push_back(elseBB);
    Builder->SetInsertPoint(elseBB);
    Value *elseValue = conditionalExpression->codegen();
    if (elseValue == nullptr)
        return nullptr;
    Builder->CreateBr(mergeBB);
    elseBB = Builder->GetInsertBlock();
    function->getBasicBlockList().push_back(mergeBB);
    Builder->SetInsertPoint(mergeBB);
    PHINode *phiNode = Builder->CreatePHI(Type::getInt32Ty(*TheContext), 2, "iftmp");
    phiNode->addIncoming(thenValue, thenBB);
    phiNode->addIncoming(elseValue, elseBB);
    return phiNode;
}

void dump_ast() {
  for (auto external_declaration : external_declarations)
      external_declaration->print(0);
}

void dump_ir() {
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("llvm codegen", *TheContext);

    Builder = std::make_unique<IRBuilder<>>(*TheContext);

    for (auto external_declaration : external_declarations)
        external_declaration->codegen();
    TheModule->print(errs(), nullptr);
}
