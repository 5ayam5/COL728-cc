#include "ast.hpp"

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

void dump_ast() {
  for (auto external_declaration : external_declarations)
      external_declaration->print(0);
}
