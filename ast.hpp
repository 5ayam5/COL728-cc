#ifndef AST_H
#define AST_H

#include <vector>
#include <string>
#include <iostream>
#include <map>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

#define TABBING "  "

using namespace std;
using namespace llvm;

extern std::unique_ptr<LLVMContext> TheContext;
extern std::unique_ptr<Module> TheModule;
extern std::unique_ptr<IRBuilder<>> Builder;
extern std::map<std::string, Value *> NamedValues;

class Node {
public:
    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "Node (not implemented)";
        cout << '\n';
    }

    virtual Value *codegen() {
        return nullptr;
    }
};

class ExternalDeclaration : public Node {};

class StringType : public Node {
private:
    string id, value;

public:
    StringType(string id, string value = "") : id(id), value(value) {}

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << id << ' ' << value;
        cout << '\n';
    }

    string getValue() {
        return value;
    }
};

class DeclarationSpecifiers : public Node {
private:
    vector<StringType *> specifiers;

public:
    void add(StringType *specifier) { specifiers.push_back(specifier); }

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "DeclarationSpecifiers";
        cout << '\n';
        for (auto specifier : specifiers)
            specifier->print(depth + 1);
    }

    Type *getType() {
        for (auto specifier : specifiers) {
            if (specifier->getValue() == "FLOAT")
                return Type::getFloatTy(*TheContext);
            else if (specifier->getValue() == "DOUBLE")
                return Type::getDoubleTy(*TheContext);
            else if (specifier->getValue() == "CHAR")
                return Type::getInt8Ty(*TheContext);
            else if (specifier->getValue() == "VOID")
                return Type::getVoidTy(*TheContext);
            else if (specifier->getValue() == "BOOL")
                return Type::getInt1Ty(*TheContext);
            else if (specifier->getValue() == "SHORT")
                return Type::getInt16Ty(*TheContext);
            else if (specifier->getValue() == "INT")
                return Type::getInt32Ty(*TheContext);
            else if (specifier->getValue() == "LONG")
                return Type::getInt64Ty(*TheContext);
            else if (specifier->getValue() == "UNSIGNED")
                return Type::getInt32Ty(*TheContext);
            else if (specifier->getValue() == "SIGNED")
                return Type::getInt32Ty(*TheContext);
        }
        return nullptr;
    }

    vector<StringType *> getSpecifiers() { return specifiers; }
};

class TypeQualifierList : public Node {
private:
    vector<StringType *> typeQualifiers;

public:
    void add(StringType *typeQualifier) { typeQualifiers.push_back(typeQualifier); }

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "TypeQualifierList";
        cout << '\n';
        for (auto typeQualifier : typeQualifiers)
            typeQualifier->print(depth + 1);
    }
};

class Pointer : public Node {
private:
    TypeQualifierList *typeQualifierList;
    Pointer *childPointer;

public:
    Pointer(TypeQualifierList *typeQualifierList,
            Pointer *childPointer)
    : typeQualifierList(typeQualifierList), childPointer(childPointer) {}
    Pointer() : typeQualifierList(nullptr), childPointer(nullptr) {}

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "Pointer";
        cout << '\n';
        if (typeQualifierList != nullptr)
            typeQualifierList->print(depth + 1);
        if (childPointer != nullptr)
            childPointer->print(depth + 1);
    }

    Type *getType(Type *type) {
        if (childPointer == nullptr)
            return type;
        else
            return childPointer->getType(PointerType::get(type, 0));
    }
};

class Declarator;

class AbstractDeclarator;

class ParameterDeclaration : public Node {
private:
    DeclarationSpecifiers *declarationSpecifiers;
    union {
        Declarator *declarator;
        AbstractDeclarator *abstractDeclarator;
    };
    uint8_t type;

public:
    ParameterDeclaration(DeclarationSpecifiers *declarationSpecifiers, Declarator *declarator) : declarationSpecifiers(declarationSpecifiers), declarator(declarator), type(1) {}
    ParameterDeclaration(DeclarationSpecifiers *declarationSpecifiers, AbstractDeclarator *abstractDeclarator)
    : declarationSpecifiers(declarationSpecifiers), abstractDeclarator(abstractDeclarator), type(2) {}
    ParameterDeclaration(DeclarationSpecifiers *declarationSpecifiers) : declarationSpecifiers(declarationSpecifiers), type(0) {}

    virtual void print(int depth);

    string getIdentifier();

    Type *getType();

    bool isVariadic() {
        for (auto specifier : declarationSpecifiers->getSpecifiers()) {
            if (specifier->getValue() == "ELLIPSIS")
                return true;
        }
        return false;
    }
};

class ParameterTypeList : public Node {
private:
    vector<ParameterDeclaration *> parameterDeclarations;

public:
    void add(ParameterDeclaration *parameterDeclaration) { parameterDeclarations.push_back(parameterDeclaration); }

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "ParameterTypeList";
        cout << '\n';
        for (auto parameterDeclaration : parameterDeclarations)
            parameterDeclaration->print(depth + 1);
    }

    vector<ParameterDeclaration *> getParameterDeclarations() {
        return parameterDeclarations;
    }
};

class IdentifierList : public Node {
private:
    vector<StringType *> identifiers;

public:
    void add(StringType *identifier) { identifiers.push_back(identifier); }

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "IdentifierList";
        cout << '\n';
        for (auto identifier : identifiers)
            identifier->print(depth + 1);
    }

    vector<StringType *> getIdentifiers() {
        return identifiers;
    }
};

class DirectDeclaratorUtil : public Node {
private:
    union {
        ParameterTypeList *parameterTypeList;
        IdentifierList *identifierList;
    };
    uint8_t type;

public:
    DirectDeclaratorUtil(ParameterTypeList *parameterTypeList) : parameterTypeList(parameterTypeList), type(1) {}
    DirectDeclaratorUtil(IdentifierList *identifierList) : identifierList(identifierList), type(2) {}
    DirectDeclaratorUtil() : type(0) {}

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "DirectDeclaratorUtil";
        cout << '\n';
        switch (type) {
        case 1:
            if (parameterTypeList != nullptr)
                parameterTypeList->print(depth + 1);
            break;
        case 2:
            if (identifierList != nullptr)
                identifierList->print(depth + 1);
            break;
        default:
            break;
        }
    }

    ParameterTypeList *getParameterTypeList() {
        return parameterTypeList;
    }

    IdentifierList *getIdentifierList() {
        return identifierList;
    }
};

class DirectDeclarator : public Node {
private:
    union {
        StringType *stringType;
        Declarator *declarator;
        struct {
            DirectDeclarator *directDeclarator;
            DirectDeclaratorUtil *ddUtil;
        };
    };
    uint8_t type;

public:
    DirectDeclarator(StringType *stringType) : stringType(stringType), type(1) {}
    DirectDeclarator(Declarator *declarator) : declarator(declarator), type(2) {}
    DirectDeclarator(DirectDeclarator *directDeclarator, DirectDeclaratorUtil *ddUtil) : directDeclarator(directDeclarator), ddUtil(ddUtil), type(3) {}
    DirectDeclarator() : type(0) {}

    virtual void print(int depth);

    string getName() {
        if (type == 1)
            return stringType->getValue();
        if (type == 3)
            return directDeclarator->getName();
        else
            return "";
    }

    IdentifierList *getIdentifierList() {
        if (type == 3)
            return ddUtil->getIdentifierList();
        else
            return nullptr;
    }

    ParameterTypeList *getParameterTypeList() {
        if (type == 3 && ddUtil)
            return ddUtil->getParameterTypeList();
        else
            return nullptr;
    }

    uint8_t getASTType() { return type; }
};

class Declarator : public Node {
private:
    Pointer *pointer;
    DirectDeclarator *directDeclarator;

public:
    Declarator(Pointer *pointer, DirectDeclarator *directDeclarator)
    : pointer(pointer), directDeclarator(directDeclarator) {}

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "Declarator";
        cout << '\n';
        if (pointer != nullptr)
            pointer->print(depth + 1);
        if (directDeclarator != nullptr)
            directDeclarator->print(depth + 1);
    }

    DirectDeclarator *getDirectDeclarator() { return directDeclarator; }

    Type *getType(Type *type) {
        if (pointer == nullptr)
            return type;
        return pointer->getType(PointerType::get(type, 0));
    }
};

class Initializer : public Node {};

class InitDeclarator : public Node {
private:
    Declarator *declarator;
    Initializer *initializer;

public:
    InitDeclarator(Declarator *declarator, Initializer *initializer) : declarator(declarator), initializer(initializer) {}

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "InitDeclarator";
        cout << '\n';
        if (declarator != nullptr)
            declarator->print(depth + 1);
        if (initializer != nullptr)
            initializer->print(depth + 1);
    }

    Declarator *getDeclarator() { return declarator; }
};

class InitDeclaratorList : public Node {
private:
    vector<InitDeclarator *> initDeclarators;

public:
    void add(InitDeclarator *initDeclarator) { initDeclarators.push_back(initDeclarator); }

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "InitDeclaratorList";
        cout << '\n';
        for (auto initDeclarator : initDeclarators)
            initDeclarator->print(depth + 1);
    }

    vector<InitDeclarator *> getInitDeclarators() {
        return initDeclarators;
    }
};

class StaticAssertDeclaration : public Node {};

class Declaration : public ExternalDeclaration {
private:
    union {
        struct {
            DeclarationSpecifiers *declarationSpecifiers;
            InitDeclaratorList *initDeclaratorList;
        };
        StaticAssertDeclaration *staticAssertDeclaration;
    };
    uint8_t type;

public:
    Declaration(DeclarationSpecifiers *declarationSpecifiers, InitDeclaratorList *initDeclaratorList) : declarationSpecifiers(declarationSpecifiers), initDeclaratorList(initDeclaratorList), type(1) {}
    Declaration(StaticAssertDeclaration *staticAssertDeclaration) : staticAssertDeclaration(staticAssertDeclaration), type(2) {}
    Declaration() : type(0) {}

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "Declaration";
        cout << '\n';
        switch (type) {
        case 1:
            if (declarationSpecifiers != nullptr)
                declarationSpecifiers->print(depth + 1);
            if (initDeclaratorList != nullptr)
                initDeclaratorList->print(depth + 1);
            break;
        case 2:
            if (staticAssertDeclaration != nullptr)
                staticAssertDeclaration->print(depth + 1);
            break;
        default:
            break;
        }
    }

    virtual Value *codegen() {
        // cout << "Codegen for Declaration" << '\n';
        Type *type = declarationSpecifiers->getType();
        Value *last;
        for (auto initDeclarator : initDeclaratorList->getInitDeclarators()) {
            Declarator *declarator = initDeclarator->getDeclarator();
            string name = declarator->getDirectDeclarator()->getName();
            if (name == "")
                continue;
            bool isVariable = declarator->getDirectDeclarator()->getASTType() == 1;
            if (isVariable)
                Builder->CreateAlloca(type, nullptr, name);
            else {
                vector<Type *> argTypes;
                bool isVariadic = false;
                for (auto parameterDeclaration : declarator->getDirectDeclarator()->getParameterTypeList()->getParameterDeclarations()) {
                    Type *argType = parameterDeclaration->getType();
                    if (argType != nullptr)
                        argTypes.push_back(argType);
                    isVariadic |= parameterDeclaration->isVariadic();
                }
                Function *function = Function::Create(FunctionType::get(type, argTypes, isVariadic), Function::ExternalLinkage, name, TheModule.get());

                unsigned i = 0;
                for (auto &arg : function->args())
                    arg.setName(declarator->getDirectDeclarator()->getParameterTypeList()->getParameterDeclarations()[i++]->getIdentifier());
                
                last = function;
            }
        }
        return last;
    }
};

class DeclarationList : public Node {
private:
    vector<Declaration *> declarations;

public:
    void add(Declaration *declaration) { declarations.push_back(declaration); }

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "DeclarationList";
        cout << '\n';
        for (auto declaration : declarations)
            declaration->print(depth + 1);
    }
};

class LabeledStatement : public Node {};

class ExpressionStatement;

class AssignmentExpression;

class TypeName;

class GenericAssociation : public Node {
private:
    TypeName *typeName;
    AssignmentExpression *assignmentExpression;

public:
    GenericAssociation(TypeName *typeName, AssignmentExpression *assignmentExpression) : typeName(typeName), assignmentExpression(assignmentExpression) {}

    virtual void print(int depth);
};

class GenericAssocList : public Node {
private:
    vector<GenericAssociation *> genericAssociations;

public:
    void add(GenericAssociation *genericAssociation) { genericAssociations.push_back(genericAssociation); }

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "GenericAssocList";
        cout << '\n';
        for (auto genericAssociation : genericAssociations)
            genericAssociation->print(depth + 1);
    }
};

class GenericSelection : public Node {
private:
    AssignmentExpression *assignmentExpression;
    GenericAssocList *genericAssocList;

public:
    GenericSelection(AssignmentExpression *assignmentExpression, GenericAssocList *genericAssocList) {}

    virtual void print(int depth);
};

class PrimaryExpression : public Node {
private:
    union {
        StringType *stringType;
        ExpressionStatement *expressionStatement;
        GenericSelection *genericSelection;
    };
    uint8_t type;

public:
    PrimaryExpression(StringType *stringType, uint8_t type) : stringType(stringType), type(type) {}
    PrimaryExpression(ExpressionStatement *expressionStatement) : expressionStatement(expressionStatement), type(4) {}
    PrimaryExpression(GenericSelection *genericSelection) : genericSelection(genericSelection), type(5) {}
    PrimaryExpression() : type(0) {}

    virtual void print(int depth);

    virtual Value *codegen();

    string getName() {
        switch (type) {
        case 1:
            return stringType->getValue();
        default:
            return "";
        }
    }
};

class ArgumentExpressionList : public Node {
private:
    vector<AssignmentExpression *> assignmentExpressions;

public:
    void add(AssignmentExpression *assignmentExpression) { assignmentExpressions.push_back(assignmentExpression); }

    virtual void print(int depth);

    size_t getSize() { return assignmentExpressions.size(); }

    vector<AssignmentExpression *> getAssignmentExpressions() { return assignmentExpressions; }
};

class PostfixExpression : public Node {
private:
    union {
        PrimaryExpression *primaryExpression;
        struct {
            PostfixExpression *postfixExpression;
            ArgumentExpressionList *argumentExpressionList;
        };
    };
    uint8_t type;

public:
    PostfixExpression(PrimaryExpression *primaryExpression) : primaryExpression(primaryExpression), type(1) {}
    PostfixExpression(PostfixExpression *postfixExpression, ArgumentExpressionList *argumentExpressionList) : postfixExpression(postfixExpression), argumentExpressionList(argumentExpressionList), type(2) {}
    PostfixExpression() : type(0) {}

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "PostfixExpression";
        cout << '\n';
        switch (type) {
        case 1:
            if (primaryExpression != nullptr)
                primaryExpression->print(depth + 1);
            break;
        case 2:
            if (postfixExpression != nullptr)
                postfixExpression->print(depth + 1);
            if (argumentExpressionList != nullptr)
                argumentExpressionList->print(depth + 1);
            break;
        default:
            break;
        }
    }

    virtual Value *codegen();

    string getName() {
        if (type == 1)
            return primaryExpression->getName();
        return postfixExpression->getName();
    }
};

class CastExpression;
class TypeName;

class UnaryExpression : public Node {
private:
    union {
        PostfixExpression *postfixExpression;
        struct {
            StringType *unaryOp;
            UnaryExpression *unaryExpression;
        };
        struct {
            StringType *castOp;
            CastExpression *castExpression;
        };
        struct {
            StringType *qualifier;
            TypeName *typeName;
        };
    };
    uint8_t type;

public:
    UnaryExpression(PostfixExpression *postfixExpression) : postfixExpression(postfixExpression), type(1) {}
    UnaryExpression(StringType *unaryOp, UnaryExpression *unaryExpression) : unaryOp(unaryOp), unaryExpression(unaryExpression), type(2) {}
    UnaryExpression(StringType *castOp, CastExpression *castExpression) : castOp(castOp), castExpression(castExpression), type(3) {}
    UnaryExpression(StringType *qualifier, TypeName *typeName) : qualifier(qualifier), typeName(typeName), type(4) {}
    UnaryExpression() : type(0) {}

    virtual void print(int depth);

    virtual Value *codegen() {
        // cout << "Codegen for UnaryExpression" << '\n';
        if (type == 1) {
            return postfixExpression->codegen();
        } else if (type == 2) {
            Value *unVal = unaryExpression->codegen();
            if (unaryOp->getValue() == "&")
                return nullptr;
            else if (unaryOp->getValue() == "*")
                return Builder->CreateLoad(unVal->getType(), unVal);
            else if (unaryOp->getValue() == "+")
                return unVal;
            else if (unaryOp->getValue() == "-")
                return Builder->CreateNeg(unVal);
            else if (unaryOp->getValue() == "~")
                return Builder->CreateNot(unVal);
            else if (unaryOp->getValue() == "!")
                return Builder->CreateNot(unVal);
            else if (unaryOp->getValue() == "++")
                return Builder->CreateAdd(unVal, ConstantInt::get(*TheContext, APInt(32, 1)));
            else if (unaryOp->getValue() == "--")
                return Builder->CreateSub(unVal, ConstantInt::get(*TheContext, APInt(32, 1)));
            else
                return nullptr;
        } else
            return nullptr;
    }
};

class SpecifierQualifierList : public Node {
private:
    vector<StringType *> sqList; // specifier-qualifier List

public:
    void add(StringType *stringType) { sqList.push_back(stringType); }

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "SpecifierQualifierList";
        cout << '\n';
        for (auto stringType : sqList)
            stringType->print(depth + 1);
    }
};

class DirectAbstractDeclarator : public Node {};

class AbstractDeclarator : public Node {
private:
    Pointer *pointer;
    DirectAbstractDeclarator *directAbstractDeclarator;

public:
    AbstractDeclarator(Pointer *pointer, DirectAbstractDeclarator *directAbstractDeclarator) :
    pointer(pointer), directAbstractDeclarator(directAbstractDeclarator) {}

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "AbstractDeclarator";
        cout << '\n';
        if (pointer != nullptr)
            pointer->print(depth + 1);
        if (directAbstractDeclarator != nullptr)
            directAbstractDeclarator->print(depth + 1);
    }
};

class TypeName : public Node {
private:
    SpecifierQualifierList *specifierQualifierList;
    AbstractDeclarator *abstractDeclarator;

public:
    TypeName(SpecifierQualifierList *specifierQualifierList, AbstractDeclarator *abstractDeclarator)
    : specifierQualifierList(specifierQualifierList), abstractDeclarator(abstractDeclarator) {}

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "AbstractDeclarator";
        cout << '\n';
        if (specifierQualifierList != nullptr)
            specifierQualifierList->print(depth + 1);
        if (abstractDeclarator != nullptr)
            abstractDeclarator->print(depth + 1);
    }
};

class CastExpression : public Node {
private:
    union {
        UnaryExpression *unaryExpression;
        struct {
            TypeName *typeName;
            CastExpression *castExpression;
        };
    };
    uint8_t type;

public:
    CastExpression(UnaryExpression *unaryExpression) : unaryExpression(unaryExpression), type(1) {}
    CastExpression(TypeName *typeName, CastExpression *castExpression) : typeName(typeName), castExpression(castExpression), type(2) {}
    CastExpression() : type(0) {}

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "CastExpression";
        cout << '\n';
        switch (type) {
        case 1:
            if (unaryExpression != nullptr)
                unaryExpression->print(depth + 1);
            break;
        case 2:
            if (typeName != nullptr)
                typeName->print(depth + 1);
            if (castExpression != nullptr)
                castExpression->print(depth + 1);
            break;
        default:
            break;
        }
    }

    virtual Value *codegen() {
        if (type == 1)
            return unaryExpression->codegen();
        else
            return nullptr;
    }
};

class BinaryExpression : public Node {
private:
    union {
        struct {
            BinaryExpression *operand1;
            StringType *operator_;
            BinaryExpression *operand2;
        };
        CastExpression *castExpression;
    };
    uint8_t type;

public:
    BinaryExpression(BinaryExpression *operand1, StringType *operator_, BinaryExpression *operand2) : operand1(operand1), operator_(operator_), operand2(operand2), type(1) {}
    BinaryExpression(CastExpression *castExpression) : castExpression(castExpression), type(2) {}
    BinaryExpression() : type(0) {}

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "BinaryExpression";
        cout << '\n';
        switch (type) {
        case 1:
            if (operand1 != nullptr)
                operand1->print(depth + 1);
            if (operator_ != nullptr)
                operator_->print(depth + 1);
            if (operand2 != nullptr)
                operand2->print(depth + 1);
            break;
        case 2:
            if (castExpression != nullptr)
                castExpression->print(depth + 1);
            break;
        default:
            break;
        }
    }

    virtual Value *codegen() {
        if (type == 1) {
            Value *op1 = operand1->codegen();
            Value *op2 = operand2->codegen();
            if (op1 == nullptr || op2 == nullptr)
                return nullptr;
            if (operator_->getValue() == "+")
                return Builder->CreateAdd(op1, op2);
            else if (operator_->getValue() == "-")
                return Builder->CreateSub(op1, op2);
            else if (operator_->getValue() == "*")
                return Builder->CreateMul(op1, op2);
            else if (operator_->getValue() == "/")
                return Builder->CreateSDiv(op1, op2);
            else if (operator_->getValue() == "%")
                return Builder->CreateSRem(op1, op2);
            else if (operator_->getValue() == "<<")
                return Builder->CreateShl(op1, op2);
            else if (operator_->getValue() == ">>")
                return Builder->CreateAShr(op1, op2);
            else if (operator_->getValue() == "<")
                return Builder->CreateICmpSLT(op1, op2);
            else if (operator_->getValue() == ">")
                return Builder->CreateICmpSGT(op1, op2);
            else if (operator_->getValue() == "<=")
                return Builder->CreateICmpSLE(op1, op2);
            else if (operator_->getValue() == ">=")
                return Builder->CreateICmpSGE(op1, op2);
            else if (operator_->getValue() == "==")
                return Builder->CreateICmpEQ(op1, op2);
            else if (operator_->getValue() == "!=")
                return Builder->CreateICmpNE(op1, op2);
            else if (operator_->getValue() == "&")
                return Builder->CreateAnd(op1, op2);
            else if (operator_->getValue() == "^")
                return Builder->CreateXor(op1, op2);
            else if (operator_->getValue() == "|")
                return Builder->CreateOr(op1, op2);
            else if (operator_->getValue() == "&&")
                return Builder->CreateAnd(op1, op2);
            else if (operator_->getValue() == "||")
                return Builder->CreateOr(op1, op2);
            else
                return nullptr;
        } else if (type == 2)
            return castExpression->codegen();
        else
            return nullptr;
    }
};

class ExpressionStatement;

class ConditionalExpression : public Node {
private:
    BinaryExpression *binaryExpression;
    ExpressionStatement *expressionStatement;
    ConditionalExpression *conditionalExpression;

public:
    ConditionalExpression(BinaryExpression *binaryExpression, ExpressionStatement *expressionStatement, ConditionalExpression *conditionalExpression)
    : binaryExpression(binaryExpression), expressionStatement(expressionStatement), conditionalExpression(conditionalExpression) {}

    virtual void print(int depth);

    virtual Value *codegen();
};

class AssignmentExpression : public Node {
private:
    union {
        struct {
            UnaryExpression *unaryExpression;
            StringType *assignmentOperator;
            AssignmentExpression *assignmentExpression;
        };
        ConditionalExpression *conditionalExpression;
    };
    uint8_t type;

public:
    AssignmentExpression(UnaryExpression *unaryExpression, StringType *assignmentOperator, AssignmentExpression *assignmentExpression)
    : unaryExpression(unaryExpression), assignmentOperator(assignmentOperator), assignmentExpression(assignmentExpression), type(1) {}
    AssignmentExpression(ConditionalExpression *conditionalExpression) : conditionalExpression(conditionalExpression), type(2) {}
    AssignmentExpression() : type(0) {}

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "AssignmentExpression";
        cout << '\n';
        switch (type) {
        case 1:
            if (unaryExpression != nullptr)
                unaryExpression->print(depth + 1);
            if (assignmentOperator != nullptr)
                assignmentOperator->print(depth + 1);
            if (assignmentExpression != nullptr)
                assignmentExpression->print(depth + 1);
            break;
        case 2:
            if (conditionalExpression != nullptr)
                conditionalExpression->print(depth + 1);
            break;
        default:
            break;
        }
    }

    virtual Value *codegen() {
        // cout << "Codegen for AssignmentExpression" << '\n';
        if (type == 1) {
            Value *lval = unaryExpression->codegen();
            Value *rval = assignmentExpression->codegen();
            if (lval == nullptr || rval == nullptr)
                return nullptr;
            else if (assignmentOperator->getValue() == "ASSIGN")
                return Builder->CreateStore(rval, lval);
            else if (assignmentOperator->getValue() == "MUL_ASSIGN")
                return Builder->CreateStore(Builder->CreateMul(lval, rval), lval);
            else if (assignmentOperator->getValue() == "DIV_ASSIGN")
                return Builder->CreateStore(Builder->CreateSDiv(lval, rval), lval);
            else if (assignmentOperator->getValue() == "MOD_ASSIGN")
                return Builder->CreateStore(Builder->CreateSRem(lval, rval), lval);
            else if (assignmentOperator->getValue() == "ADD_ASSIGN")
                return Builder->CreateStore(Builder->CreateAdd(lval, rval), lval);
            else if (assignmentOperator->getValue() == "SUB_ASSIGN")
                return Builder->CreateStore(Builder->CreateSub(lval, rval), lval);
            else if (assignmentOperator->getValue() == "LEFT_ASSIGN")
                return Builder->CreateStore(Builder->CreateShl(lval, rval), lval);
            else if (assignmentOperator->getValue() == "RIGHT_ASSIGN")
                return Builder->CreateStore(Builder->CreateAShr(lval, rval), lval);
            else if (assignmentOperator->getValue() == "AND_ASSIGN")
                return Builder->CreateStore(Builder->CreateAnd(lval, rval), lval);
            else if (assignmentOperator->getValue() == "XOR_ASSIGN")
                return Builder->CreateStore(Builder->CreateXor(lval, rval), lval);
            else if (assignmentOperator->getValue() == "OR_ASSIGN")
                return Builder->CreateStore(Builder->CreateOr(lval, rval), lval);
            else
                return nullptr;
        } else if (type == 2)
            return conditionalExpression->codegen();
        else
            return nullptr;
    }
};

class ExpressionStatement : public Node {
private:
    vector<AssignmentExpression *> expressions;

public:
    void add(AssignmentExpression *assignmentExpression) { expressions.push_back(assignmentExpression); }

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "ExpressionStatement";
        cout << '\n';
        for (auto expression : expressions)
            expression->print(depth + 1);
    }

    virtual Value *codegen() {
        // cout << "Codegen for ExpressionStatement" << '\n';
        Value *last = nullptr;
        for (auto expression : expressions)
            last = expression->codegen();
        return last;
    }
};

class BlockItem;

class Statement;

class CompoundStatement : public Node {
private:
    vector<BlockItem *> blockItems;

public:
    void add(BlockItem *blockItem);

    virtual void print(int depth);

    virtual Value *codegen();
};

class SelectionStatement : public Node {
private:
    union {
        struct {
            ExpressionStatement *iteExpressionStatement;
            Statement *ifStatement;
            Statement *elseStatement;
        };
        struct {
            ExpressionStatement *switchExpressionStatement;
            Statement *switchStatement;
        };
    };
    uint8_t type;

public:
    SelectionStatement(ExpressionStatement *iteExpressionStatement, Statement *ifStatement, Statement *elseStatement)
    : iteExpressionStatement(iteExpressionStatement), ifStatement(ifStatement), elseStatement(elseStatement), type(1) {}
    SelectionStatement(ExpressionStatement *switchExpressionStatement, Statement *switchStatement)
    : switchExpressionStatement(switchExpressionStatement), switchStatement(switchStatement), type(2) {}
    SelectionStatement() : type(0) {}

    virtual void print(int depth);

    virtual Value *codegen();
};

class IterationStatement : public Node {
private:
    union {
        struct {
            ExpressionStatement *whileExpressionStatement;
            Statement *whileStatement;
        };
        struct {
            ExpressionStatement *doExpressionStatement;
            Statement *doStatement;
        };
        struct {
            union {
                Declaration *forDeclaration;
                ExpressionStatement *forExpressionStatement;
            };
            ExpressionStatement *forConditional;
            ExpressionStatement *forIncrement;
            Statement *forStatement;
        };
    };
    uint8_t type;

public:
    IterationStatement(ExpressionStatement *whileExpressionStatement, Statement *whileStatement)
    : whileExpressionStatement(whileExpressionStatement), whileStatement(whileStatement), type(1) {}
    IterationStatement(Statement *doStatement, ExpressionStatement *doExpressionStatement)
    : doExpressionStatement(doExpressionStatement), doStatement(doStatement), type(2) {}
    IterationStatement(Declaration *forDeclaration, ExpressionStatement *forConditional, ExpressionStatement *forIncrement, Statement *forStatement)
    : forDeclaration(forDeclaration), forConditional(forConditional), forIncrement(forIncrement), forStatement(forStatement), type(3) {}
    IterationStatement(ExpressionStatement *forExpressionStatement, ExpressionStatement *forConditional, ExpressionStatement *forIncrement, Statement *forStatement)
    : forExpressionStatement(forExpressionStatement), forConditional(forConditional), forIncrement(forIncrement), forStatement(forStatement), type(4) {}
    IterationStatement() : type(0) {}

    virtual void print(int depth);

    virtual Value *codegen();
};

class JumpStatement : public Node {
private:
    StringType *jumpType;
    ExpressionStatement *expressionStatement;

public:
    JumpStatement(StringType *jumpType, ExpressionStatement *expressionStatement = nullptr) : jumpType(jumpType), expressionStatement(expressionStatement) {}

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "JumpStatement";
        cout << '\n';
        if (jumpType != nullptr)
            jumpType->print(depth + 1);
        if (expressionStatement != nullptr)
            expressionStatement->print(depth + 1);
    }

    virtual Value *codegen() {
        // cout << "Codegen for JumpStatement" << '\n';
        if (jumpType->getValue() == "return") {
            if (expressionStatement != nullptr) {
                Value *value = expressionStatement->codegen();
                if (value == nullptr)
                    return nullptr;
                return Builder->CreateRet(value);
            }
            return Builder->CreateRetVoid();
        } else
            return nullptr;
    }
};

class Statement : public Node {
private:
    union {
        LabeledStatement *labeled;
        CompoundStatement *compound;
        ExpressionStatement *expression;
        SelectionStatement *selection;
        IterationStatement *iteration;
        JumpStatement *jump;
    };
    uint8_t type;

public:
    Statement(LabeledStatement *labeled) : labeled(labeled), type(1) {}
    Statement(CompoundStatement *compound) : compound(compound), type(2) {}
    Statement(ExpressionStatement *expression) : expression(expression), type(3) {}
    Statement(SelectionStatement *selection) : selection(selection), type(4) {}
    Statement(IterationStatement *iteration) : iteration(iteration), type(5) {}
    Statement(JumpStatement *jump) : jump(jump), type(6) {}
    Statement() : type(0) {}

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "Statement";
        cout << '\n';
        switch (type) {
        case 1:
            if (labeled != nullptr)
                labeled->print(depth + 1);
            break;
        case 2:
            if (compound != nullptr)
                compound->print(depth + 1);
            break;
        case 3:
            if (expression != nullptr)
                expression->print(depth + 1);
            break;
        case 4:
            if (selection != nullptr)
                selection->print(depth + 1);
            break;
        case 5:
            if (iteration != nullptr)
                iteration->print(depth + 1);
            break;
        case 6:
            if (jump != nullptr)
                jump->print(depth + 1);
            break;
        default:
            break;
        }
    }

    virtual Value *codegen() {
        switch (type) {
        case 1:
            return labeled->codegen();
        case 2:
            return compound->codegen();
        case 3:
            return expression->codegen();
        case 4:
            return selection->codegen();
        case 5:
            return iteration->codegen();
        case 6:
            return jump->codegen();
        default:
            return nullptr;
        }
    }
};

class BlockItem : public Node {
private:
    union {
        Declaration *declaration;
        Statement *statement;
    };
    uint8_t type;

public:
    BlockItem(Declaration *declaration) : declaration(declaration), type(1) {}
    BlockItem(Statement *statement) : statement(statement), type(2) {}
    BlockItem() : type(0) {}

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "BlockItem";
        cout << '\n';
        switch (type) {
        case 1:
            if (declaration != nullptr)
                declaration->print(depth + 1);
            break;
        case 2:
            if (statement != nullptr)
                statement->print(depth + 1);
            break;
        default:
            break;
        }
    }

    virtual Value *codegen() {
        // cout << "Codegen for BlockItem" << '\n';
        switch (type) {
        case 1:
            return declaration->codegen();
        case 2:
            return statement->codegen();
        default:
            return nullptr;
        }
    }
};

class FunctionDefinition : public ExternalDeclaration {
private:
    DeclarationSpecifiers *declarationSpecifiers;
    Declarator *declarator;
    DeclarationList *declarationList;
    CompoundStatement *compoundStatement;

public:
    FunctionDefinition(DeclarationSpecifiers *declarationSpecifiers,
                       Declarator *declarator,
                       DeclarationList *declarationList,
                       CompoundStatement *compoundStatement)
    : declarationSpecifiers(declarationSpecifiers), declarator(declarator),
      declarationList(declarationList), compoundStatement(compoundStatement) {}

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "FunctionDefinition";
        cout << '\n';
        if (declarationSpecifiers != nullptr)
            declarationSpecifiers->print(depth + 1);
        if (declarator != nullptr)
            declarator->print(depth + 1);
        if (declarationList != nullptr)
            declarationList->print(depth + 1);
        if (compoundStatement != nullptr)
            compoundStatement->print(depth + 1);
    }

    virtual Value *codegen() {
        // cout << "Codegen for FunctionDefinition" << '\n';
        Type *returnType = declarationSpecifiers->getType();
        DirectDeclarator *directDeclarator = declarator->getDirectDeclarator();
        string funcName = directDeclarator->getName();
        ParameterTypeList *parameterTypeList = directDeclarator->getParameterTypeList();

        Function *function = TheModule->getFunction(funcName);

        if (function == nullptr) {
            vector<Type *> argTypes;
            bool isVariadic = false;
            if (parameterTypeList != nullptr) {
                for (auto &parameterDeclaration : parameterTypeList->getParameterDeclarations()) {
                    argTypes.push_back(parameterDeclaration->getType());
                    isVariadic |= parameterDeclaration->isVariadic();
                }
            }

            FunctionType *functionType = FunctionType::get(returnType, argTypes, isVariadic);
            function = Function::Create(functionType, Function::ExternalLinkage, funcName, TheModule.get());
        }

        BasicBlock *basicBlock = BasicBlock::Create(*TheContext, "entry", function);
        Builder->SetInsertPoint(basicBlock);

        NamedValues.clear();
        unsigned i = 0;
        for (auto &arg : function->args()) {
            arg.setName(parameterTypeList->getParameterDeclarations()[i]->getIdentifier());
            NamedValues[arg.getName().str()] = &arg;
            i++;
        }

        Value *retValue = compoundStatement->codegen();
        Builder->CreateRet(retValue);
        verifyFunction(*function);
        return function;
    }
};

extern vector<ExternalDeclaration *> external_declarations;

void dump_ast();

void dump_ir();

#endif
