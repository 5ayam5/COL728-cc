#ifndef AST_H
#define AST_H

#include <vector>
#include <string>
#include <iostream>

#define TABBING "  "

using namespace std;

class Node {
public:
    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "Node (not implemented)";
        cout << '\n';
    }
};
#define YYSTYPE Node*

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
};

class ParameterTypeList : public Node {
private:
    vector<ParameterDeclaration *> parameterDeclarations;

public:
    void add(ParameterDeclaration *parameterDeclaration) { parameterDeclarations.push_back(parameterDeclaration); }

    virtual void print(int depth) {
        for (int i = 0; i < depth; i++)
            cout << TABBING;
        cout << "IdentifierList";
        cout << '\n';
        for (auto parameterDeclaration : parameterDeclarations)
            parameterDeclaration->print(depth + 1);
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
    PrimaryExpression(StringType *stringType) : stringType(stringType), type(1) {}
    PrimaryExpression(ExpressionStatement *expressionStatement) : expressionStatement(expressionStatement), type(2) {}
    PrimaryExpression(GenericSelection *genericSelection) : genericSelection(genericSelection), type(3) {}
    PrimaryExpression() : type(0) {}

    virtual void print(int depth);
};

class PostfixExpression : public Node {
private:
    union {
        PrimaryExpression *primaryExpression;
    };
    uint8_t type;

public:
    PostfixExpression(PrimaryExpression *primaryExpression) : primaryExpression(primaryExpression), type(1) {}
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
        default:
            break;
        }
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
};

class BlockItem;

class CompoundStatement : public Node {
private:
    vector<BlockItem *> blockItems;

public:
    void add(BlockItem *blockItem);

    virtual void print(int depth);
};

class SelectionStatement : public Node {};

class IterationStatement : public Node {};

class JumpStatement : public Node {};

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
};

extern vector<ExternalDeclaration *> external_declarations;

void dump_ast();

#endif
