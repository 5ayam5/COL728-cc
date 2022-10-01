#ifndef AST_H
#define AST_H

#include <vector>
#include <string>

using namespace std;

class Node {};
#define YYSTYPE Node*

class ExternalDeclaration : public Node {};

class StringType : public Node {
private:
    string name;

public:
    StringType(string name) : name(name) {}
};

class DeclarationSpecifiers : public Node {
private:
    vector<StringType *> specifiers;

public:
    void add(StringType *specifier) { specifiers.push_back(specifier); }
};

class TypeQualifierList : public Node {
private:
    vector<StringType *> typeQualifiers;

public:
    void add(StringType *stringType) { typeQualifiers.push_back(stringType); }
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
};

class DirectDeclarator : public Node {};

class Declarator : public Node {
private:
    Pointer *pointer;
    DirectDeclarator *directDeclarator;

public:
    Declarator(Pointer *pointer, DirectDeclarator *directDeclarator)
    : pointer(pointer), directDeclarator(directDeclarator) {}
};

class Initializer : public Node {};

class InitDeclarator : public Node {
private:
    Declarator *declarator;
    Initializer *initializer;

public:
    InitDeclarator(Declarator *declarator, Initializer *initializer) : declarator(declarator), initializer(initializer) {}
};

class InitDeclaratorList : public Node {
private:
    vector<InitDeclarator *> initDeclarators;

public:
    void add(InitDeclarator *initDeclarator) { initDeclarators.push_back(initDeclarator); }
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
};

class DeclarationList : public Node {
private:
    vector<Declaration *> declarations;

public:
    void add(Declaration *declaration) { declarations.push_back(declaration); }
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
};

class GenericAssocList : public Node {
private:
    vector<GenericAssociation *> genericAssociations;

public:
    void add(GenericAssociation *genericAssociation) { genericAssociations.push_back(genericAssociation); }
};

class GenericSelection : public Node {
private:
    AssignmentExpression *assignmentExpression;
    GenericAssocList *genericAssocList;

public:
    GenericSelection(AssignmentExpression *assignmentExpression, GenericAssocList *genericAssocList) {}
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
};

class SpecifierQualifierList : public Node {
private:
    vector<StringType *> sqList; // specifier-qualifier List

public:
    void add(StringType *stringType) { sqList.push_back(stringType); }
};

class DirectAbstractDeclarator : public Node {};

class AbstractDeclarator : public Node {
private:
    Pointer *pointer;
    DirectAbstractDeclarator *directAbstractDeclarator;

public:
    AbstractDeclarator(Pointer *pointer, DirectAbstractDeclarator *directAbstractDeclarator) :
    pointer(pointer), directAbstractDeclarator(directAbstractDeclarator) {}
};

class TypeName : public Node {
private:
    SpecifierQualifierList *specifierQualifierList;
    AbstractDeclarator *abstractDeclarator;

public:
    TypeName(SpecifierQualifierList *specifierQualifierList, AbstractDeclarator *abstractDeclarator)
    : specifierQualifierList(specifierQualifierList), abstractDeclarator(abstractDeclarator) {}
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
};

class ExpressionStatement : public Node {
private:
    vector<AssignmentExpression *> expressions;

public:
    void add(AssignmentExpression *assignmentExpression) { expressions.push_back(assignmentExpression); }
};

class BlockItem;

class CompoundStatement : public Node {
private:
    vector<BlockItem *> blockItemList;

public:
    void add(BlockItem *blockItem);
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
};

class BlockItem : public Node {
private:
    union {
        Declaration *declaration;
        Statement *statement;
    };
    bool isDeclaration;

public:
    BlockItem(Declaration *declaration) : declaration(declaration), isDeclaration(true) {}
    BlockItem(Statement *statement) : statement(statement), isDeclaration(false) {}
};

class FunctionDefinition : public ExternalDeclaration {
private:
    DeclarationSpecifiers *declarationSpecifers;
    Declarator *declarator;
    DeclarationList *declarationList;
    CompoundStatement *compoundStatement;

public:
    FunctionDefinition(DeclarationSpecifiers *declarationSpecifers,
                       Declarator *declarator,
                       DeclarationList *declarationList,
                       CompoundStatement *compoundStatement)
    : declarationSpecifers(declarationSpecifers), declarator(declarator),
      declarationList(declarationList), compoundStatement(compoundStatement) {}
};

extern vector<ExternalDeclaration *> declarations;

#endif
