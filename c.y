%code requires{
#include "ast.hpp"
}

%{
#include <cstdio>
#include <iostream>

// stuff from flex that bison needs to know about:
extern "C" int yylex();
int yyparse();
extern "C" FILE *yyin;
 
void yyerror(const char *s);
%}

%token  IDENTIFIER I_CONSTANT F_CONSTANT STRING_LITERAL FUNC_NAME SIZEOF
%token  PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token  AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token  SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token  XOR_ASSIGN OR_ASSIGN
%token  TYPEDEF_NAME ENUMERATION_CONSTANT

%token  TYPEDEF EXTERN STATIC AUTO REGISTER INLINE
%token  CONST RESTRICT VOLATILE
%token  BOOL CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE VOID
%token  COMPLEX IMAGINARY 
%token  STRUCT UNION ENUM ELLIPSIS

%token  CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%token  ALIGNAS ALIGNOF ATOMIC GENERIC NORETURN STATIC_ASSERT THREAD_LOCAL

%start translation_unit

%union {
    const char *text;
    Node *node;
}

%type <node> translation_unit external_declaration function_definition declaration declaration_specifiers init_declarator_list init_declarator declarator direct_declarator pointer type_name parameter_type_list parameter_list parameter_declaration identifier_list type_specifier specifier_qualifier_list struct_or_union_specifier struct_or_union struct_declaration_list struct_declaration struct_declarator_list struct_declarator enum_specifier enumerator_list initializer initializer_list designator designator_list constant_expression assignment_expression conditional_expression logical_or_expression logical_and_expression inclusive_or_expression exclusive_or_expression and_expression equality_expression relational_expression shift_expression additive_expression multiplicative_expression cast_expression unary_expression postfix_expression primary_expression argument_expression_list statement labeled_statement compound_statement expression_statement selection_statement iteration_statement jump_statement constant declaration_list string enumeration_constant generic_selection generic_assoc_list generic_association unary_operator assignment_operator expression storage_class_specifier type_qualifier type_qualifier_list enumerator abstract_declarator function_specifier alignment_specifier block_item_list block_item static_assert_declaration atomic_type_specifier direct_abstract_declarator
%%

primary_expression
    : IDENTIFIER { $$ = new PrimaryExpression(new StringType("IDENTIFIER", $<text>1), 1); }
    | constant { $$ = new PrimaryExpression(static_cast<StringType *>($1), 2); }
    | string { $$ = new PrimaryExpression(static_cast<StringType *>($1), 3); }
    | '(' expression ')' { $$ = new PrimaryExpression(static_cast<ExpressionStatement *>($2)); }
    | generic_selection { $$ = new PrimaryExpression(static_cast<GenericSelection *>($1)); }
    ;

constant
    : I_CONSTANT        /* includes character_constant */ { $$ = new StringType("I_CONSTANT", $<text>1); }
    | F_CONSTANT { $$ = new StringType("F_CONSTANT", $<text>1); }
    | ENUMERATION_CONSTANT  /* after it has been defined as such */ { $$ = new StringType("ENUMERATION_CONSTANT", $<text>1); }
    ;

enumeration_constant
    : IDENTIFIER { $$ = new StringType("IDENTIFIER", $<text>1); }
    ;

string
    : STRING_LITERAL { $$ = new StringType("STRING_LITERAL", $<text>1); }
    | FUNC_NAME { $$ = new StringType("FUNC_NAME", $<text>1); }
    ;

generic_selection
    : GENERIC '(' assignment_expression ',' generic_assoc_list ')' { $$ = new GenericSelection(static_cast<AssignmentExpression *>($3), static_cast<GenericAssocList *>($5)); }
    ;

generic_assoc_list
    : generic_association { $$ = new GenericAssocList(); ((GenericAssocList *)$$)->add(static_cast<GenericAssociation *>($1)); }
    | generic_assoc_list ',' generic_association { ((GenericAssocList *)$1)->add(static_cast<GenericAssociation *>($3)); $$ = $1; }
    ;

generic_association
    : type_name ':' assignment_expression { $$ = new GenericAssociation(static_cast<TypeName *>($1), static_cast<AssignmentExpression *>($3)); }
    | DEFAULT ':' assignment_expression { $$ = new GenericAssociation(nullptr, static_cast<AssignmentExpression *>($3)); }
    ;

postfix_expression
    : primary_expression { $$ = new PostfixExpression(static_cast<PrimaryExpression *>($1)); }
    | postfix_expression '[' expression ']'
    | postfix_expression '(' ')'
    | postfix_expression '(' argument_expression_list ')' { $$ = new PostfixExpression(static_cast<PostfixExpression *>($1), static_cast<ArgumentExpressionList *>($3)); }
    | postfix_expression '.' IDENTIFIER
    | postfix_expression PTR_OP IDENTIFIER
    | postfix_expression INC_OP
    | postfix_expression DEC_OP
    | '(' type_name ')' '{' initializer_list '}'
    | '(' type_name ')' '{' initializer_list ',' '}'
    ;

argument_expression_list
    : assignment_expression { $$ = new ArgumentExpressionList(); ((ArgumentExpressionList *)$$)->add(static_cast<AssignmentExpression *>($1)); }
    | argument_expression_list ',' assignment_expression { ((ArgumentExpressionList *)$1)->add(static_cast<AssignmentExpression *>($3)); $$ = $1; }
    ;

unary_expression
    : postfix_expression { $$ = new UnaryExpression(static_cast<PostfixExpression *>($1)); }
    | INC_OP unary_expression { $$ = new UnaryExpression(new StringType("++", "++"), static_cast<UnaryExpression *>($2)); }
    | DEC_OP unary_expression { $$ = new UnaryExpression(new StringType("--", "--"), static_cast<UnaryExpression *>($2)); }
    | unary_operator cast_expression { $$ = new UnaryExpression(static_cast<StringType *>($1), static_cast<CastExpression *>($2)); }
    | SIZEOF unary_expression { $$ = new UnaryExpression(new StringType("SIZEOF", "SIZEOF"), static_cast<UnaryExpression *>($2)); }
    | SIZEOF '(' type_name ')' { $$ = new UnaryExpression(new StringType("SIZEOF", "SIZEOF"), static_cast<TypeName *>($3)); }
    | ALIGNOF '(' type_name ')' { $$ = new UnaryExpression(new StringType("ALIGNOF", "ALIGNOF"), static_cast<TypeName *>($3)); }
    ;

unary_operator
    : '&' { $$ = new StringType("&", "&"); }
    | '*' { $$ = new StringType("*", "*"); }
    | '+' { $$ = new StringType("+", "+"); }
    | '-' { $$ = new StringType("-", "-"); }
    | '~' { $$ = new StringType("~", "~"); }
    | '!' { $$ = new StringType("!", "!"); }
    ;

cast_expression
    : unary_expression { $$ = new CastExpression(static_cast<UnaryExpression *>($1)); }
    | '(' type_name ')' cast_expression { $$ = new CastExpression(static_cast<TypeName *>($2), static_cast<CastExpression *>($4)); }
    ;

multiplicative_expression
    : cast_expression { $$ = new BinaryExpression(static_cast<CastExpression *>($1)); }
    | multiplicative_expression '*' cast_expression { $$ = new BinaryExpression(static_cast<BinaryExpression *>($1), new StringType("*", "*"), static_cast<BinaryExpression *>($3)); }
    | multiplicative_expression '/' cast_expression { $$ = new BinaryExpression(static_cast<BinaryExpression *>($1), new StringType("/", "/"), static_cast<BinaryExpression *>($3)); }
    | multiplicative_expression '%' cast_expression { $$ = new BinaryExpression(static_cast<BinaryExpression *>($1), new StringType("%", "%"), static_cast<BinaryExpression *>($3)); }
    ;

additive_expression
    : multiplicative_expression { $$ = $1; }
    | additive_expression '+' multiplicative_expression { $$ = new BinaryExpression(static_cast<BinaryExpression *>($1), new StringType("+", "+"), static_cast<BinaryExpression *>($3)); }
    | additive_expression '-' multiplicative_expression { $$ = new BinaryExpression(static_cast<BinaryExpression *>($1), new StringType("-", "+"), static_cast<BinaryExpression *>($3)); }
    ;

shift_expression
    : additive_expression { $$ = $1; }
    | shift_expression LEFT_OP additive_expression { $$ = new BinaryExpression(static_cast<BinaryExpression *>($1), new StringType("<<", "<<"), static_cast<BinaryExpression *>($3)); }
    | shift_expression RIGHT_OP additive_expression { $$ = new BinaryExpression(static_cast<BinaryExpression *>($1), new StringType(">>", ">>"), static_cast<BinaryExpression *>($3)); }
    ;

relational_expression
    : shift_expression { $$ = $1; }
    | relational_expression '<' shift_expression { $$ = new BinaryExpression(static_cast<BinaryExpression *>($1), new StringType("<", "<"), static_cast<BinaryExpression *>($3)); }
    | relational_expression '>' shift_expression { $$ = new BinaryExpression(static_cast<BinaryExpression *>($1), new StringType(">", ">"), static_cast<BinaryExpression *>($3)); }
    | relational_expression LE_OP shift_expression { $$ = new BinaryExpression(static_cast<BinaryExpression *>($1), new StringType("<=", "<="), static_cast<BinaryExpression *>($3)); }
    | relational_expression GE_OP shift_expression { $$ = new BinaryExpression(static_cast<BinaryExpression *>($1), new StringType(">=", ">="), static_cast<BinaryExpression *>($3)); }
    ;

equality_expression
    : relational_expression { $$ = $1; }
    | equality_expression EQ_OP relational_expression { $$ = new BinaryExpression(static_cast<BinaryExpression *>($1), new StringType("==", "=="), static_cast<BinaryExpression *>($3)); }
    | equality_expression NE_OP relational_expression { $$ = new BinaryExpression(static_cast<BinaryExpression *>($1), new StringType("!=", "!="), static_cast<BinaryExpression *>($3)); }
    ;

and_expression
    : equality_expression { $$ = $1; }
    | and_expression '&' equality_expression { $$ = new BinaryExpression(static_cast<BinaryExpression *>($1), new StringType("&", "&"), static_cast<BinaryExpression *>($3)); }
    ;

exclusive_or_expression
    : and_expression { $$ = $1; }
    | exclusive_or_expression '^' and_expression { $$ = new BinaryExpression(static_cast<BinaryExpression *>($1), new StringType("^", "^"), static_cast<BinaryExpression *>($3)); }
    ;

inclusive_or_expression
    : exclusive_or_expression { $$ = $1; }
    | inclusive_or_expression '|' exclusive_or_expression { $$ = new BinaryExpression(static_cast<BinaryExpression *>($1), new StringType("|", "|"), static_cast<BinaryExpression *>($3)); }
    ;

logical_and_expression
    : inclusive_or_expression { $$ = $1; }
    | logical_and_expression AND_OP inclusive_or_expression { $$ = new BinaryExpression(static_cast<BinaryExpression *>($1), new StringType("&&", "&&"), static_cast<BinaryExpression *>($3)); }
    ;

logical_or_expression
    : logical_and_expression { $$ = $1; }
    | logical_or_expression OR_OP logical_and_expression { $$ = new BinaryExpression(static_cast<BinaryExpression *>($1), new StringType("||", "||"), static_cast<BinaryExpression *>($3)); }
    ;

conditional_expression
    : logical_or_expression { $$ = new ConditionalExpression(static_cast<BinaryExpression *>($1), nullptr, nullptr); }
    | logical_or_expression '?' expression ':' conditional_expression { $$ = new ConditionalExpression(static_cast<BinaryExpression *>($1), static_cast<ExpressionStatement *>($3), static_cast<ConditionalExpression *>($5)); }
    ;

assignment_expression
    : conditional_expression { $$ = new AssignmentExpression(static_cast<ConditionalExpression *>($1)); }
    | unary_expression assignment_operator assignment_expression { $$ = new AssignmentExpression(static_cast<UnaryExpression *>($1), static_cast<StringType *>($2), static_cast<AssignmentExpression *>($3)); }
    ;

assignment_operator
    : '=' { $$ = new StringType("ASSIGN", "ASSIGN"); }
    | MUL_ASSIGN { $$ = new StringType("MUL_ASSIGN", "MUL_ASSIGN"); }
    | DIV_ASSIGN { $$ = new StringType("DIV_ASSIGN", "DIV_ASSIGN"); }
    | MOD_ASSIGN { $$ = new StringType("MOD_ASSIGN", "MOD_ASSIGN"); }
    | ADD_ASSIGN { $$ = new StringType("ADD_ASSIGN", "ADD_ASSIGN"); }
    | SUB_ASSIGN { $$ = new StringType("SUB_ASSIGN", "SUB_ASSIGN"); }
    | LEFT_ASSIGN { $$ = new StringType("LEFT_ASSIGN", "LEFT_ASSIGN"); }
    | RIGHT_ASSIGN { $$ = new StringType("RIGHT_ASSIGN", "RIGHT_ASSIGN"); }
    | AND_ASSIGN { $$ = new StringType("AND_ASSIGN", "AND_ASSIGN"); }
    | XOR_ASSIGN { $$ = new StringType("XOR_ASSIGN", "XOR_ASSIGN"); }
    | OR_ASSIGN { $$ = new StringType("OR_ASSIGN", "OR_ASSIGN"); }
    ;

expression
    : assignment_expression { $$ = new ExpressionStatement(); ((ExpressionStatement *)$$)->add(static_cast<AssignmentExpression *>($1)); }
    | expression ',' assignment_expression { ((ExpressionStatement *)$1)->add(static_cast<AssignmentExpression *>($3)); $$ = $1; }
    ;

constant_expression
    : conditional_expression    /* with constraints */
    ;

declaration
    : declaration_specifiers ';' { $$ = new Declaration(static_cast<DeclarationSpecifiers *>($1), nullptr); }
    | declaration_specifiers init_declarator_list ';' { $$ = new Declaration(static_cast<DeclarationSpecifiers *>($1), static_cast<InitDeclaratorList *>($2)); }
    | static_assert_declaration { $$ = new Declaration(static_cast<StaticAssertDeclaration *>($1)); }
    ;

declaration_specifiers
    : storage_class_specifier declaration_specifiers { ((DeclarationSpecifiers *)$2)->add(static_cast<StringType *>($1)); $$ = $2; }
    | storage_class_specifier { $$ = new DeclarationSpecifiers(); ((DeclarationSpecifiers *)$$)->add(static_cast<StringType *>($1)); }
    | type_specifier declaration_specifiers { ((DeclarationSpecifiers *)$2)->add(static_cast<StringType *>($1)); $$ = $2; }
    | type_specifier { $$ = new DeclarationSpecifiers(); ((DeclarationSpecifiers *)$$)->add(static_cast<StringType *>($1)); }
    | type_qualifier declaration_specifiers { ((DeclarationSpecifiers *)$2)->add(static_cast<StringType *>($1)); $$ = $2; }
    | type_qualifier { $$ = new DeclarationSpecifiers(); ((DeclarationSpecifiers *)$$)->add(static_cast<StringType *>($1)); }
    | function_specifier declaration_specifiers { ((DeclarationSpecifiers *)$2)->add(static_cast<StringType *>($1)); $$ = $2; }
    | function_specifier { $$ = new DeclarationSpecifiers(); ((DeclarationSpecifiers *)$$)->add(static_cast<StringType *>($1)); }
    | alignment_specifier declaration_specifiers { ((DeclarationSpecifiers *)$2)->add(static_cast<StringType *>($1)); $$ = $2; }
    | alignment_specifier { $$ = new DeclarationSpecifiers(); ((DeclarationSpecifiers *)$$)->add(static_cast<StringType *>($1)); }
    ;

init_declarator_list
    : init_declarator { $$ = new InitDeclaratorList(); ((InitDeclaratorList *)$$)->add(static_cast<InitDeclarator *>($1)); }
    | init_declarator_list ',' init_declarator { ((InitDeclaratorList *)$1)->add(static_cast<InitDeclarator *>($3)); $$ = $1; }
    ;

init_declarator
    : declarator '=' initializer { $$ = new InitDeclarator(static_cast<Declarator *>($1), static_cast<Initializer *>($3)); }
    | declarator { $$ = new InitDeclarator(static_cast<Declarator *>($1), nullptr); }
    ;

storage_class_specifier
    : TYPEDEF   /* identifiers must be flagged as TYPEDEF_NAME */ { $$ = new StringType("TYPEDEF", "TYPEDEF"); }
    | EXTERN { $$ = new StringType("EXTERN", "EXTERN"); }
    | STATIC { $$ = new StringType("STATIC", "STATIC"); }
    | THREAD_LOCAL { $$ = new StringType("THREAD_LOCAL", "THREAD_LOCAL"); }
    | AUTO { $$ = new StringType("AUTO", "AUTO"); }
    | REGISTER { $$ = new StringType("REGISTER", "REGISTER"); }
    ;

type_specifier
    : VOID { $$ = new StringType("VOID", "VOID"); }
    | CHAR { $$ = new StringType("CHAR", "CHAR"); }
    | SHORT { $$ = new StringType("SHORT", "SHORT"); }
    | INT { $$ = new StringType("INT", "INT"); }
    | LONG { $$ = new StringType("LONG", "LONG"); }
    | FLOAT { $$ = new StringType("FLOAT", "FLOAT"); }
    | DOUBLE { $$ = new StringType("DOUBLE", "DOUBLE"); }
    | SIGNED { $$ = new StringType("SIGNED", "SIGNED"); }
    | UNSIGNED { $$ = new StringType("UNSIGNED", "UNSIGNED"); }
    | BOOL { $$ = new StringType("BOOL", "BOOL"); }
    | COMPLEX { $$ = new StringType("COMPLEX", "COMPLEX"); }
    | IMAGINARY     /* non-mandated extension */ { $$ = new StringType("IMAGINARY", "IMAGINARY"); }
    | atomic_type_specifier { $$ = $1; }
    | struct_or_union_specifier { $$ = $1; }
    | enum_specifier { $$ = $1; }
    | TYPEDEF_NAME      /* after it has been defined as such */ { $$ = new StringType("TYPEDEF_NAME", $<text>1); }
    ;

struct_or_union_specifier
    : struct_or_union '{' struct_declaration_list '}' // TODO
    | struct_or_union IDENTIFIER '{' struct_declaration_list '}'
    | struct_or_union IDENTIFIER
    ;

struct_or_union
    : STRUCT
    | UNION
    ;

struct_declaration_list
    : struct_declaration
    | struct_declaration_list struct_declaration
    ;

struct_declaration
    : specifier_qualifier_list ';'  /* for anonymous struct/union */
    | specifier_qualifier_list struct_declarator_list ';'
    | static_assert_declaration
    ;

specifier_qualifier_list
    : type_specifier specifier_qualifier_list { ((SpecifierQualifierList *)$2)->add(static_cast<StringType *>($1)); $$ = $2; }
    | type_specifier { $$ = new SpecifierQualifierList(); ((SpecifierQualifierList *)$$)->add(static_cast<StringType *>($1)); }
    | type_qualifier specifier_qualifier_list { ((SpecifierQualifierList *)$2)->add(static_cast<StringType *>($1)); $$ = $2; }
    | type_qualifier { $$ = new SpecifierQualifierList(); ((SpecifierQualifierList *)$$)->add(static_cast<StringType *>($1)); }
    ;

struct_declarator_list
    : struct_declarator
    | struct_declarator_list ',' struct_declarator
    ;

struct_declarator
    : ':' constant_expression
    | declarator ':' constant_expression
    | declarator
    ;

enum_specifier
    : ENUM '{' enumerator_list '}' // TODO
    | ENUM '{' enumerator_list ',' '}'
    | ENUM IDENTIFIER '{' enumerator_list '}'
    | ENUM IDENTIFIER '{' enumerator_list ',' '}'
    | ENUM IDENTIFIER
    ;

enumerator_list
    : enumerator
    | enumerator_list ',' enumerator
    ;

enumerator
    : enumeration_constant '=' constant_expression
    | enumeration_constant
    ;

atomic_type_specifier
    : ATOMIC '(' type_name ')' // TODO
    ;

type_qualifier
    : CONST { $$ = new StringType("CONST", "CONST"); }
    | RESTRICT { $$ = new StringType("CONST", "CONST"); }
    | VOLATILE { $$ = new StringType("CONST", "CONST"); }
    | ATOMIC { $$ = new StringType("CONST", "CONST"); }
    ;

function_specifier
    : INLINE { $$ = new StringType("INLINE", "INLINE"); }
    | NORETURN { $$ = new StringType("NORETURN", "NORETURN"); }
    ;

alignment_specifier
    : ALIGNAS '(' type_name ')' // TODO
    | ALIGNAS '(' constant_expression ')'
    ;

declarator
    : pointer direct_declarator { $$ = new Declarator(static_cast<Pointer *>($1), static_cast<DirectDeclarator *>($2)); }
    | direct_declarator { $$ = new Declarator(nullptr, static_cast<DirectDeclarator *>($1)); }
    ;

direct_declarator
    : IDENTIFIER { $$ = new DirectDeclarator(new StringType("IDENTIFIER", $<text>1)); }
    | '(' declarator ')' { $$ = new DirectDeclarator(static_cast<Declarator *>($2)); }
    | direct_declarator '[' ']'
    | direct_declarator '[' '*' ']'
    | direct_declarator '[' STATIC type_qualifier_list assignment_expression ']'
    | direct_declarator '[' STATIC assignment_expression ']'
    | direct_declarator '[' type_qualifier_list '*' ']'
    | direct_declarator '[' type_qualifier_list STATIC assignment_expression ']'
    | direct_declarator '[' type_qualifier_list assignment_expression ']'
    | direct_declarator '[' type_qualifier_list ']'
    | direct_declarator '[' assignment_expression ']'
    | direct_declarator '(' parameter_type_list ')' { $$ = new DirectDeclarator(static_cast<DirectDeclarator *>($1), new DirectDeclaratorUtil(static_cast<ParameterTypeList *>($3))); }
    | direct_declarator '(' ')' { $$ = new DirectDeclarator(static_cast<DirectDeclarator *>($1), nullptr); }
    | direct_declarator '(' identifier_list ')' { $$ = new DirectDeclarator(static_cast<DirectDeclarator *>($1), new DirectDeclaratorUtil(static_cast<IdentifierList *>($3))); }
    ;

pointer
    : '*' type_qualifier_list pointer { $$ = new Pointer(static_cast<TypeQualifierList *>($2), static_cast<Pointer *>($3)); }
    | '*' type_qualifier_list { $$ = new Pointer(static_cast<TypeQualifierList *>($2), nullptr); }
    | '*' pointer { $$ = new Pointer(nullptr, static_cast<Pointer *>($2)); }
    | '*' { $$ = new Pointer(nullptr, nullptr); }
    ;

type_qualifier_list
    : type_qualifier { $$ = new TypeQualifierList(); ((TypeQualifierList *)$$)->add(static_cast<StringType *>($1)); }
    | type_qualifier_list type_qualifier { ((TypeQualifierList *)$1)->add(static_cast<StringType *>($2)); $$ = $1; }
    ;


parameter_type_list
    : parameter_list ',' ELLIPSIS { DeclarationSpecifiers *ellipsis = new DeclarationSpecifiers(); ellipsis->add(new StringType("ELLIPSIS", "ELLIPSIS"));
                                    ((ParameterTypeList *)$1)->add(new ParameterDeclaration(ellipsis)); $$ = $1; }
    | parameter_list { $$ = $1; }
    ;

parameter_list
    : parameter_declaration { $$ = new ParameterTypeList(); ((ParameterTypeList *)$$)->add(static_cast<ParameterDeclaration *>($1)); }
    | parameter_list ',' parameter_declaration { ((ParameterTypeList *)$1)->add(static_cast<ParameterDeclaration *>($3)); $$ = $1; }
    ;

parameter_declaration
    : declaration_specifiers declarator { $$ = new ParameterDeclaration(static_cast<DeclarationSpecifiers *>($1), static_cast<Declarator *>($2)); }
    | declaration_specifiers abstract_declarator { $$ = new ParameterDeclaration(static_cast<DeclarationSpecifiers *>($1), static_cast<AbstractDeclarator *>($2)); }
    | declaration_specifiers { $$ = new ParameterDeclaration(static_cast<DeclarationSpecifiers *>($1)); }
    ;

identifier_list
    : IDENTIFIER { $$ = new IdentifierList(); ((IdentifierList *)$$)->add(new StringType("IDENTIFIER", $<text>1)); }
    | identifier_list ',' IDENTIFIER { ((IdentifierList *)$1)->add(new StringType("IDENTIFIER", $<text>3)); $$ = $1; }
    ;

type_name
    : specifier_qualifier_list abstract_declarator { $$ = new TypeName(static_cast<SpecifierQualifierList *>($1), static_cast<AbstractDeclarator *>($2)); }
    | specifier_qualifier_list { $$ = new TypeName(static_cast<SpecifierQualifierList *>($1), nullptr); }
    ;

abstract_declarator
    : pointer direct_abstract_declarator { $$ = new AbstractDeclarator(static_cast<Pointer *>($1), static_cast<DirectAbstractDeclarator *>($2)); }
    | pointer { $$ = new AbstractDeclarator(static_cast<Pointer *>($1), nullptr); }
    | direct_abstract_declarator { $$ = new AbstractDeclarator(nullptr, static_cast<DirectAbstractDeclarator *>($1)); }
    ;

direct_abstract_declarator
    : '(' abstract_declarator ')'
    | '[' ']'
    | '[' '*' ']'
    | '[' STATIC type_qualifier_list assignment_expression ']'
    | '[' STATIC assignment_expression ']'
    | '[' type_qualifier_list STATIC assignment_expression ']'
    | '[' type_qualifier_list assignment_expression ']'
    | '[' type_qualifier_list ']'
    | '[' assignment_expression ']'
    | direct_abstract_declarator '[' ']'
    | direct_abstract_declarator '[' '*' ']'
    | direct_abstract_declarator '[' STATIC type_qualifier_list assignment_expression ']'
    | direct_abstract_declarator '[' STATIC assignment_expression ']'
    | direct_abstract_declarator '[' type_qualifier_list assignment_expression ']'
    | direct_abstract_declarator '[' type_qualifier_list STATIC assignment_expression ']'
    | direct_abstract_declarator '[' type_qualifier_list ']'
    | direct_abstract_declarator '[' assignment_expression ']'
    | '(' ')'
    | '(' parameter_type_list ')'
    | direct_abstract_declarator '(' ')'
    | direct_abstract_declarator '(' parameter_type_list ')'
    ;

initializer
    : '{' initializer_list '}'
    | '{' initializer_list ',' '}'
    | assignment_expression
    ;

initializer_list
    : designation initializer
    | initializer
    | initializer_list ',' designation initializer
    | initializer_list ',' initializer
    ;

designation
    : designator_list '='
    ;

designator_list
    : designator
    | designator_list designator
    ;

designator
    : '[' constant_expression ']'
    | '.' IDENTIFIER
    ;

static_assert_declaration
    : STATIC_ASSERT '(' constant_expression ',' STRING_LITERAL ')' ';'
    ;

statement
    : labeled_statement { $$ = new Statement(static_cast<LabeledStatement *>($1)); }
    | compound_statement { $$ = new Statement(static_cast<CompoundStatement *>($1)); }
    | expression_statement { $$ = new Statement(static_cast<ExpressionStatement *>($1)); }
    | selection_statement { $$ = new Statement(static_cast<SelectionStatement *>($1)); }
    | iteration_statement { $$ = new Statement(static_cast<IterationStatement *>($1)); }
    | jump_statement { $$ = new Statement(static_cast<JumpStatement *>($1)); }
    ;

labeled_statement
    : IDENTIFIER ':' statement
    | CASE constant_expression ':' statement
    | DEFAULT ':' statement
    ;

compound_statement
    : '{' '}' { $$ = new CompoundStatement(); }
    | '{'  block_item_list '}' { $$ = $2; }
    ;

block_item_list
    : block_item { $$ = new CompoundStatement(); ((CompoundStatement *)$$)->add(static_cast<BlockItem *>($1)); }
    | block_item_list block_item { ((CompoundStatement *)$1)->add(static_cast<BlockItem *>($2)); $$ = $1; }
    ;

block_item
    : declaration { $$ = new BlockItem(static_cast<Declaration *>($1)); }
    | statement { $$ = new BlockItem(static_cast<Statement *>($1)); }
    ;

expression_statement
    : ';' { $$ = nullptr; }
    | expression ';' { $$ = $1; }
    ;

selection_statement
    : IF '(' expression ')' statement ELSE statement { $$ = new SelectionStatement(static_cast<ExpressionStatement *>($3), static_cast<Statement *>($5), static_cast<Statement *>($7)); }
    | IF '(' expression ')' statement { $$ = new SelectionStatement(static_cast<ExpressionStatement *>($3), static_cast<Statement *>($5), nullptr); }
    | SWITCH '(' expression ')' statement { $$ = new SelectionStatement(static_cast<ExpressionStatement *>($3), static_cast<Statement *>($5), nullptr); }
    ;

iteration_statement
    : WHILE '(' expression ')' statement { $$ = new IterationStatement(static_cast<ExpressionStatement *>($3), static_cast<Statement *>($5)); }
    | DO statement WHILE '(' expression ')' ';' { $$ = new IterationStatement(static_cast<Statement *>($2), static_cast<ExpressionStatement *>($5)); }
    | FOR '(' expression_statement expression_statement ')' statement { $$ = new IterationStatement(static_cast<ExpressionStatement *>($3), static_cast<ExpressionStatement *>($4), nullptr, static_cast<Statement *>($6)); }
    | FOR '(' expression_statement expression_statement expression ')' statement { $$ = new IterationStatement(static_cast<ExpressionStatement *>($3), static_cast<ExpressionStatement *>($4), static_cast<ExpressionStatement *>($5), static_cast<Statement *>($7)); }
    | FOR '(' declaration expression_statement ')' statement { $$ = new IterationStatement(static_cast<Declaration *>($3), static_cast<ExpressionStatement *>($4), nullptr, static_cast<Statement *>($6)); }
    | FOR '(' declaration expression_statement expression ')' statement { $$ = new IterationStatement(static_cast<Declaration *>($3), static_cast<ExpressionStatement *>($4), static_cast<ExpressionStatement *>($5), static_cast<Statement *>($7)); }
    ;

jump_statement
    : GOTO IDENTIFIER ';' { $$ = new JumpStatement(new StringType("IDENTIFIER", $<text>1)); }
    | CONTINUE ';' { $$ = new JumpStatement(new StringType("CONTINUE", $<text>1)); }
    | BREAK ';' { $$ = new JumpStatement(new StringType("BREAK", $<text>1)); }
    | RETURN ';' { $$ = new JumpStatement(new StringType("RETURN", $<text>1)); }
    | RETURN expression ';' { $$ = new JumpStatement(new StringType("RETURN", $<text>1), static_cast<ExpressionStatement *>($2)); }
    ;

translation_unit
    : external_declaration { external_declarations.push_back(static_cast<ExternalDeclaration *>($1)); }
    | translation_unit external_declaration { external_declarations.push_back(static_cast<ExternalDeclaration *>($2)); }
    ;

external_declaration
    : function_definition { $$ = $1; }
    | declaration { $$ = $1; }
    ;

function_definition
    : declaration_specifiers declarator declaration_list compound_statement { $$ = new FunctionDefinition(static_cast<DeclarationSpecifiers *>($1), static_cast<Declarator *>($2), static_cast<DeclarationList *>($3), static_cast<CompoundStatement *>($4)); }
    | declaration_specifiers declarator compound_statement { $$ = new FunctionDefinition(static_cast<DeclarationSpecifiers *>($1), static_cast<Declarator *>($2), nullptr, static_cast<CompoundStatement *>($3)); }
    ;

declaration_list
    : declaration { $$ = new DeclarationList(); ((DeclarationList *)$$)->add(static_cast<Declaration *>($1)); }
    | declaration_list declaration { ((DeclarationList *)$1)->add(static_cast<Declaration *>($2)); $$ = $1; }
    ;

%%
#include <stdio.h>

void yyerror(const char *s)
{
    fflush(stdout);
    fprintf(stderr, "*** %s\n", s);
}
