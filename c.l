%e  1019
%p  2807
%n  371
%k  284
%a  1213
%o  1117

O   [0-7]
D   [0-9]
NZ  [1-9]
L   [a-zA-Z_]
A   [a-zA-Z_0-9]
H   [a-fA-F0-9]
HP  (0[xX])
E   ([Ee][+-]?{D}+)
P   ([Pp][+-]?{D}+)
FS  (f|F|l|L)
IS  (((u|U)(l|L|ll|LL)?)|((l|L|ll|LL)(u|U)?))
CP  (u|U|L)
SP  (u8|u|U|L)
ES  (\\(['"\?\\abfnrtv]|[0-7]{1,3}|x[a-fA-F0-9]+))
WS  [ \t\v\n\f]

%{
#include <stdio.h>
#include "c.tab.hpp"

extern void yyerror(const char *);  /* prints grammar violation message */

extern int sym_type(const char *);  /* returns type from symbol table */

#define sym_type(identifier) IDENTIFIER /* with no symbol table, fake it */

static void comment(void);
static int check_type(void);
#define YY_DECL extern "C" int yylex()
%}

%%
"/*"                                    { comment(); }
"//".*                                    { /* consume //-comment */ }

"auto"					{ yylval.text = strdup(yytext); return(AUTO); }
"break"					{ yylval.text = strdup(yytext); return(BREAK); }
"case"					{ yylval.text = strdup(yytext); return(CASE); }
"char"					{ yylval.text = strdup(yytext); return(CHAR); }
"const"					{ yylval.text = strdup(yytext); return(CONST); }
"continue"				{ yylval.text = strdup(yytext); return(CONTINUE); }
"default"				{ yylval.text = strdup(yytext); return(DEFAULT); }
"do"					{ yylval.text = strdup(yytext); return(DO); }
"double"				{ yylval.text = strdup(yytext); return(DOUBLE); }
"else"					{ yylval.text = strdup(yytext); return(ELSE); }
"enum"					{ yylval.text = strdup(yytext); return(ENUM); }
"extern"				{ yylval.text = strdup(yytext); return(EXTERN); }
"float"					{ yylval.text = strdup(yytext); return(FLOAT); }
"for"					{ yylval.text = strdup(yytext); return(FOR); }
"goto"					{ yylval.text = strdup(yytext); return(GOTO); }
"if"					{ yylval.text = strdup(yytext); return(IF); }
"inline"				{ yylval.text = strdup(yytext); return(INLINE); }
"int"					{ yylval.text = strdup(yytext); return(INT); }
"long"					{ yylval.text = strdup(yytext); return(LONG); }
"register"				{ yylval.text = strdup(yytext); return(REGISTER); }
"restrict"				{ yylval.text = strdup(yytext); return(RESTRICT); }
"return"				{ yylval.text = strdup(yytext); return(RETURN); }
"short"					{ yylval.text = strdup(yytext); return(SHORT); }
"signed"				{ yylval.text = strdup(yytext); return(SIGNED); }
"sizeof"				{ yylval.text = strdup(yytext); return(SIZEOF); }
"static"				{ yylval.text = strdup(yytext); return(STATIC); }
"struct"				{ yylval.text = strdup(yytext); return(STRUCT); }
"switch"				{ yylval.text = strdup(yytext); return(SWITCH); }
"typedef"				{ yylval.text = strdup(yytext); return(TYPEDEF); }
"union"					{ yylval.text = strdup(yytext); return(UNION); }
"unsigned"				{ yylval.text = strdup(yytext); return(UNSIGNED); }
"void"					{ yylval.text = strdup(yytext); return(VOID); }
"volatile"				{ yylval.text = strdup(yytext); return(VOLATILE); }
"while"					{ yylval.text = strdup(yytext); return(WHILE); }
"_Alignas"                              { yylval.text = strdup(yytext); return ALIGNAS; }
"_Alignof"                              { yylval.text = strdup(yytext); return ALIGNOF; }
"_Atomic"                               { yylval.text = strdup(yytext); return ATOMIC; }
"_Bool"                                 { yylval.text = strdup(yytext); return BOOL; }
"_Complex"                              { yylval.text = strdup(yytext); return COMPLEX; }
"_Generic"                              { yylval.text = strdup(yytext); return GENERIC; }
"_Imaginary"                            { yylval.text = strdup(yytext); return IMAGINARY; }
"_Noreturn"                             { yylval.text = strdup(yytext); return NORETURN; }
"_Static_assert"                        { yylval.text = strdup(yytext); return STATIC_ASSERT; }
"_Thread_local"                         { yylval.text = strdup(yytext); return THREAD_LOCAL; }
"__func__"                              { yylval.text = strdup(yytext); return FUNC_NAME; }

{L}{A}*					{ yylval.text = strdup(yytext); return check_type(); }

{HP}{H}+{IS}?				{ yylval.text = strdup(yytext); return I_CONSTANT; }
{NZ}{D}*{IS}?				{ yylval.text = strdup(yytext); return I_CONSTANT; }
"0"{O}*{IS}?				{ yylval.text = strdup(yytext); return I_CONSTANT; }
{CP}?"'"([^'\\\n]|{ES})+"'"		{ yylval.text = strdup(yytext); return I_CONSTANT; }

{D}+{E}{FS}?				{ yylval.text = strdup(yytext); return F_CONSTANT; }
{D}*"."{D}+{E}?{FS}?			{ yylval.text = strdup(yytext); return F_CONSTANT; }
{D}+"."{E}?{FS}?			{ yylval.text = strdup(yytext); return F_CONSTANT; }
{HP}{H}+{P}{FS}?			{ yylval.text = strdup(yytext); return F_CONSTANT; }
{HP}{H}*"."{H}+{P}{FS}?			{ yylval.text = strdup(yytext); return F_CONSTANT; }
{HP}{H}+"."{P}{FS}?			{ yylval.text = strdup(yytext); return F_CONSTANT; }

({SP}?\"([^"\\\n]|{ES})*\"{WS}*)+	{ yylval.text = strdup(yytext); return STRING_LITERAL; }

"..."					{ yylval.text = strdup(yytext); return ELLIPSIS; }
">>="					{ yylval.text = strdup(yytext); return RIGHT_ASSIGN; }
"<<="					{ yylval.text = strdup(yytext); return LEFT_ASSIGN; }
"+="					{ yylval.text = strdup(yytext); return ADD_ASSIGN; }
"-="					{ yylval.text = strdup(yytext); return SUB_ASSIGN; }
"*="					{ yylval.text = strdup(yytext); return MUL_ASSIGN; }
"/="					{ yylval.text = strdup(yytext); return DIV_ASSIGN; }
"%="					{ yylval.text = strdup(yytext); return MOD_ASSIGN; }
"&="					{ yylval.text = strdup(yytext); return AND_ASSIGN; }
"^="					{ yylval.text = strdup(yytext); return XOR_ASSIGN; }
"|="					{ yylval.text = strdup(yytext); return OR_ASSIGN; }
">>"					{ yylval.text = strdup(yytext); return RIGHT_OP; }
"<<"					{ yylval.text = strdup(yytext); return LEFT_OP; }
"++"					{ yylval.text = strdup(yytext); return INC_OP; }
"--"					{ yylval.text = strdup(yytext); return DEC_OP; }
"->"					{ yylval.text = strdup(yytext); return PTR_OP; }
"&&"					{ yylval.text = strdup(yytext); return AND_OP; }
"||"					{ yylval.text = strdup(yytext); return OR_OP; }
"<="					{ yylval.text = strdup(yytext); return LE_OP; }
">="					{ yylval.text = strdup(yytext); return GE_OP; }
"=="					{ yylval.text = strdup(yytext); return EQ_OP; }
"!="					{ yylval.text = strdup(yytext); return NE_OP; }
";"					{ yylval.text = strdup(yytext); return ';'; }
("{"|"<%")				{ yylval.text = strdup(yytext); return '{'; }
("}"|"%>")				{ yylval.text = strdup(yytext); return '}'; }
","					{ yylval.text = strdup(yytext); return ','; }
":"					{ yylval.text = strdup(yytext); return ':'; }
"="					{ yylval.text = strdup(yytext); return '='; }
"("					{ yylval.text = strdup(yytext); return '('; }
")"					{ yylval.text = strdup(yytext); return ')'; }
("["|"<:")				{ yylval.text = strdup(yytext); return '['; }
("]"|":>")				{ yylval.text = strdup(yytext); return ']'; }
"."					{ yylval.text = strdup(yytext); return '.'; }
"&"					{ yylval.text = strdup(yytext); return '&'; }
"!"					{ yylval.text = strdup(yytext); return '!'; }
"~"					{ yylval.text = strdup(yytext); return '~'; }
"-"					{ yylval.text = strdup(yytext); return '-'; }
"+"					{ yylval.text = strdup(yytext); return '+'; }
"*"					{ yylval.text = strdup(yytext); return '*'; }
"/"					{ yylval.text = strdup(yytext); return '/'; }
"%"					{ yylval.text = strdup(yytext); return '%'; }
"<"					{ yylval.text = strdup(yytext); return '<'; }
">"					{ yylval.text = strdup(yytext); return '>'; }
"^"					{ yylval.text = strdup(yytext); return '^'; }
"|"					{ yylval.text = strdup(yytext); return '|'; }
"?"					{ yylval.text = strdup(yytext); return '?'; }

{WS}+					{ /* whitespace separates tokens */ }
.					{ /* discard bad characters */ }

%%

int yywrap(void)        /* called at end of input */
{
    return 1;           /* terminate now */
}

static void comment(void)
{
    int c;

    while ((c = yyinput()) != 0)
        if (c == '*')
        {
            while ((c = yyinput()) == '*')
                ;

            if (c == '/')
                return;

            if (c == 0)
                break;
        }
    yyerror("unterminated comment");
}

static int check_type(void)
{
    switch (sym_type(yytext))
    {
    case TYPEDEF_NAME:                /* previously defined */
        return TYPEDEF_NAME;
    case ENUMERATION_CONSTANT:        /* previously defined */
        return ENUMERATION_CONSTANT;
    default:                          /* includes undefined */
        return IDENTIFIER;
    }
}
