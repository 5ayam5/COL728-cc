---
geometry:
- top=25mm
- left=20mm
- right=20mm
- bottom=30mm
documentclass: extarticle
fontsize: 12pt
numbersections: true
title: Lab 1 (part 2) - 2019CS10399
--- 

# `c.y` File
This is a bison file that is used to define the CFG used to define the grammar for the tokens that were lexed by the lexer.

# Generating `c.tab.cpp` from `c.y`
`bison` command is used to generate `c.tab.cpp` file from `c.y` file. This also generates the `c.tab.hpp` file which is also required by `c.lex.cpp`.

# Understanding `c.tab.cpp`
`c.tab.cpp` file is an auto-generated file based on the contents in the `c.y` file. The main parser function that does the generation of the parse tree is `yyparse()`.

## Working of `yyparse`
1. On call to the function, multiple variables are initialised, some of which are `yyss`, `yyssa` (these two are used to store the base and the top of the stack), `yyvs`, `yyvsp` (these two are used to store the base and top of the semantic value stack), `yyresult` (return value of `yyparse`), `yytoken` (lookahead symbol)
1. The following `goto` labels are defined in `yyparse`:
    a. `yynewstate`: updates stack pointer after value is pushed in the stack
    a. `yysetstate`: updates the top of the stack to the current state (`yystate`)
    a. `yybackup`: performs processiong based on the current state and lookahead token
    a. `yydefault`: default action for current state
    a. `yyreduce`: perform reduction
    a. `yyerrlab`: label to go to on detecting error
    a. `yyerrorlab`: error raised by `YYERROR`
    a. `yyerrlab1`: common code for both syntax error and `YYERROR`
    a. `yyacceptlab`: on arriving at `YYACCEPT`
    a. `yyabortlab`: `on arriving at `YYABORT`
    a. `yyexhaustedlab`: if memory is exceeded `YYNOMEM`
    a. `yyreturnlab`: parsing is completed, clean up and return

## Explanation of each `goto` State in Detail

### `yynewstate`
This simply increments the top of the stack since a state has been pushed to the stack and then fall through to `yysetstate`

### `yysetstate`
This sets the top of the stack to `yystate` and checks for overflow (stack being filled). After this, it checks if the state is the final state, then it calls `YYACCEPT`, otherwise jumps to `yybackup`.

### `yybackup`
It first attempts to find the next action without using the lookahead token, it consults `yypact` table for this. If the proposed action is the default value, we jump to `yydefault`. Else it reads the lookahead token to find the action to perform. Here, it calls `yylex` if all the previously lexed characters have been consumed. It handles EOF and errors in lexing. It then finds the action to take based on the lookahead character read. If if finds a valid action, it reduces, else it shifts and jumps to `yynewstate`.

### `yydefault`
Checks for the default rule to reduce the current state using a `yydefact` table (this value is stored in `yyn` variable). If the value is $0$, then there is an error and we jump to `yyerrlab`, else we jump to `yyreduce`.

### `yyreduce`
This reduces the top elements of the stack using the rule identified in `yyn` variable. It then makes a transition in the automaton based on the new top of the stack using tables `yytable` and `yydefgoto`. It then jumps to `yynewstate`.

### `yyerrlab`
This checks the `yyerrstatus` variable to check error recovery status, so that the parsing is not completely stopped and the user can get more information about the parsing of their code. It then jumps to `yyerrlab1`.

### `yyerrorlab`
If `YYERROR` was explicitly raised, simply pop the stack and jump to `yyerrlab1`

### `yyerrlab1`
This pops the stack until a state is found that can shift on the error token. If we reach the bottom of the stack, we call `YYABORT`. Once we reach a state that can shift the error token, we jump to `yynewstate` to resume parsing from here.

### `yyacceptlab`
On calling `YYACCEPT`, we jump to here, set `yyresult` to $0$ and jump to `yyreturnlab`.

### `yyabortlab`
On calling `YYABORT`, we jump to here, set `yyresult` to $1$ and jump to `yyreturnlab`.

### `yyexhaustedlab`
On calling `YYNOMEM`, we jump to here, set `yyresult` to $2$ and jump to `yyreturnlab`.

### `yyreturnlab`
This cleans and frees the stack and then returns `yyresult`, exiting from `yyparse`.
