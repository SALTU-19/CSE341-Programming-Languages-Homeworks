1. "gpp_lexer.l" and "lex.yy.c" is part 1 of homework using flex.
2. "gpp_lexer.lisp" is part 2 of homework using lisp.
3. You can execute part 1 by take input from user. for example give input as "(deffun sum (x y) " and it will give output as "( : OP_OP deffun: KW_DEFFUN sum: IDENTIFIER ( : OP_OP x: IDENTIFIER y: IDENTIFIER ) : OP_CP ".
4. You can execute part 2 by read input from file. Give the filename of .gpp file as input and it will give list of lexing values as " (("(" "OP_OP") ("DEFFUN" "KW_DEFFUN") ("sum" "IDENTIFIER") ("(" "OP_OP") ("x" "IDENTIFIER") ("y" "IDENTIFIER") "
 (")" "OP_CP"))".    