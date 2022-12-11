TARGET = out

YACC_OBJ = gpp_interpreter.y

YACC_C_OBJ = y.tab.c

LEX_OBJ = gpp_lexer.l

LEX_C_OBJ = lex.yy.c

YACC_CC = yacc

FLEX_CC = flex

GCC = gcc

all:
	$(YACC_CC) -d $(YACC_OBJ)
	$(FLEX_CC) $(LEX_OBJ)
	$(CC) $(LEX_C_OBJ) $(YACC_C_OBJ)
clean:
	rm gpp_lexer.out
