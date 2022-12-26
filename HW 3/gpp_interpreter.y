%{
	#include <string.h>
	#include <stdio.h>
	#include <stdlib.h>
	#include <math.h>
	void yyerror (char *array);
	int yylex(void);
	void printResult(float num);
	int if_condition(int statement, int value1,int value2);
	int while_condition(int statement);
    int power(int base, int exp);
    struct entry {
        char name[100];
        int value;
    };
    void put_entry(char *name, int value);
    int get_entry(char *name);
%}

%union{
	float num;
	char string[100];
}

%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_SET KW_DEFFUN KW_WHILE KW_IF KW_EXIT KW_DISP KW_TRUE KW_FALSE
%token OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP OP_DBLMULT OP_OC OP_CC OP_COMMA COMMENT 
%type<num> EXPB
%type<num> EXPI
%type<num> BinaryValue
%type<string> EXPS
%token<string> IDENTIFIER
%token<string> VALUESTR
%token<num> VALUE

%%
START :| INPUT START
	
;

INPUT : EXPS
    | EXPI {printResult($1); }
	| EXPB {printResult($1); }
    | OP_OP KW_EXIT OP_CP {exit(0);}
    | COMMENT {printf("Syntax OK.\n");}

;
EXPB : OP_OP KW_AND EXPB EXPB OP_CP { $$ = $3 && $4;}
	| OP_OP KW_OR EXPB EXPB OP_CP { $$ = $3 || $4;}
	| OP_OP KW_NOT EXPB OP_CP { $$ = !$3;}
	| OP_OP KW_LESS EXPB EXPB OP_CP { $$ = $3 < $4;}
	| OP_OP KW_EQUAL EXPB EXPB OP_CP { $$ = $3 == $4;}
	| OP_OP KW_EQUAL EXPI EXPI OP_CP { $$ = $3 == $4;}
	| OP_OP KW_DISP EXPB OP_CP { $$ = $3;}
	| EXPI 
	| BinaryValue {$$ = $1;}
;
EXPI : OP_OP OP_PLUS EXPI EXPI OP_CP { $$ = $3 + $4; }
	| OP_OP OP_MINUS EXPI EXPI OP_CP { $$ = $3 - $4;}
	| OP_OP OP_DIV EXPI EXPI OP_CP { $$ = $3 / $4; if($4 == 0) {printf("Zero Division Error!");}}
    | OP_OP OP_MULT EXPI EXPI OP_CP { $$ = $3 * $4;}
	| OP_OP OP_DBLMULT EXPI EXPI OP_CP { $$ = power($3,$4);}
	| OP_OP KW_IF EXPB EXPI EXPI OP_CP {$$ = if_condition($3,$4,$5);}
	| OP_OP KW_WHILE EXPB EXPI EXPI OP_CP {while_condition($3);}
	| OP_OP KW_WHILE EXPB EXPI OP_CP {$$ = (1 == $3) ? $4 : 0;}
	| OP_OP KW_SET IDENTIFIER EXPI OP_CP {$$ = $4; put_entry($3, $4);}
    | OP_OP KW_DISP EXPI OP_CP { $$ = $3;}
	| IDENTIFIER {$$ = get_entry($1);}
	| VALUE

;
EXPS : OP_OP KW_DISP OP_OC VALUESTR OP_CC OP_CP { strcpy($$,$4); printf("Print: %s\n", $4);}
; 
BinaryValue: KW_TRUE {$$ = 1;}
	| KW_FALSE {$$ = 0;}
;
%%
int entry_counter = 0;
struct entry symtable[100];
void printResult(float value){
	printf("Syntax OK.\n");
	printf("Result: %.2f\n",value);
}
void yyerror (char *array) {
	printf("SYNTAX_ERROR Expression not recognized\n");
} 
int if_condition(int statement, int value1,int value2){
	if(statement)
		return value1;
	else
		return value2;
}
int while_condition(int statement){
	printf("%d\n",statement);
	if(statement)
		return yyparse();
	return 0;
}
void put_entry(char *name, int value){
    int i;
    for(i = 0; i < entry_counter; i++)
        if(strcmp(symtable[i].name, name) == 0){
            symtable[i].value = value;
            return;
        }
    strcpy(symtable[entry_counter].name, name);
    symtable[entry_counter++].value = value;
}
int get_entry(char *name){
    int i;
    for(i = 0; i < entry_counter; i++)
        if(strcmp(symtable[i].name, name) == 0)
            return symtable[i].value;
    printf("Undefined variable %s\n", name);
    return 0;
}
int power(int base, int exp){
    int result = 1;
    while(exp){
        if(exp & 1)
            result *= base;
        exp >>= 1;
        base *= base;
    }
    return result;
}
int main(int argc, char **argv){
    return yyparse();
}