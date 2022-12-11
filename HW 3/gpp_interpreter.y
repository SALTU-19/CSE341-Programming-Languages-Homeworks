%{
	#include <string.h>
	#include <stdio.h>
	#include <stdlib.h>
	#include <math.h>
	void yyerror (char *array);
	int yylex(void);
	void printResult(float num);
	void print_list();
	void add_to_list(int num);
	void if_condition(int value);
	void if_condition2(int value,int array1[],int array2[]);
	void for_loop(int initial,int final);
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

%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEFFUN KW_FOR KW_IF KW_EXIT KW_LOAD KW_DISP KW_TRUE KW_FALSE
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
    | EXPI
	| EXPB {printResult($1); }
	| EXPLISTI {printf("Syntax OK.\n");print_list(); }
    | KW_EXIT {exit(0);}
    | COMMENT {printf("Syntax OK.\n");}

;
EXPB : OP_OP KW_AND EXPB EXPB OP_CP { $$ = $3 && $4;}
	| OP_OP KW_OR EXPB EXPB OP_CP { $$ = $3 || $4;}
	| OP_OP KW_NOT EXPB OP_CP { $$ = !$3;}
	| OP_OP KW_LESS EXPB EXPB OP_CP { $$ = $3 < $4;}
	| OP_OP KW_EQUAL EXPB EXPB OP_CP { $$ = $3 == $4;}
	| OP_OP KW_EQUAL EXPI EXPI OP_CP { $$ = $3 == $4;}
	| EXPI 
	| BinaryValue {$$ = $1;}
;
EXPI : OP_OP OP_PLUS EXPI EXPI OP_CP { $$ = $3 + $4; printResult($$);}
	| OP_OP OP_MINUS EXPI EXPI OP_CP { $$ = $3 - $4; printResult($$);}
	| OP_OP OP_DIV EXPI EXPI OP_CP { $$ = $3 / $4; if($4 == 0) {printf("Zero Division Error!");} else  printResult($$);}
    | OP_OP OP_MULT EXPI EXPI OP_CP { $$ = $3 * $4; printResult($$);}
	| OP_OP OP_DBLMULT EXPI EXPI OP_CP { $$ = power($3,$4);printResult($$);}
	| OP_OP KW_IF EXPB EXPLISTI EXPLISTI OP_CP {printf("Syntax OK.\n");}
	| OP_OP KW_IF EXPB EXPLISTI OP_CP {if_condition($3);print_list();}
	| OP_OP KW_FOR OP_OP IDENTIFIER EXPI EXPI OP_CP EXPLISTI OP_CP {for_loop($5,$6);}
	| OP_OP KW_DEFFUN IDENTIFIER OP_OP IDENTIFIER OP_CP EXPI OP_CP{}
	| OP_OP KW_SET IDENTIFIER EXPI OP_CP {printf("Syntax OK.\n"); $$ = $4; put_entry($3, $4);}
	| OP_OP IDENTIFIER EXPLISTI OP_CP {printf("Syntax OK.\n");}
    | OP_OP KW_DISP EXPI OP_CP { $$ = $3; printf("Print: %.2f\n", $3);}
	| IDENTIFIER {$$ = get_entry($1);}
	| VALUE

;
EXPS : OP_OP KW_DISP OP_OC VALUESTR OP_CC OP_CP { strcpy($$,$4); printf("Print: %s\n", $4);}
    | OP_OP KW_LOAD OP_OC VALUESTR OP_CC OP_CP { strcpy($$,$4); printf("Syntax OK.\n");}
; 
EXPLISTI : OP_OP KW_LIST  VALUES  OP_CP 
	| OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP 
	| OP_OP KW_APPEND EXPI EXPLISTI OP_CP {add_to_list($3);}
	| LISTVALUE 
;
LISTVALUE : OP_OC  OP_OP VALUES OP_CP
	| OP_OC OP_OP OP_CP
	| KW_NIL
;
BinaryValue: KW_TRUE {$$ = 1;}
	| KW_FALSE {$$ = 0;}
;
VALUES : VALUES VALUE {add_to_list($2);}
    | VALUE {add_to_list($1);}
;
%%
int array[1000];
int counter = 0;
int entry_counter = 0;
struct entry symtable[100];
void printResult(float value){
	printf("Syntax OK.\n");
	printf("Result: %.2f\n",value);
}
void yyerror (char *array) {
	printf("SYNTAX_ERROR Expression not recognized\n");
} 
void add_to_list(int num){
	array[counter] = num;
	counter++;
}
void if_condition(int value){
	if(value){
		print_list();
	}
}
void if_condition2(int value,int array1[],int array2[]){
	if(value){
		printf("Result: (");
		for(int i=0;i<counter;i++){
			printf("%d ",array1[i]);
		}
		printf(")\n");
	}
	else
		printf("Result: (");
		for(int i=0;i<counter;i++){
			printf("%d ",array2[i]);
		}
		printf(")\n");
}
void for_loop(int initial, int final){
	for(int i= initial; i<final;i++)
		print_list();
}
void print_list(){
	if(counter != 0){
		printf("Result: (");
		for(int i=0;i<counter;i++){
			printf("%d ",array[i]);
		}
		printf(")\n");
		counter = 0;
	}
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
    FILE *yyin;   
    if(argc == 2){
        yyin = fopen(argv[1], "r");
        if(yyin ==  NULL){
            printf("File not found\n");
            return 1;
        }
        else{
            yyparse();
            fclose(yyin);
        }
    }
    else{
	    return yyparse();
    }
}