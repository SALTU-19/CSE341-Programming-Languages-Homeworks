
%{
    #include "y.tab.h"
    #include <math.h>
    #include <stdlib.h>
    int quotesCount = 0;
    float parseFloat(char *str){
        int i = 0;
        float result = 0;
        while(str[i] != 'f'){
            result = result * 10 + (str[i] - '0');
            i++;
        }
        i++;
        float decimal = 0;
        int j = 0;
        while(str[i] != '\0'){
            decimal = decimal * 10 + (str[i] - '0');
            i++;
            j++;
        }
        result = result / decimal;
        return result;
    }
%}


%%

;;.* {return COMMENT;}
and {return KW_AND;}
or {return KW_OR;}
not {return KW_NOT;}
equal {return KW_EQUAL;} 
less {return KW_LESS;} 
nil {return KW_NIL;} 
list {return KW_LIST;} 
append {return KW_APPEND;} 
concat {return KW_CONCAT;}
set {return KW_SET;} 
deffun {return KW_DEFFUN;}
for {return KW_FOR;}
if {return KW_IF;}
exit {return KW_EXIT;}
load {return KW_LOAD;}
disp {return KW_DISP;}
true {yylval.num = 1; return KW_TRUE;}
false {yylval.num = 0; return KW_FALSE;}
[+] {return OP_PLUS;}
[-] {return OP_MINUS;}
[/] {return OP_DIV;}
[*] {return OP_MULT;}
[(] {return OP_OP;}
[)] {return OP_CP;}
[*][*] {return OP_DBLMULT;}
["] {
    if(quotesCount % 2 == 0){
        quotesCount++;
        return OP_OC;
    }
    else
        return OP_CC;
}
[,] {return OP_COMMA;}
[0-9]+ {yylval.num = atoi(yytext);return VALUE;}
[0-9]+[f][0-9]+ {yylval.num = parseFloat(yytext); return VALUE;}
[[:space:]]+ 		/* ignore space */
[a-zA-Z][a-zA-Z0-9]* {
    if(quotesCount % 2 == 0){
        //printf("%s: IDENTIFIER\n",yytext);
        strcpy(yylval.string, yytext);
        return IDENTIFIER;
    }
    else
        //printf("%s: VALUESTR\n",yytext);
        strcpy(yylval.string, yytext);
        return VALUESTR;
    quotesCount++;
}
[0-9][0-9a-zA-Z]+ {printf("LEXICAL ERROR: %s identifier can not be start number\n", yytext); return 0;}
.	 {printf("SYNTAX_ERROR %s cannot be tokenized\n", yytext); return 0;}
%%



int yywrap(){}
/*int main(int argc, char *argv[]){

// Uncomment the lines below 
// to take input from file
FILE *fp;
char *filename;
if(argv[1] != NULL){
    filename = argv[1];
    fp = fopen(filename, "r");
    if(fp == NULL){
        printf("File not found\n");
        return 0;
    }
    yyin = fp;
}
else{
    printf("Enter string: ");
}
yylex();


return 0;
}*/
