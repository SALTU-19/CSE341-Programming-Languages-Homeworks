
%{
int quotesCount = 0;
%}


%%

";;".* {printf("COMMENT\n");}
and {printf("KW_AND\n");}
or {printf("KW_OR\n");}
not {printf("KW_NOT\n");}
equal {printf("KW_EQUAL\n");} 
less {printf("KW_LESS\n");} 
nil {printf("KW_NIL\n");} 
list {printf("KW_LIST\n");} 
append {printf("KW_APPEND\n");} 
concat {printf("KW_CONCAT\n");}
set {printf("KW_SET\n");} 
deffun {printf("KW_DEFFUN\n");}
for {printf("KW_FOR\n");}
if {printf("KW_IF\n");}
exit {printf("KW_EXIT\n");}
load {printf("KW_LOAD\n");}
disp {printf("KW_DISP\n");}
true {printf("KW_TRUE\n");}
false {printf("KW_FALSE\n");}
[+] {printf("OP_PLUS\n");}
[-] {printf("OP_MINUS\n");}
[/] {printf("OP_DIV\n");}
[*] {printf("OP_MULT\n");}
[(] {printf("OP_OP\n");}
[)] {printf("OP_CP\n");}
[*][*] {printf("OP_DBLMULT\n");}
["] {
    if(quotesCount % 2 == 0){
        printf("OP_OC\n");
        quotesCount++;
    }
    else
        printf("OP_CC\n");
}
[,] {printf("OP_COMMA\n");}
[0-9]+ {printf("VALUEI\n");}
[0-9]+[f][0-9]+ {printf("VALUEF\n");}
[ ] {}
[a-zA-Z][a-zA-Z0-9]* {printf("IDENTIFER\n");}
[0-9][0-9a-zA-Z]+ {printf("SYNTAX_ERROR %s cannot be tokenized\n", yytext); return 0;}
.	 {printf("SYNTAX_ERROR %s cannot be tokenized\n", yytext);}
\n {return 0;}
%%


int yywrap(){}
int main(){

printf("Enter string: ");
yylex();


return 0;
}
