
%{
int quotesCount = 0;
%}


%%

";;".* {printf("%s: COMMENT\n",yytext);}
and {printf("%s: KW_AND\n",yytext);}
or {printf("%s: KW_OR\n",yytext);}
not {printf("%s: KW_NOT\n",yytext);}
equal {printf("%s: KW_EQUAL\n",yytext);} 
less {printf("%s: KW_LESS\n",yytext);} 
nil {printf("%s: KW_NIL\n",yytext);} 
list {printf("%s: KW_LIST\n",yytext);} 
append {printf("%s: KW_APPEND\n",yytext);} 
concat {printf("%s: KW_CONCAT\n",yytext);}
set {printf("%s: KW_SET\n",yytext);} 
deffun {printf("%s: KW_DEFFUN\n",yytext);}
for {printf("%s: KW_FOR\n",yytext);}
if {printf("%s: KW_IF\n",yytext);}
exit {printf("%s: KW_EXIT\n",yytext);}
load {printf("%s: KW_LOAD\n",yytext);}
disp {printf("%s: KW_DISP\n",yytext);}
true {printf("%s: KW_TRUE\n",yytext);}
false {printf("%s: KW_FALSE\n",yytext);}
[+] {printf("%s: OP_PLUS\n",yytext);}
[-] {printf("%s: OP_MINUS\n",yytext);}
[/] {printf("%s: OP_DIV\n",yytext);}
[*] {printf("%s: OP_MULT\n",yytext);}
[(] {printf("%s: OP_OP\n",yytext);}
[)] {printf("%s: OP_CP\n",yytext);}
[*][*] {printf("%s: OP_DBLMULT\n",yytext);}
["] {
    if(quotesCount % 2 == 0){
        printf("%s: OP_OC\n",yytext);
        quotesCount++;
    }
    else
        printf("%s: OP_CC\n",yytext);
}
[,] {printf("%s: OP_COMMA\n",yytext);}
[0-9]+ {
    
    if(quotesCount % 2 == 0){
        printf("%s: VALUEI\n",yytext);
    }
    else
        printf("%s: VALUESTR\n",yytext);
    
}
[0-9]+[f][0-9]+ {
    if(quotesCount % 2 == 0){
        printf("%s: VALUEF\n",yytext);
    }
    else
        printf("%s: VALUESTR\n",yytext);
}
[ ] {}
[a-zA-Z][a-zA-Z0-9]* {
    if(quotesCount % 2 == 0){
        printf("%s: IDENTIFIER\n",yytext);
    }
    else
        printf("%s: VALUESTR\n",yytext);
}
[0-9][0-9a-zA-Z]+ {printf("LEXICAL ERROR: %s identifier can not be start number\n", yytext); return 0;}
.	 {printf("SYNTAX_ERROR %s cannot be tokenized\n", yytext);}
\n\n {return 0;}
%%


int yywrap(){}
int main(){

printf("Enter string(for exit enter '\\n\\n'): ");
yylex();


return 0;
}
