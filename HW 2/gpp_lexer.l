/*** Definition Section has one variable
which can be accessed inside yylex()
and main() ***/
%{
int quotesCount = 0;
%}

/*** Rule Section has three rules, first rule
matches with capital letters, second rule
matches with any character except newline and
third rule does not take input after the enter***/
%%

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
[ ] {}
[a-zA-Z][a-zA-Z0-9]* {printf("IDENTIFER\n");}
[0-9][0-9a-zA-Z]+ {printf("SYNTAX_ERROR %s cannot be tokenized\n", yytext); return 0;}
.	 {printf("SYNTAX_ERROR %s cannot be tokenized\n", yytext);}
\n {return 0;}
%%

/*** Code Section prints the number of
capital letter present in the given input***/
int yywrap(){}
int main(){

// Explanation:
// yywrap() - wraps the above rule section
/* yyin - takes the file pointer
		which contains the input*/
/* yylex() - this is the main flex function
		which runs the Rule Section*/
// yytext is the text in the buffer

// Uncomment the lines below
// to take input from file
// FILE *fp;
// char filename[50];
// printf("Enter the filename: \n");
// scanf("%s",filename);
// fp = fopen(filename,"r");
// yyin = fp;
printf("Enter string: ");
yylex();


return 0;
}
