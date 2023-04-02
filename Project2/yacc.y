%token START FINISH TAB NL
%token FUNCTION ARRAY VAR IDENTIFIER
%token TRUE FALSE
%token LP RP CURLY_LP CURLY_RP SQUARE_RP SQUARE_LP
%token SC COMMA
%token IF FOR ELSE RETURN WHILE INPUT OUTPUT
%token ALPHABETIC NUMERIC
%token STRING SINGLE_LINE_COMMENT MULTIPLE_LINE_COMMENT
%right NOT
%right ASSIGNMENT_OP
%left LP RP
%left NOT_EQUALS EQUALS_OP
%left AND
%left OR
%left IMPLIES
%left DOUBLE_IMPLIES
%%
program: START NL stmts FINISH
stmts: stmt
     	| NL stmts
     	| stmt NL
	| stmt NL stmts
	| stmt stmts
stmt: assign_stmt
    	| logical_stmt
	| if_stmt
	| loop_stmt
	| return_stmt
	| var_dec
	| input_stmt
	| output_stmt
return_stmt: RETURN expr SC
expr: expr logical_op expr
	| bool
	| LP expr RP
	| IDENTIFIER
	| NOT expr
assign_stmt:  mult_var ASSIGNMENT_OP expr SC
	| VAR mult_var ASSIGNMENT_OP expr SC
var_dec: VAR IDENTIFIER SC
	| VAR IDENTIFIER COMMA mult_var SC
mult_var: IDENTIFIER
	| IDENTIFIER COMMA mult_var
logical_op: EQUALS_OP
	| NOT_EQUALS
	| AND
	| OR
	| IMPLIES
	| DOUBLE_IMPLIES
bool: TRUE | FALSE
logical_stmt: LP IDENTIFIER logical_op logical_stmt RP
    | bool logical_op logical_stmt
    | bool
if_stmt: IF logical_stmt CURLY_LP stmts CURLY_RP
    | IF logical_stmt CURLY_LP stmts CURLY_RP  ELSE CURLY_LP  stmts CURLY_RP
    | IF logical_stmt CURLY_LP stmts CURLY_RP else_if_stmt
else_if_stmt: ELSE IF logical_stmt CURLY_LP stmts CURLY_RP
    | ELSE IF logical_stmt CURLY_LP stmts CURLY_RP else_if_stmt
loop_stmt: while_loop | for_loop
while_loop: WHILE logical_stmt CURLY_LP stmts CURLY_RP
for_loop: FOR LP assign_stmt SC logical_stmt SC assign_stmt RP CURLY_LP stmts CURLY_RP 
function_def: FUNCTION IDENTIFIER LP param RP CURLY_LP stmts CURLY_RP
    | FUNCTION IDENTIFIER LP RP CURLY_LP stmts CURLY_RP 
function_call: IDENTIFIER LP RP SC
	     | IDENTIFIER LP elems RP SC
param: VAR IDENTIFIER 
    | VAR IDENTIFIER COMMA param 
array_structure: ARRAY IDENTIFIER ASSIGNMENT_OP SQUARE_LP elems SQUARE_RP
    | ARRAY IDENTIFIER ASSIGNMENT_OP SQUARE_LP SQUARE_RP
elems: elem 
	| elem COMMA elems
elem: IDENTIFIER
    | bool
comment: "/" "*" string_content "*" "/"
      | "/" "/" single_line_content 
input_stmt: IDENTIFIER ASSIGNMENT_OP INPUT LP RP SC
output_stmt: OUTPUT LP expr RP SC
		| OUTPUT LP string RP SC
string: '\"' '\"' | '\"' string_content '\"'
single_line_content: alphanumeric | alphanumeric single_line_content
string_content: '\n' | '|n' string_content | alphanumeric | alphanumeric string_content
alphanumeric: ALPHABETIC | NUMERIC
%%
#include "lex.yy.c"
void yyerror(char *s){
printf("%s, line: %d\n", s, yylineno);
}
int main(){
	yyparse();
	return 0;
}
