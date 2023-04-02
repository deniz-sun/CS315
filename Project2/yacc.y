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
	| START NL FINISH
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
	| identifiers
	| NOT expr
assign_stmt:  mult_var ASSIGNMENT_OP expr SC
	| VAR mult_var ASSIGNMENT_OP expr SC
var_dec: VAR identifiers SC
	| VAR identifiers COMMA mult_var SC
mult_var: identifiers
	| identifiers COMMA mult_var
logical_op: EQUALS_OP
	| NOT_EQUALS
	| AND
	| OR
	| IMPLIES
	| DOUBLE_IMPLIES
bool: TRUE | FALSE
logical_stmt: LP identifiers logical_op logical_stmt RP
    | elem logical_op logical_stmt
    | elem
if_stmt: IF logical_stmt CURLY_LP stmts CURLY_RP
    | IF logical_stmt CURLY_LP stmts CURLY_RP  ELSE CURLY_LP  stmts CURLY_RP
    | IF logical_stmt CURLY_LP stmts CURLY_RP else_if_stmt
else_if_stmt: ELSE IF logical_stmt CURLY_LP stmts CURLY_RP
    | ELSE IF logical_stmt CURLY_LP stmts CURLY_RP else_if_stmt
loop_stmt: while_loop | for_loop
while_loop: WHILE logical_stmt CURLY_LP stmts CURLY_RP
for_loop: FOR LP assign_stmt SC logical_stmt SC assign_stmt RP CURLY_LP stmts CURLY_RP 
function_def: FUNCTION identifiers LP param RP CURLY_LP stmts CURLY_RP
    | FUNCTION identifiers LP RP CURLY_LP stmts CURLY_RP 
function_call: identifiers LP RP SC
	     | identifiers LP elems RP SC
param: VAR identifiers 
    | VAR identifiers COMMA param 
array_structure: ARRAY identifiers ASSIGNMENT_OP SQUARE_LP elems SQUARE_RP
    | ARRAY identifiers ASSIGNMENT_OP SQUARE_LP SQUARE_RP
elems: elem 
	| elem COMMA elems
elem: identifiers
    | bool
comment: "/" "*" string_content "*" "/"
      | "/" "/" single_line_content
identifiers: ALPHABETICAL | IDENTIFIER
input_stmt: identifiers ASSIGNMENT_OP INPUT LP RP SC
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
