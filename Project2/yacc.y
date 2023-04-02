%token START FINISH TAB NL
%token FUNCTION ARRAY VAR IDENTIFIER
%token TRUE FALSE
%token LP RP CURLY_LP CURLY_RP SQUARE_RP SQUARE_LP
%token SC COMMA
%token IF FOR ELSE RETURN WHILE INPUT OUTPUT
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
     	| stmt NL
	| stmt NL stmts
	| stmt stmts
stmt: assign_stmt
    	| logical_stmt
	| if_stmt
	| loop_stmt
	| return_stmt
	| var_dec
	| input_statement
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
    | IDENTIFIER logical_op logical_stmt
    | IDENTIFIER
if_stmt: IF logical_stmt CURLY_LP stmts CURLY_RP
    | IF logical_stmt CURLY_LP stmts CURLY_RP  ELSE CURLY_LP  stmts CURLY_RP
    | IF logical_stmt CURLY_LP stmts CURLY_RP else_if_stmt
else_if_stmt: ELSE IF logical_stmt CURLY_LP stmts CURLY_RP
    | ELSE IF logical_stmt CURLY_LP stmts CURLY_RP else_if_stmt
loop_stmt: while_loop | for_loop
while_loop: WHILE logical_stmt CURLY_LP stmts CURLY_RP
	  | WHILE logical_stmt CURLY_LP NL stmts CURLY_RP
	  | WHILE logical_stmt NL CURLY_LP stmts CURLY_RP
for_loop: FOR LP assign_stmt SC logical_stmt SC assign_stmt RP CURLY_LP stmts CURLY_RP
function_declaration : FUNCTION IDENTIFIER LP identifiers RP CURLY_LP  stmts CURLY_RP 
identifiers : IDENTIFIER 
    | IDENTIFIER COMMA identifiers
    ;
function_def: FUNCTION IDENTIFIER LP param RP CURLY_LP stmts CURLY_RP
| FUNCTION IDENTIFIER LP RP CURLY_LP stmts return_stmt CURLY_RP 
    | FUNCTION IDENTIFIER LP RP CURLY_LP stmts CURLY_RP
    | FUNCTION IDENTIFIER LP param RP CURLY_LP stmts CURLY_RP
param: VAR IDENTIFIER 
    | VAR IDENTIFIER COMMA param 
array_structure: ARRAY identifiers ASSIGNMENT_OP SQUARE_LP elems SQUARE_RP
    | ARRAY identifiers ASSIGNMENT_OP SQUARE_LP SQUARE_RP
elems: elem 
	| elem COMMA elems
elem: IDENTIFIER
    | bool
input_statement: IDENTIFIER ASSIGNMENT_OP INPUT LP RP SC
%%
#include "lex.yy.c"
void yyerror(char *s){
printf("%s, line: %d\n", s, yylineno);
}
int main(){
	yyparse();
	return 0;
}
