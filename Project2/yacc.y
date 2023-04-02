%token START FINISH TAB
%token FUNCTION ARRAY VAR IDENTIFIER
%token TRUE FALSE
%token LP RP CURLY_LP CURLY_RP SQUARE_RP SQUARE_LP
%token SC COMMA
%token IF FOR ELSE ELSE_IF RETURN WHILE INPUT OUTPUT
%token ALPHABETIC NUMERIC
%token STRING SINGLE_LINE_COMMENT MULTIPLE_LINE_COMMENT
%right ASSIGNMENT_OP
%nonassoc NOT
%nonassoc NL
%left LP RP
%left NOT_EQUALS EQUALS_OP
%left AND
%left OR
%left IMPLIES
%left DOUBLE_IMPLIES

%%
program: START NL stmts FINISH %prec NL
        | START NL FINISH %prec NL
stmts:stmts_list
stmts_list: stmt
        | stmts_list NL stmt
stmt: assign_stmt %prec ASSIGNMENT_OP
        | logical_stmt
        | if_stmt
        | loop_stmt
        | return_stmt
        | var_dec
        | input_stmt
        | output_stmt
assign_stmt: direct_assign
        | declare_assign
direct_assign: mult_var ASSIGNMENT_OP expr SC
declare_assign: VAR mult_var ASSIGNMENT_OP expr SC
logical_stmt: LP identifiers logical_op logical_stmt RP
        | elem logical_op logical_stmt
        | elem {printf("saw elem");}
if_stmt: IF logical_stmt CURLY_LP stmts CURLY_RP
        | IF logical_stmt CURLY_LP stmts CURLY_RP  ELSE CURLY_LP  stmts CURLY_RP
        | IF logical_stmt CURLY_LP stmts CURLY_RP ELSE_IF else_if_stmt
loop_stmt: while_loop | for_loop
return_stmt: RETURN expr SC
var_dec: VAR identifiers SC
        | VAR identifiers COMMA mult_var SC
input_stmt: identifiers ASSIGNMENT_OP INPUT LP RP SC
output_stmt: OUTPUT LP expr RP SC
        | OUTPUT LP string RP SC
mult_var: identifiers
        | identifiers COMMA mult_var
expr: expr logical_op expr
        | elem
        | LP expr RP
        | NOT expr
logical_op: EQUALS_OP %prec EQUALS_OP
        | NOT_EQUALS %prec NOT_EQUALS
        | AND %prec AND
        | OR %prec OR
        | IMPLIES %prec DOUBLE_IMPLIES
        | DOUBLE_IMPLIES %prec DOUBLE_IMPLIES
else_if_stmt: LP logical_stmt RP CURLY_LP stmts CURLY_RP
        | LP logical_stmt RP CURLY_LP stmts CURLY_RP ELSE_IF else_if_stmt
        | LP logical_stmt RP CURLY_LP stmts CURLY_RP ELSE CURLY_LP stmts CURLY_RP
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
elems: elems_list
elems_list: elem
        | elems_list COMMA elem
elem: identifiers
        | bool
bool: TRUE | FALSE
comment: "/" "*" string_content "*" "/"
        | "/" "/" single_line_content
identifiers: ALPHABETIC | IDENTIFIER {printf("found identifier\n");}
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
