newline                         \n
tab                             \t
letter                          [a-zA-Z]
bool_true                       true|1
bool_false                      false|0
function                        foo|func1|func2|func3|func4|func5|func6|func7
array                           arr1|arr2|arr3|arr4|arr5|arr6|arr7|arr8
var_type                        var
array_type                      array
var                             [a-z]
%%
{newline}                       printf("\n");
{tab}                           printf("\t");
start                           printf("START ");
finish                          printf("FINISH ");
{var}                           printf("VARIABLE ");
{function}                      printf("FUNCTION ");
{array}                         printf("ARRAY_IDENTIFIER ");
{var_type}                      printf("VAR_TYPE ");
{array_type}                    printf("ARRAY_TYPE ");
{bool_true}                     printf("TRUE ");
{bool_false}                    printf("FALSE ");
\&\&                            printf("AND ");
\|\|                            printf("OR ");
\~                              printf("NOT ");
\!\=                            printf("NOT_EQUALS ");
\-\>                            printf("IMPLIES ");
\<\-\>                          printf("DOUBLE_IMPLIES ");
\=\=                            printf("EQUALS_OP ");
\=                              printf("ASSIGNMENT_OP ");
\)                              printf("RP ");
\(                              printf("LP ");
\{                              printf("CURLY_LP ");
\}                              printf("CURLY_RP ");
\]                              printf("SQUARE_RP ");
\[                              printf("SQUARE_LP ");
\;                              printf("SC ");
\,                              printf("COMMA ");
if                              printf("IF ");
else                            printf("ELSE ");
return                          printf("RETURN ");
while                           printf("WHILE ");
in                              printf("INPUT ");
out                             printf("OUTPUT ");
\"(.|\n)*\"                     printf("STRING ");
"//".*      		        printf("SINGLE_LINE_COMMENT ");
"/*"(.|\n)*"*/" 	     printf("MULTIPLE_LINE_COMMENT ");
.                               ;
%%
