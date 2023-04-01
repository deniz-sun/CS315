%option yylineno
newline		\n
tab             \t
letter          [a-zA-Z]
bool_true       true|1
bool_false      false|0
function        function
array           array
var             var
identifier      [a-zA-Z][a-zA-Z0-9]*
%%
{newline}       return NL;
{tab}           return TAB;
start           return START;
finish          return FINISH;
{function}	return FUNCTION;
{array}		return ARRAY;
{var}	return VAR;
{bool_true}     return TRUE;
{bool_false}    return FALSE;
\&\&            return AND;
\|\|            return OR;
\~              return NOT;
\-\>            return IMPLIES;
\<\-\>          return DOUBLE_IMPLIES;
\=\=            return EQUALS_OP;
\=              return ASSIGNMENT_OP;
\)              return RP;
\(              return LP;
\{              return CURLY_LP;
\}              return CURLY_RP;
\[		return SQUARE_LP;
\]		return SQUARE_RP;
\;              return SC;
\,              return COMMA;
if              return IF;
else            return ELSE;
return          return RETURN;
while           return WHILE;
in              return INPUT;
out             return OUTPUT;
"//".*      	return SINGLE_LINE_COMMENT;
"/*"(.|\n)*"*/" return MULTIPLE_LINE_COMMENT;
\"(.|\n)*\"		return STRING;
{identifier}    return IDENTIFIER;
.           	;
%%
int yywrap() { return 1; }
