%option main
newline		\n
tab             \t
letter          [a-zA-Z]
bool_true       true|1
bool_false      false|0
function	foo|func1|func2|func3|func4|func5|func6|func7
array		arr1|arr2|arr3|arr4|arr5|arr6|arr7|arr8
var_type        var
array_type	array	
var		[a-z]
%%
{newline}       return NL;
{tab}           return TAB;
start           return START;
finish          return FINISH;
{var}	 	return VAR_IDENTIFIER;
{function}	return FUNCTION;
{array}		return ARRAY_IDENTIFIER;
{var_type}	return VAR_TYPE;
{array_type}	return ARRAY_TYPE;
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
\"\"		return STRING;
.           	;
%%
