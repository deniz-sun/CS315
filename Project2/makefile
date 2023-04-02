parser: lex.yy.c y.tab.c
	gcc -o parser y.tab.c
y.tab.c: yacc.y
	yacc yacc.y
lex.yy.c: lex.l
	lex lex.l