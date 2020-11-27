parse: mini_l.lex mini_l.yy
	bison -v -d --file-prefix=y mini_l.yy
	flex mini_l.lex
	gcc -o parser y.tab.c lex.yy.c -lfl

clean:
	rm -f lex.yy.c y.tab.* y.output *.o parser
