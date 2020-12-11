parse: mini_l.lex mini_l.yy
	bison -v -d --file-prefix=y mini_l.yy
	flex mini_l.lex
	g++ -std=c++11 -g -o parser y.tab.cc lex.yy.c -lfl

clean:
	rm -f lex.yy.c y.tab.* y.output *.o parser location.* position.* stack.* 1.mil 
