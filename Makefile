checker: cc.cpp c.tab.o c.lex.o ast.o
	clang++ -g c.tab.o c.lex.o cc.cpp ast.o -lm -ll -lfl `llvm-config --cxxflags --ldflags --system-libs --libs core` -o $@

ast.o: ast.cpp ast.hpp
	clang++ -g -c ast.cpp

c.tab.o: c.tab.cpp c.tab.hpp ast.hpp
	clang++ -g -c c.tab.cpp

c.tab.cpp c.tab.hpp: c.y
	bison -o c.tab.cpp -d c.y

c.lex.o: c.lex.cpp c.tab.hpp
	clang++ -g -c c.lex.cpp

c.lex.cpp: c.l c.tab.hpp
	flex -o c.lex.cpp -l c.l

clean:
	rm -f c.tab.cpp c.tab.hpp c.lex.cpp *.o checker
