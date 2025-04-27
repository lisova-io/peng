run: 
	./mill --ticker false run

gv: run
	dot -Tsvg program.dot > resources/program.svg
