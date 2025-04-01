run: 
	./mill --ticker false run

gv:
	dot -Tsvg program.dot > resources/program.svg
