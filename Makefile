file = solveWithSAT

$(file): $(file).pl
	swipl -O -g main --stand_alone=true -o $(file) -c $(file).pl

clean:
	rm clauses graph.ps header infile.cnf model

