####################################################
# 			README

# Given a board with cells, some of those contains a number between [0..3].
# The number inside of the cell represents the total of its adjacent edges that must be set on true, not one less, nor one more.

# The problem consists on wrapping the cells of the board with cicles,
# but ensuring that the cicles do not overlap and the cell constraints are respected.

# The code provided to draw the solution (if found by the solver) has been included in the file solveWithSAT.pl.

# In order to change the instance of problem to be solved, it is necessary to modify the first line of solveWithSAT.pl
# with the desired filename inside the braquets of :-include().

# In order to check the written clauses, it is necessary to set the option of line 3, symbolicOutput to 1.

if [[ -a graph.ps ]]
    then
	echo "Cleaning files from other executions..."
	make clean
fi

echo "Calling Makefile to compile solveWithSAT.pl"
make

echo 'Running the executable file "solveWithSAT"'
./solveWithSAT

echo "Opening the result of the execution"
#OSX:
open graph.ps

#gnome
#gnome-open graph.ps &

#kde
#okular graph.ps &
