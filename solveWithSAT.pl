:-include(entradaRodear1).
:-dynamic(varNumber/3).
symbolicOutput(0). % set to 1 to see symbolic output only; 0 otherwise.

%% *********************************************************************************************** %
%%                                                                                                 *
%%                                       Lògica a la Informàtica                                   *
%%    \_____________________________________________________________________________________/      *
%%    /																						\      *
%%                                       CICLES WRAPPING CELLS:                                    *
%%                                      25/04/2015, Qm Primavera                                   *
%%                                                                                                 *
%%                                                                                                 *
%% *************************************************************************************************
%%                                                                                                 *
%%                                        Arnau Sangrà Rocamora                                    *
%%                                                                                                 *
%% *********************************************************************************************** %

symbolicOutput(0). % set to 1 to see symbolic output only; 0 otherwise.

writeClauses:- restrict_cells, constraints_for_intersections.

restrict_cells:-
	num(I,J,K), restrict_cell(I,J,K), fail.
restrict_cells.

%% cells with k = 0 don't have wrapping edges.
restrict_cell(I,J,0):- 
	I1 is I + 1,
	J1 is J + 1,
	writeClause( [ \+h-I-J ] ),
	writeClause( [ \+h-I1-J ] ),
	writeClause( [ \+v-I-J ] ),
	writeClause( [ \+v-I-J1 ] ).
%% cell with k != 0 has exactly "k" wrapping edges.
restrict_cell(I,J,K):- 
	at_least_k_cell(I,J,K),
	at_most_k_cell(I,J,K).

get_list(I,J,L):-
	I1 is I + 1, J1 is J + 1,
	L = [ h-I-J, h-I1-J, v-I-J, v-I-J1 ].

%% ensure that every numbered cell has at least the minimum edges
at_least_k_cell(I,J,K):-
	get_list(I,J,L),
	%% at_least_k_list(L,K).
	length(L,NL),
	K1 is K - 1,
	K2 is NL - K1,
	subset(L,S),
	length(S,K2),
	writeClause(S).

%% ensure that every numbered cell has at most K wrapping edges
at_most_k_cell(I,J,K):-
	get_list(I,J,L),
	%% at_most_k_list(L,K),
	K1 is K + 1,
	subset(L,S),
	length(S,K1),
	invert_list(S,C), %make all literals of S negative and store in C
	writeClause(C).

%% store all possible subsets of L
subset([],[]).
subset([X|S],[X|Ss]):- subset(S,Ss).
subset([_|S],Ss):- subset(S,Ss).

%% invert all the variables from the list L
invert_list([],[]).
invert_list([V-I-J|L],C):-
	invert_list(L,N),
	C = [\+V-I-J|N].
invert_list([\+V-I-J|L],C):-
	invert_list(L,N),
	C = [V-I-J|N].

% for every intersection, there can only be either 0 or 2 edges set in order
% to avoid creating cicles that might intersect other cicles.
constraints_for_intersections:-
	rows(N), columns(M),
	N1 is N + 1, M1 is M + 1,
	between(1,N1,I), between(1,M1,J),
	restrict_intersection(I,J), fail.
constraints_for_intersections.

restrict_intersection(I,J):- 
	edges_of_intersection(I,J,L),
	either_0_2(L).

%% get edges that conform a given interserction, board edges are special
edges_of_intersection(I,J,L):-
	I1 is I - 1, J1 is J - 1,
	get_h_edge(I,J,H1), %h_i_j
	append([],H1,L1),
	get_h_edge(I,J1,H2), %h_i_j-1
	append(L1,H2,L2),
	get_v_edge(I,J,V1), %v_i_j
	append(L2,V1,L3),
	get_v_edge(I1,J,V2), %v_i-1_j
	append(L3,V2,L).

%% horz. edges \in [1..N+1]x[1..M]
get_h_edge(I,J,[]):- columns(M), (I < 1; J < 1; J > M).
get_h_edge(I,J,[h-I-J]).

%% vert edges \in [1..N]x[1..M+1]
get_v_edge(I,J,[]):- rows(N), (I < 1; J < 1; I > N).
get_v_edge(I,J,[v-I-J]).

%% intersection not set (0) || at most 2 edges. 
either_0_2(L):-
	subset(L,S),
	length(S,3),
	invert_list(S,C),
	writeClause(C),
	only_one__another_must_be_set(L).

%% to forbid only 1 edge, if it is set, then an adjacent must be set
only_one__another_must_be_set(L):-
	subset(L,S),
	length(S,1),
	get_all_others_from_list(L,S,R),
	%% if one edge is set, it implies its adjacents
	write_implication_clause(S,R).

%% get all the elements that from the list that differ S.
get_all_others_from_list([],_,[]).
get_all_others_from_list([X|L],[S],[X|R]):- X \= S, get_all_others_from_list(L,[S],R).
get_all_others_from_list([X|L],[S],R):- X = S, get_all_others_from_list(L,[S],R).

%% (H -> T) == (¬H v T):
write_implication_clause(H,T):-
	invert_list(H,N),
	append(N,T,C),
	writeClause(C).

%% displaySol([]).
%% displaySol([Nv|S]):- num2var(Nv,x-I-J), write(I), write(': color '), write(J), nl, displaySol(S).

%% ============================= DISPLAY: =====================================
writeHeaderPS:-
    writeln('%!PS'),
    writeln('matrix currentmatrix /originmat exch def'),
    writeln('/umatrix {originmat matrix concatmatrix setmatrix} def'),
    writeln('[28.3465 0 0 28.3465 10.5 100.0] umatrix').

writeGrid:-
    writeln('0.01 setlinewidth'),
    writeVertGrid,
    writeHorizGrid.

writeVertGrid:-
    rows(R), columns(C), C1 is C+1,
    between(1,R,I), between(1,C1,J), drawVertical(I,J),fail.
writeVertGrid.

writeHorizGrid:-
    rows(R), columns(C), R1 is R+1,
    between(1,R1,I), between(1,C,J), drawHorizontal(I,J),fail.
writeHorizGrid.

drawVertical(I,J):-
    rows(R),columns(C),
    Size is min(22/R,18/C),
    X is 1+(J-1)*Size,
    Y is 23-(I-1)*Size,
    write(X), write(' '), write(Y), write(' moveto'),nl,
    Y1 is Y-Size,
    write(X), write(' '), write(Y1), write(' lineto'),nl,
    writeln('stroke').

drawHorizontal(I,J):-
    rows(R),columns(C),
    Size is min(22/R,18/C),
    X is 1+(J-1)*Size,
    Y is 23-(I-1)*Size,
    write(X), write(' '), write(Y), write(' moveto'),nl,
    X1 is X+Size,
    write(X1), write(' '), write(Y), write(' lineto'),nl,
    writeln('stroke').

writeNumbers:-
    num(I,J,K),
    writeNumber(I,J,K),
    fail.
writeNumbers.

writeNumber(I,J,K):-
    rows(R),columns(C),
    Size is min(22/R,18/C),
    X is 1+(J-1)*Size + 3*Size/7,
    Y is 23-(I-1)*Size - 5*Size/7,
    writeln('0.001 setlinewidth'),
    S is Size/2,
    write('/Times-Roman findfont '), write(S), writeln(' scalefont setfont'),
    write(X), write(' '), write(Y), write(' moveto ('), write(K), writeln(') show').

writeSolution([X|M]):-
    writeLine(X),
    writeSolution(M).
writeSolution([]).
    
writeLine(X):-num2var(X,h-I-J),!,
    rows(R), columns(C), T is max(R,C),
    W is 2/T,
    write(W), 
    writeln(' setlinewidth'),
    drawHorizontal(I,J).
writeLine(X):-num2var(X,v-I-J),!,
    rows(R), columns(C), T is max(R,C),
    W is 2/T,
    write(W), 
    writeln(' setlinewidth'),
    drawVertical(I,J).
writeLine(_).

displaySol(M):-
    tell('graph.ps'),
    writeHeaderPS,
    writeGrid,
    writeNumbers,
    writeSolution(M),
    writeln('showpage'),
    told.

%% ============================================================================= %%
% ================== No need to change the following: =============================

main:- symbolicOutput(1), !, writeClauses, halt. % escribir bonito, no ejecutar
main:-  assert(numClauses(0)), assert(numVars(0)),
	tell(clauses), writeClauses, told,
	tell(header),  writeHeader,  told,
	unix('cat header clauses > infile.cnf'),
	unix('picosat -v -o model infile.cnf'),
	unix('cat model'),
	see(model), readModel(M), seen, displaySol(M),
	halt.

var2num(T,N):- hash_term(T,Key), varNumber(Key,T,N),!.
var2num(T,N):- retract(numVars(N0)), N is N0+1, assert(numVars(N)), hash_term(T,Key),
	assert(varNumber(Key,T,N)), assert( num2var(N,T) ), !.

writeHeader:- numVars(N),numClauses(C),write('p cnf '),write(N), write(' '),write(C),nl.

countClause:-  retract(numClauses(N)), N1 is N+1, assert(numClauses(N1)),!.
writeClause([]):- symbolicOutput(1),!, nl.
writeClause([]):- countClause, write(0), nl.
writeClause([Lit|C]):- w(Lit), writeClause(C),!.
w( Lit ):- symbolicOutput(1), write(Lit), write(' '),!.
w(\+Var):- var2num(Var,N), write(-), write(N), write(' '),!.
w(  Var):- var2num(Var,N),           write(N), write(' '),!.
unix(Comando):-shell(Comando),!.
unix(_).

readModel(L):- get_code(Char), readWord(Char,W), readModel(L1), addIfPositiveInt(W,L1,L),!.
readModel([]).

addIfPositiveInt(W,L,[N|L]):- W = [C|_], between(48,57,C), number_codes(N,W), N>0, !.
addIfPositiveInt(_,L,L).

readWord(99,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!.
readWord(-1,_):-!, fail. %end of file
readWord(C,[]):- member(C,[10,32]), !. % newline or white space marks end of word
readWord(Char,[Char|W]):- get_code(Char1), readWord(Char1,W), !.
%========================================================================================
