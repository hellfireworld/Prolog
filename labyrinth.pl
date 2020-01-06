valid(M/N,Barriers_list):-
	M>0,N>0,
	M =<5,N=<9,
	not(member(M/N, Barriers_list)).

can_go(X/Y,MX/Y,Barriers_list):- 
	MX is X + 1,
	valid(X/Y,Barriers_list),  %elenxos tou prwtou shmeiou
	valid(MX/Y,Barriers_list). %elenxos tou deuterou shmeiou
can_go(X/Y,MX/Y,Barriers_list):- 
	MX is X - 1,
	valid(X/Y,Barriers_list),
	valid(MX/Y,Barriers_list).
can_go(X/Y,X/MY,Barriers_list):- 
	MY is Y + 1,
	valid(X/Y,Barriers_list),
	valid(X/MY,Barriers_list).
can_go(X/Y,X/MY,Barriers_list):- 
	MY is Y - 1,
	valid(X/Y,Barriers_list),
	valid(X/MY,Barriers_list).

solve_maze(M,N,Barriers_list,From,To,Path):-
	M=<5, N=<9, %% theloume kanonika M is 5, N is 9 is alla to vazw kai gia allous lavurinthous
	valid(From,Barriers_list), %%to validation tou starting point(From)
	valid(To,Barriers_list), %%to validation tou ending point(To)
	breadthfirst([[From]], To, M, N, Barriers_list, Path). %solution1(swsth lush)
	%depthfirst_cyclefree([From], To,M, N, Barriers_list, Path). %solution2(nomizw doulevei adistrofa kai borei na vrei toixo kapoies fores)

goal(Statea, Stateb) :- 
	Statea = Stateb. %%kanonika tha evaza 2 ison alla an valoume na doulepsei o depthfirst_cyclefree den doulevei me 2.

move_cyclefree(Visited, Node, NextNode, Barriers_list) :-
	can_go(Node,NextNode,Barriers_list),
	\+member(NextNode, Visited).

%%%%solution1 BFS
expand_path(Path, NewPath, Barriers_list) :-
	last(Path, LastNode),
move_cyclefree(Path, LastNode, NewLastNode, Barriers_list),
	append(Path, [NewLastNode], NewPath).
expand_breadthfirst(Path, ExpPaths, Barriers_list) :-
	findall(NewPath, expand_path(Path, NewPath, Barriers_list), ExpPaths).
breadthfirst([Path|_], To, M, N, Barriers_list, Path) :-
	last(Path,Goal),
	goal(Goal, To).
breadthfirst([Path|Paths], To, M, N, Barriers_list, SolutionPath) :-
	expand_breadthfirst(Path, ExpPaths, Barriers_list),
	append(Paths, ExpPaths, NewPaths),
	breadthfirst(NewPaths, To, M, N, Barriers_list,SolutionPath).
%%efoson theloume mono to pio sudomo monopati svhnoume thn teleutaia teleia kai vazoume ,!.

%%%%solution2 Cycle-free Depth-first Search
depthfirst_cyclefree(_, Node, M, N, Barriers_list, []) :-
	goal(Node, To).
depthfirst_cyclefree(Visited, Node, M, N, Barriers_list, [Node|Path]) :- %Adi gia LastNode|Path -> Node|Path
	move_cyclefree(Visited, Node, NextNode, Barriers_list),
	depthfirst_cyclefree([NextNode|Visited],NextNode, M, N, Barriers_list, Path).
