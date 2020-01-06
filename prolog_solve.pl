%%%%%%%%%%%%%%%%%%% Alexandros Tsevrenis MTN1914 %%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Serpico Snake %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%place_snake([a,b,c,d,e],9,9,5,Box).	?- place_snake([a,b,c,d],2,3,5,Box).
%epistrefei								epistrefei
%Box = [[a,b,c,d,e,a,b,c,d]		, Box = [[a,b,c],
%	   [c,b,a,e,d,c,b,a,e]		,		 [b,a,d],
%	   [d,e,a,b,c,d,e,a,b]		,		 [c,d,o],
%      [a,e,d,c,b,a,e,d,c]		,		 [o,o,o],
%	   [b,c,d,e,a,b,c,d,e]]		,		 [o,o,o]]
%enw to ?- place_snake([a,b,c,d,e,f,g],9,3,5,Box). fails. epeidh K*PatternSize <= N*M

place_snake(Pattern,K,N,M,Snake):-
	Row_Size is N,
	Lists_size is M,
	snake_fit(Pattern,K,Row_Size,Lists_size),
	copy_list(Pattern,K,List),
	check_o_loop(List,N,M,O_size_loop),
	%append_o_1(List,O_size_loop,ListO),
	append_o(List,o,O_size_loop,ListO),
	splitlist(ListO,Row_Size,Lists),
	reverse_list(Lists,Snake).

%elenxos an xwraei to fidi kai benei se sleep_mode
snake_fit(Pattern,K,N,M):-
	length(Pattern,PatternSize),
	PatternSize*K =<  N*M.

%Adigrafh tou Pattern K fores
copy_list(_, 0, []).
copy_list(L, K, R) :-
    K > 0,
    K1 is K-1,
    copy_list(L, K1, R2),
    append(L, R2, R).

%elenxos gia posa o theloume 
check_o_loop(List,N,M,O_size_loop):-
	length(List,Lenlist),
	SnakeSize = N*M,
	O_size_loop is SnakeSize-Lenlist.
	%write(O_size_loop).

%htan polu pio eukolo me maplist!
%append_o_1(Xs, N, Ys) :-
%   length(Es, N),
%   maplist(=(o), Es),
%   append(Xs, Es, Ys).

%append ta o
append_o([], CharO, N, List) :-
    fill(CharO, N, List).
append_o([Head| Tail0], CharO, N, [Head| Tail]) :-
    append_o(Tail0, CharO, N, Tail).
fill(_, 0, []) :- !.
fill(CharO, N, [CharO| Tail]) :-
    N > 0,
    M is N - 1,
    fill(CharO, M, Tail).

%xwrizw thn lista afou valame ta o
splitlist([], _, []).
splitlist(L, N, [LsH|LsT]) :-
   length(LsH, N),
   append(LsH, LT, L),
   splitlist(LT, N, LsT).

%reverse tis deuteres lists(even)
reverse_list([],[]).
reverse_list([H|T],[H|T2]):-
	reverse_list2(T,T2).
reverse_list2([],[]).
reverse_list2([H|T],[H1|T2]):-
	reverse(H,H1), 
	reverse_list(T,T2).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Color Map %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%color_map([ adjacent(a,[b,c,d]), adjacent(b,[a,c,e]), adjacent(c,[a,b,d,e,f]), adjacent(d,[a,c,f]), adjacent(e,[b,c,f]), adjacent(f,[c,d,e]) ], [red,green,blue,black], Coloring).
%%Exw valei na emfanizei oles tis dunates luseis, termatizei!

append_list([],[]).
append_list([adjacent(Node1,Node2_list) | Rest], Nodeset) :- 
	append2(Node1, Node2_list, Ns), 
	append_list(Rest,Nodeset2), 
	append(Ns, Nodeset2, Nodeset).

append2(X, [], []).
append2(X, [Y|Ys], [(X,Y)|Z]) :- 
	append2(X,Ys,Z).

adjacent2(Map,Edges):-
	append_list(Map, Edges).

generate([],_,[]).
generate([adjacent(V,_)|Vs], Colors, [color(V,C)|N]):-
	member(C, Colors),
	generate(Vs,Colors,N).

test([],_).
test([(V1,V2)|Rest],Coloring):-
	member(color(V1,C1),Coloring),
	member(color(V2,C2),Coloring),
	not(C1=C2),not(C2=C1),
	%not(conflict(color(V1,C2),[color(V2,C2)|Rest2])),
	test(Rest,Coloring).	

solution(Map, Colors, Coloring):-
	generate(Map, Colors, Coloring), 
	adjacent2(Map, Test_list),
	test(Test_list, Coloring).

color_map(Map,Colors,Coloring):-
	solution(Map,Colors,Coloring).

%map_edge([adjacent(Node1,Node)|Rest],Node1,Node2):-
%	member(Node2,Node).
%map_edge([_|Rest],Node1,Node2) :- 
%	map_edge(Rest, Node1, Node2).

%neighbor(X,Y):-
%	member(X,Y).
%neighbor(X,Y):-
%	member(Y,X).	
%conflict(color(Area1,Color1),[color(Area2,Color2)|_]):-
%	neighbor(Area1,Area2).
%conflict(color(Area,Color),[_|Rest]):-
%	conflict(color(Area,Color),Rest).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Labyrinth %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%solve_maze(5, 9, [1/8, 2/1, 2/2,2/4, 2/5, 3/4, 3/7, 3/9, 4/4, 4/7, 4/8, 4/9, 5/2], 3/2, 2/6, Path).
%%EFOSON theloume OLA TA MONOPATIA svhnoume ,!. (CUT) kai vazoume .(to exw grapsei kai katw)

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
	breadthfirst(NewPaths, To, M, N, Barriers_list,SolutionPath),!.

%%EFOSON theloume OLA TA MONOPATIA svhnoume ,!. (CUT) kai vazoume .

%%%%solution2 Cycle-free Depth-first Search
depthfirst_cyclefree(_, Node, M, N, Barriers_list, []) :-
	goal(Node, To).
depthfirst_cyclefree(Visited, Node, M, N, Barriers_list, [Node|Path]) :- %Adi gia LastNode|Path -> Node|Path
	move_cyclefree(Visited, Node, NextNode, Barriers_list),
	depthfirst_cyclefree([NextNode|Visited],NextNode, M, N, Barriers_list, Path).
