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
	


