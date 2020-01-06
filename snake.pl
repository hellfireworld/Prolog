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
	