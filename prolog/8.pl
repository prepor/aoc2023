:- autoload(library(readutil), [read_file_to_string/3]).
:- autoload(library(dcg/basics), [blanks/2]).
:- autoload(library(lists), [member/2]).

example("RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)").

file_input(I) :-
    read_file_to_string("/Users/prepor/Dropbox/lab/aoc2023/files/input8.txt", I, []).

parse(I, LRs, Nodes) :-
    atom_codes(I, Codes), phrase(p(LRs, Nodes), Codes).

sol(Map, Steps, Counter) :-
  sol(Map, Steps, Counter, 0, "AAA", Steps).
sol(_, _, Counter, Counter, "ZZZ", _) :- !.
sol(Map, Steps, Counter, Acc, CurrentNode, []) :-
    sol(Map, Steps, Counter, Acc, CurrentNode, Steps), !.
sol(Map, Steps, Counter, Acc, CurrentNode, [l|Steps1]) :-
    member(CurrentNode-Next-_, Map),
    Acc2 is Acc + 1,
    sol(Map, Steps, Counter, Acc2, Next, Steps1).
sol(Map, Steps, Counter, Acc, CurrentNode, [r|Steps1]) :-
    member(CurrentNode-_-Next, Map),
    Acc2 is Acc + 1,
    sol(Map, Steps, Counter, Acc2, Next, Steps1).



% --- PARSER

p(LRs, Nodes) -->
    p_lrs(LRs), p_nodes(Nodes), blanks.


p_lrs([]) --> [].
p_lrs([l|Lrs]) -->
    "L", p_lrs(Lrs).
p_lrs([r|Lrs]) -->
    "R", p_lrs(Lrs).

p_nodes([]) --> [].
p_nodes([Id-L-R|Nodes]) -->
    blanks, alphas(IdC), " = ",
    "(", alphas(LC), ", ", alphas(RC), ")",
    {string_codes(Id, IdC), string_codes(L, LC), string_codes(R, RC)},
    p_nodes(Nodes).

alphas([H|T]) -->
	alpha(H), !,
	alphas(T).
alphas([]) -->
	[].

alpha(A) -->
	[A],
	{ code_type(A, alpha)
	}.

