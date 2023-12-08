:- autoload(library(readutil), [read_file_to_string/3]).
:- autoload(library(dcg/basics), [blanks/2]).
:- autoload(library(lists), [member/2]).

example("LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)").

file_input(I) :-
    read_file_to_string("/Users/prepor/Dropbox/lab/aoc2023/files/input8.txt", I, []).

parse(I, Steps, Map) :-
    atom_codes(I, Codes), phrase(p(Steps, Map), Codes).

init_a([], []).
init_a([((Prefix-"A")-_-_)|Map], [Prefix-"A"|Res]) :- !, init_a(Map, Res).
init_a([_|Map], Res) :- init_a(Map, Res).

sol(Map, Steps, Res) :-
    findall(C, (
            init_a(Map, As),
            member(N, As),
            sol(Map, Steps, C, 0, N, Steps)),
            [C1|Counters]),
    lcm_of(Counters, C1, Res).
sol(_, _, Counter, Counter, _-"Z", _) :-  !.
sol(Map, Steps, Counter, Acc, CurrentNode, []) :-
    sol(Map, Steps, Counter, Acc, CurrentNode, Steps), !.
sol(Map, Steps, Counter, Acc, CurrentNode, [Next|Steps1]) :-
    next_step(Map, CurrentNode, Next, Next2),
    Acc2 is Acc + 1,
    sol(Map, Steps, Counter, Acc2, Next2, Steps1).

next_step(Map, CurrentNode, l, Next) :-
    member(CurrentNode-Next-_, Map).
next_step(Map, CurrentNode, r, Next) :-
    member(CurrentNode-_-Next, Map).

lcm_of([], R, R).
lcm_of([N|Numbers], Acc, Res) :-
    Acc2 is lcm(N, Acc),
    lcm_of(Numbers, Acc2, Res).


% sol(Map, Steps, Counter, Acc, CurrentNodes, [S|Steps1]) :-
%     findall(Next, (member(CurrentNode, CurrentNodes),
%                    next_step(Map, CurrentNode, S, Next)),
%            CurrentNodes2),
%     Acc2 is Acc + 1,
%     sol(Map, Steps, Counter, Acc2, CurrentNodes2, Steps1).

% next_step(Map, CurrentNode, l, Next) :-
%     member(CurrentNode-Next-_, Map).
% next_step(Map, CurrentNode, r, Next) :-
%     member(CurrentNode-_-Next, Map).





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
    blanks, p_id(Id), " = ",
    "(", p_id(L), ", ", p_id(R), ")",    
    p_nodes(Nodes).

p_id(Prefix-Ending) -->
    alpha(Ch1), alpha(Ch2), alpha(Ch3),
    {string_codes(Prefix, [Ch1, Ch2]), string_codes(Ending, [Ch3])}.

alpha(A) -->
	[A],
	{ code_type(A, alpha);code_type(A, digit)
	}.

