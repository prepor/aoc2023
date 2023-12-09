:- autoload(library(readutil), [read_file_to_string/3]).
:- autoload(library(dcg/basics), [blanks/2, integer/3, blanks_to_nl/2, 
                                  white/2]).
:- autoload(library(lists), [last/2, member/2, sum_list/2]).
example("0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
").


file_input(I) :-
    read_file_to_string("/Users/prepor/Dropbox/lab/aoc2023/files/input9.txt", I, []).

parse(I, Hs) :-
    atom_codes(I, Codes), phrase(p(Hs), Codes).

prediction(H, P) :-
    all_zero(H), !, P = 0.
prediction(H, P) :-
    diff(H, D),
    prediction(D, P2),
    last(H, L),
    P is L + P2.

prediction2(H, P) :-
    all_zero(H), !, P = 0.
prediction2(H, P) :-
    diff(H, D),
    prediction2(D, P2),
    H = [F|_],
    P is F -P2.

all_zero([]).
all_zero([0|Rest]) :- all_zero(Rest).


diff([_],[]).
diff([A|Tail], [D|Diffs]) :-
    Tail = [B|_],
    D is B - A,
    diff(Tail, Diffs).

sol(Hs, X) :-
    findall(N, (member(H, Hs), prediction(H, N)), Ns),
    sum_list(Ns, X).

sol2(Hs, X) :-
    findall(N, (member(H, Hs), prediction2(H, N)), Ns),
    sum_list(Ns, X).


% --- PARSER

p(Histories) -->
    p_histories(Histories).

p_histories([]) --> [].
p_histories([H|Hs]) -->
    p_numbers(H), "\n", p_histories(Hs).

p_numbers([]) --> [].
p_numbers([N]) -->
    integer(N).
p_numbers([N|Numbers]) -->
    integer(N), white, p_numbers(Numbers).

