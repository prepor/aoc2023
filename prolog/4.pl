:- autoload(library(lists), [member/2, sum_list/2]).
:- autoload(library(dcg/basics), [integer/3, blanks/2]).
:- autoload(library(readutil), [read_file_to_string/3]).

% -- INPUT
example1('Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11').

file_input(I) :-
    read_file_to_string("/Users/prepor/Dropbox/lab/aoc2023/files/input4.txt", I, []).

input(I) :- file_input(I).

% -- API

card(Id, Winning, Numbers) :-
    card_line(L), atom_codes(L, Codes), phrase(p_card(Id, Winning, Numbers), Codes).

points(card(Id, Winning, Numbers), Points) :-
    card(Id, Winning, Numbers),
    matching_numbers(Winning, Numbers, MatchingNumbers),
    calc_points(MatchingNumbers, Points).

points_sum(Sum) :-
    findall(Points, points(_, Points), L),
    sum_list(L, Sum).

% -- PARSER

split_string_to_lines(String, Lines) :-
    split_string(String, "\n", "\n", Lines).

card_line(L) :-
    input(I),
    split_string_to_lines(I, Lines),
    member(L, Lines).

p_numbers([]) --> [].
p_numbers([N|Numbers]) -->
    blanks, integer(N), !, p_numbers(Numbers).

p_card(Id, Winning, Numbers) -->
    "Card", blanks, integer(Id), ":",
    blanks, p_numbers(Winning), blanks,
    "|",
    blanks, p_numbers(Numbers), blanks.

% -- CALC POINTS

matching_numbers(_, [], []).
matching_numbers(Winning, [N|Numbers], [N|MatchingNumbers]) :-
    member(N, Winning), matching_numbers(Winning, Numbers, MatchingNumbers).
matching_numbers(Winning, [N|Numbers], MatchingNumbers) :-
    \+ member(N, Winning), matching_numbers(Winning, Numbers, MatchingNumbers).


calc_points([], 0).
calc_points([_], 1) :- !.
calc_points([_|Tail], Points) :-
    calc_points(Tail, PrevPoints), Points is PrevPoints * 2.
