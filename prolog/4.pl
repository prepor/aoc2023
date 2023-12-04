:- autoload(library(lists), [member/2, sum_list/2, nth1/3]).
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
    read_file_to_string("/Users/prepor/Dropbox/lab/aoc2023/files/tt.txt", I, []).

input(I) :- file_input(I).
% input(I) :- example1(I).


% -- SOLUTIONS
% Part 1: points_sum(Sum).

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

won_by(Id, WonId) :-
   card(Id, Winning, Numbers),
   matching_numbers(Winning, Numbers, MatchingNumbers),
   won_id(Id, MatchingNumbers, WonId).

all_cards_count(N) :-
    bagof(_, all_cards, B),
    length(B, N).

all_cards() :- all_cards(_).
all_cards(Id) :- card(Id, _, _).
all_cards(Id) :- won_by(Id, WonId), all_cards(WonId).

% all_cards_count(N) :-
%     findall(_, all_cards, L),
%     length(L, N).

% all_cards(Count) :- all_cards(1, Count).
% all_cards(Id, Count) :-
%     card(Id, _, _),
%     won_ids(Id, WonIds),
    
%     NextId is Id + 1,
%     all_cards(NextId, NextCount),
%     Count is 1 + NextCount.
% all_cards(Id) :- won_by(Id, WonId), all_cards(WonId).





% copies_from(Id, WonId) :-
%     card(Id, Winning, Numbers),
%     matching_numbers(Winning, Numbers, MatchingNumbers),
%     won_id(Id, MatchingNumbers, WonId).

% copies_from(Id, WonId) :-
%     copies_from(Id, CopyId),
%     card(CopyId, Winning, Numbers),
%     matching_numbers(Winning, Numbers, MatchingNumbers),
%     won_id(CopyId, MatchingNumbers, WonId).
    

% copy(Id) :-
%     PrevId is Id - 1,
%     card(PrevId, )

% won_by(Id, WonId) :-
%     won_by(WonId, )

% cards_copy(WonId) :-
    % card(Id, Winning, Numbers),
    % matching_numbers(Winning, Numbers, MatchingNumbers),
    % won_id(Id, MatchingNumbers, WonId).

% cards_copy(WonId) :-
%     cards_copy(Id),
%     card(Id, Winning, Numbers),
%     matching_numbers(Winning, Numbers, MatchingNumbers),
%     won_id(Id, MatchingNumbers, WonId).


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

% -- WON CARDS

won_id(PrevId, MatchingCards, WonId) :-
    nth1(I, MatchingCards, _),
    WonId is PrevId + I.

won_ids(Id, WonIds) :-
    card(Id, Winning, Numbers),
    matching_numbers(Winning, Numbers, MatchingNumbers),
    won_ids(Id, MatchingNumbers, WonIds).
won_ids(Id, [], []).
won_ids(Id, [_|MatchingNumbers], [WonId|Acc]) :-
    length(MatchingNumbers, I),
    WonId is I + 2,
    won_ids(Id, MatchingNumbers, Acc).
