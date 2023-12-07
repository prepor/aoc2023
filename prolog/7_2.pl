:- autoload(library(readutil), [read_file_to_string/3]).
:- autoload(library(dcg/basics), [string/3, integer/3, blanks/2, 
                                  nonblank/3]).
:- autoload(library(lists), [permutation/2]).
:- autoload(library(sort), [predsort/3]).
example("32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483").


file_input(I) :-
    read_file_to_string("/Users/prepor/Dropbox/lab/aoc2023/files/input7.txt", I, []).

parse(I, Hands) :-
    atom_codes(I, Codes), phrase(p(Hands), Codes).

hand_rank([C,C,C,C,C], 7) :- !.
hand_rank(Hand, 6) :-
    permutation(Hand, [C,C,C,C,_]), !.
hand_rank(Hand, 5) :-
    permutation(Hand, [C,C,C,C2,C2]), !.
hand_rank(Hand, 4) :-
    permutation(Hand, [C,C,C,_,_]), !.
hand_rank(Hand, 3) :-
    permutation(Hand, [C,C,C2,C2,_]), !.
hand_rank(Hand, 2) :-
    permutation(Hand, [C,C,_, _, _]), !.
hand_rank(_, 1).

card_rank(j, 0).
card_rank(2, 1).
card_rank(3, 2).
card_rank(4, 3).
card_rank(5, 4).
card_rank(6, 5).
card_rank(7, 6).
card_rank(8, 7).
card_rank(9, 8).
card_rank(t, 9).
card_rank(q, 11).
card_rank(k, 12).
card_rank(a, 13).

cards_comparator(<, [], []).
cards_comparator(Delta, [C1|H1], [C2|H2]) :-
    card_rank(C1, R1), card_rank(C2, R2), compare(Delta2, R1, R2),
    (Delta2 = '='->
     cards_comparator(Delta, H1, H2)
    ; Delta = Delta2).

hands_comparator_ext(<, _, _, <).
hands_comparator_ext(>, _, _, >).
hands_comparator_ext(=, Hand1, Hand2, Delta) :-
    cards_comparator(Delta, Hand1, Hand2).

joker_is_unbound([], []).
joker_is_unbound([j|Cards], [_|Res]) :- !, joker_is_unbound(Cards, Res).
joker_is_unbound([C|Cards], [C|Res]) :- joker_is_unbound(Cards, Res).

hand_rank_with_joker(H, R) :-
    joker_is_unbound(H, H2),
    hand_rank(H2, R).

hands_comparator(Delta, H1, H2) :-
    hand_rank_with_joker(H1, R1),
    hand_rank_with_joker(H2, R2),
    compare(Delta2, R1, R2),
    hands_comparator_ext(Delta2, H1, H2, Delta).
    
bids_comparator(Delta, bid(H1, _), bid(H2, _)) :-
    hands_comparator(Delta, H1, H2).

sum_bids(L, A) :-
    sum_bids(L, 1, 0, A).
sum_bids([], _, A, A).
sum_bids([bid(_, Bid)|Bids], X, Acc, A) :-
    Acc2 is Acc + Bid * X, X2 is X + 1, sum_bids(Bids, X2, Acc2, A).

part2(I, A) :-
    parse(I, Bids), predsort(bids_comparator, Bids, Sorted),
    sum_bids(Sorted, A).

                    
% --- PARSER
p([]) --> [].
p([Bid|Bids]) -->
    p_bid(Bid),p(Bids),blanks.

p_bid(bid(Hand, Bid)) -->
    blanks, p_cards(Hand), " ", integer(Bid).

p_cards([]) --> [].
p_cards([Card|Cards]) -->
    nonblank(X), {(code_type(X, digit), number_codes(Card, [X])), !;
                  (atom_codes(Card2,[X]), downcase_atom(Card2, Card))},
    p_cards(Cards).
    
