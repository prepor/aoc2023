:- autoload(library(dcg/basics), [integer/3, blanks/2, string/3, 
                                  whites/2]).
:- autoload(library(readutil), [read_file_to_string/3]).
:- autoload(library(lists), [member/2, min_list/2]).

example("seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4").

file_input(I) :-
    read_file_to_string("/Users/prepor/Dropbox/lab/aoc2023/files/input5.txt", I, []).

parse(I) :-
    atom_codes(I, Codes), phrase(p(Seeds, Maps), Codes),
    assert_data(Seeds, Maps).

:- dynamic seeds/1.
:- dynamic m/3.
assert_data(Seeds, []) :-
    assertz(seeds(Seeds)).

assert_data(Seeds, [M|Maps]) :-
    assertz(M), assert_data(Seeds, Maps).

forget_data() :-
    retractall(seeds(_)), retractall(m(_, _, _)).

% --- MAPPING

map(Source, Dest, SourceNumber, DestNumber) :-
    m(Source, Dest, Ranges),
    member(range(DestR, SourceR, RangeL), Ranges),
    SourceREnd is SourceR + RangeL,
    SourceNumber >= SourceR, SourceNumber < SourceREnd,
    DestNumber is DestR + (SourceNumber - SourceR),
    !.

map(Source, Dest, SourceNumber, SourceNumber) :-
    m(Source, Dest, _), !.

map(Source, Dest, SourceNumber, DestNumber) :-
    map(Source, Dest2, SourceNumber, DestNumber2),
    map(Dest2, Dest, DestNumber2, DestNumber).

% -- SOLUTIONS

part1(A) :-
    findall(L, (seeds(Seeds),
                   member(Seed, Seeds),
                   map("seed", "location", Seed, L)),
            Locs),
    min_list(Locs, A).


% --- PARSER
p(Seeds, Maps) -->
    p_seeds(Seeds), "\n", !, p_maps(Maps), blanks.

p(Seeds, Maps) -->
    p_seeds(Seeds), "\n", !, p_maps(Maps), blanks.

p_numbers([]) --> [].
p_numbers([N|Rs]) --> blanks,integer(N),p_numbers(Rs).

p_seeds(Seeds) -->
    "seeds:", blanks, p_numbers(Seeds).

p_ranges([]) --> [].
p_ranges([range(Dest, Source, Len)|Ranges]) -->
    blanks,
    integer(Dest), blanks,
    integer(Source), blanks,
    integer(Len), !,
    p_ranges(Ranges).

p_map(m(From, To, Ranges)) -->
    blanks, alphas(FromC), "-to-", alphas(ToC), " map:", !,
    { string_codes(From, FromC), string_codes(To, ToC) }, 
    p_ranges(Ranges).

p_maps([]) --> [].
p_maps([M|Maps]) -->
    p_map(M), p_maps(Maps).
    

alphas([H|T]) -->
	alpha(H), !,
	alphas(T).
alphas([]) -->
	[].

alpha(A) -->
	[A],
	{ code_type(A, alpha)
	}.
