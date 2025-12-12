:- use_module(library(readutil)).
:- use_module(library(pcre)).
:- use_module(library(yall)).

:- table count_paths/3.

read_and_parse(Filename) :-
  setup_call_cleanup(
        open(Filename, read, Stream),
        read_file(Stream, Lines),
        close(Stream)
   ),
   parse(Lines).

read_file(Stream, []) :-
  at_end_of_stream(Stream), !.
read_file(Stream, [Line|Lines]) :-
    read_line_to_codes(Stream, Codes),
    ( Codes == end_of_file ->  
      Lines = []
    ; atom_codes(Line, Codes),
      read_file(Stream, Lines)
    ).

parse([]).
parse([X|Xs]) :-
  re_matchsub("^(.{3}):\\s*(.*)", X, Dict, []),
  atom_string(Head, Dict.1),
  split_string(Dict.2, " ", " ", BodyStrList),
  maplist(atom_string, Bodies, BodyStrList),
  forall(member(Body, Bodies), assertz((Head :- Body))),
  parse(Xs).

out.

main(Filename) :-
  retractall(_ :- _),
  read_and_parse(Filename),
  solve1(Count1),
  solve2(Count2),
  format('Part 1: ~w~n', [Count1]),
  format('Part 2: ~w~n', [Count2]).

solve1(Count) :-
  count_paths(you, out, Count).

solve2(Count) :-
    count_paths(svr, dac, N1),
    count_paths(dac, fft, N2),
    count_paths(fft, out, N3),
    TotalA is N1 * N2 * N3,
    count_paths(svr, fft, M1),
    count_paths(fft, dac, M2),
    count_paths(dac, out, M3),
    TotalB is M1 * M2 * M3,
    Count is TotalA + TotalB.

count_paths(Target, Target, 1) :- !.
count_paths(Current, Target, Count) :-
    findall(N, (
        clause(Current, Next), 
        Next \== true,
        count_paths(Next, Target, N)
    ), NeighborCounts),
    sum_list(NeighborCounts, Count).
