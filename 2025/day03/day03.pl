:- use_module(library(apply)).
:- use_module(library(readutil)).
:- use_module(library(yall)).

main(Filename) :- 
  setup_call_cleanup(
        open(Filename, read, Stream),
        read_file(Stream, Lines),
        close(Stream)
   ),
  parse(Lines, Batteries),
  foldl(joltage(2), Batteries, 0, Score1),
  foldl(joltage(12), Batteries, 0, Score2),
  format('Part 1: ~w~n', [Score1]),
  format('Part 2: ~w~n', [Score2]).

read_file(Stream, []) :-
  at_end_of_stream(Stream), !.
read_file(Stream, [Line|Lines]) :-
    read_line_to_codes(Stream, Codes),
    ( Codes == end_of_file ->  
      Lines = []
    ; atom_codes(Line, Codes),
      read_file(Stream, Lines)
    ).

string_to_digits(String, RowTerm) :-
  string_chars(String, Chars),
  maplist(atom_number, Chars, Digits),
  RowTerm =.. [row|Digits].

digits_to_number(Digits, Number) :-
    foldl([Digit,Acc,Res]>>(Res is Acc*10 + Digit), Digits, 0, Number).

parse([], []).
parse([X|Xs], [Y|Ys]) :-
  string_to_digits(X, Y),
  parse(Xs, Ys).

%! joltage(+Battery, +Size, -Value) is det.
%  
%  Battery is an array of integer from 0 to 9
%  True if Value is the joltage of size Size of Battery.
joltage(Size, Battery, Acc, Res) :-
  % format("joltage(~w, ~w, ~w, ~w)~n",[Size,Battery, Acc, Res]),
  functor(Battery, _, Len),
  End is Len - Size + 1,
  joltage(Size, Battery, 1, End, Digits),
  digits_to_number(Digits, N),
  Res is Acc + N.

joltage(0, _, _, _, []) :- !.
joltage(N, Battery, Start, End, [MaxVal|Res]) :-
  max(Battery, Start, End, MaxVal, MaxIdx),
  N1 is N - 1,
  Next is MaxIdx + 1,
  End1 is End + 1,
  joltage(N1, Battery, Next, End1, Res).

%! max(List, Start, End, N, Idx) is det
%
%  find the max value of List between indexes Start and End (inclusive) and return its value and index
%  Array is of the form row(r1,r2,...,rn)
max(Array, Start, End, MaxVal, MaxIdx):-
  arg(Start, Array, CurrentMax),
  Next is Start + 1,
  scan_range(Array, Next, End, CurrentMax, Start, MaxVal, MaxIdx).

scan_range(_Array, Current, End, CurrentVal, CurrentIdx, CurrentVal, CurrentIdx) :-
  Current > End.
scan_range(Array, Current, End, CurrentVal, CurrentIdx, MaxVal, MaxIdx) :-
  Current =< End,
  arg(Current, Array, Val),
  ( Val > CurrentVal -> 
    NewMaxVal = Val,
    NewMaxIdx = Current
  ; NewMaxVal = CurrentVal,
    NewMaxIdx = CurrentIdx
  ),
  Next is Current + 1,
  scan_range(Array, Next, End, NewMaxVal, NewMaxIdx, MaxVal, MaxIdx).
