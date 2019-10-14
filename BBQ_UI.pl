% -----------------------------------------------------------------------------------------------------------------
% 								 UI Predicates  								 
% -----------------------------------------------------------------------------------------------------------------


% ---------------------------------------------------------------------------------- 
% Predicate- print_board
% Summary  - prints a the game board in the UI format
% ---------------------------------------------------------------------------------- 
print_board(Board) :-
	print_all_board(Board, 97),!.

print_all_board(b(Size, Rows), LineIndex) :-
	print_border_line_seperator(Size),nl,!,
	print_board_rest(b(Size, Rows), LineIndex),
	print_columns_index_marks(Size),nl.

print_board_rest(b(Size, []), _) :- 
	print_border_line_seperator(Size),nl,!.

print_board_rest(b(Size, [Row|OtherRows]), LineIndex) :-
	Size > 0,
	write('|'),!,
	print_row(Row), write('     <-- '), name(LineIndexChar, [LineIndex]), write(LineIndexChar),nl,
	LineIndex1 is LineIndex + 1,
	(OtherRows = [] ; print_line_seperator(Size),nl),
	print_board_rest(b(Size, OtherRows),LineIndex1).



% ---------------------------------------------------------------------------------- 
% Predicate- print_row
% Summary  - prints a single row of the board in the UI format
% ---------------------------------------------------------------------------------- 
print_row([]) :- !.

print_row([Var|RowRest]) :-
	print_var(Var),
	write('|'),
	print_row(RowRest).

print_var(b) :- 
	write(' B '),!.

print_var(q) :- 
	write(' Q '),!.

print_var(n) :- 
	write('   '),!.

print_var(X/Y) :- 
	write(X),
	write('/'),
	write(Y),!.

print_var(Var) :- 
	write(' '),
	write(Var),
	write(' '),!.

% ---------------------------------------------------------------------------------- 
% Predicate- print_line_seperator
% Summary  - prints ascii chars that separets between board lines 
% ---------------------------------------------------------------------------------- 
print_line_seperator(Size) :- 
	write('|'),!,
	print_line_seperator_rest(Size).

print_line_seperator_rest(0) :- 
	write('-----|'),!.

print_line_seperator_rest(Size) :-
	Size > 0,
	write('---'),!,
	Size1 is Size - 1,
	print_line_seperator_rest(Size1).


% ---------------------------------------------------------------------------------- 
% Predicate- print_border_line_seperator
% Summary  - prints  ascii chars that represents board's border
% ---------------------------------------------------------------------------------- 
print_border_line_seperator(Size) :-
	write('+'),!,
	print_border_line_seperator_rest(Size).

print_border_line_seperator_rest(0) :- 
	write('-----+'),!.

print_border_line_seperator_rest(Size) :-
	Size > 0,
	write('---'),!,
	Size1 is Size - 1,
	print_border_line_seperator_rest(Size1).


% ---------------------------------------------------------------------------------- 
% Predicate- print_columns_index_marks
% Summary  - prints arrows and integer indexes at the bottom of the board 
% ---------------------------------------------------------------------------------- 
print_columns_index_marks(Size) :-
	write(' '),print_n_times(' /\ ', Size), nl,
	print_n_times('  | ',  Size) ,nl,
	print_integer_indexes(1, Size), nl, !.


print_integer_indexes(CurrentIndex, LastIndex) :- 
	CurrentIndex =< LastIndex,
	write('  '), write(CurrentIndex), write(' '),
	CurrentIndex1 is CurrentIndex + 1,
	print_integer_indexes(CurrentIndex1, LastIndex).

print_integer_indexes(Index, Index) :- !.


% ---------------------------------------------------------------------------------- 
% Predicate- print_n_times
% Summary  - print var given N times
% ---------------------------------------------------------------------------------- 
print_n_times(_, 0) :- !.

print_n_times(ToPrint, N) :-
	N > 0,
	write(ToPrint),
	N1 is N - 1,
	print_n_times(ToPrint, N1).


















