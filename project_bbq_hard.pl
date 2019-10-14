

% -----------------------------------------------------------------------------------------------------------------
% 								 Tests & Module documentation 								 
% -----------------------------------------------------------------------------------------------------------------


% ---- TESTS ------
% NumsBoard = b(6,[[1,2,3,1,2,9],[4,5,6,4,5,9],[7,8,9,7,8,9],[7,8,9,7,8,9],[7,8,9,7,8,9],[7,8,9,7,8,9]]), print_board(NumsBoard).
% EmptyBoard = b(6,[[n,n,n,n,n,n],[n,n,n,n,n,n],[n,n,n,n,n,n],[n,n,n,n,n,n],[n,n,n,n,n,n],[n,n,n,n,n,n]]), print_board(EmptyBoard).

% IndexBoard = b(6,[[1/1,1/2,1/3,1/4,1/5,1/6],[2/1,2/2,2/3,2/4,2/5,2/6],[3/1,3/2,3/3,3/4,3/5,3/6],[4/1,4/2,4/3,4/4,4/5,4/6],[5/1,5/2,5/3,5/4,5/5,5/6],[6/1,6/2,6/3,6/4,6/5,6/6]]), print_board(IndexBoard).
% ExampleBoard = b(6,[[b,b,n,b,n,n],[b,n,n,b,n,n],[b,n,n,b,n,n],[n,n,n,n,q,q],[q,n,n,b,n,q],[n,n,q,n,n,q]]), print_board(ExampleBoard).

% ExampleBoard = b(6,[[n,n,n,b,n,n],[n,n,n,b,n,n],[b,n,n,n,n,n],[n,n,n,n,q,q],[q,n,n,b,n,n],[n,n,q,n,n,n]]),
% ExampleBoard = b(6,[[b,b,n,b,n,n],[b,n,n,b,n,n],[b,n,n,b,n,n],[n,n,n,n,q,q],[q,n,n,b,n,q],[n,n,q,n,n,q]]),
% ExampleBoard = b(6,[[b,b,q,b,n,n],[b,n,n,b,q,q],[b,b,b,b,n,n],[n,n,n,n,q,q],[q,n,q,b,q,q],[n,q,q,n,n,q]]),
% ExampleBoard = b(6,[[b,b,q,b,n,n],[b,b,b,n,q,q],[b,q,q,q,b,n],[n,n,n,n,q,q],[b,b,b,b,q,q],[n,q,q,b,b,q]]),




/*
ExampleBoard = b(6,[[b,b,q,b,n,n],[b,b,b,n,q,q],[b,q,q,q,b,n],[n,n,n,n,q,q],[b,b,b,b,q,q],[n,q,q,b,b,q]]),
ExampleBoard = b(_, Rows),
print_board(ExampleBoard),
get_board_columns(ExampleBoard, Cols),

max_line(Rows, b, B_MaxRow, B_Max_Row_Count),
max_line(Rows, q, Q_MaxRow, Q_Max_Row_Count),

max_line(Cols, b, B_MaxCol, B_Max_Col_Count),
max_line(Cols, q, Q_MaxCol, Q_Max_Col_Count).

*/


test(NewBoard) :-
	ExampleBoard = b(6,[[n,n,n,b,n,n],[n,n,n,b,n,n],[b,n,n,n,n,n],[n,n,n,n,q,q],[q,n,n,b,n,n],[n,n,q,n,n,n]]),
	print_board(ExampleBoard),nl,nl,nl,
	place_in_board(ExampleBoard, b, NewBoard), print_board(NewBoard).

% ------------------------
% Board Format :
% b(Size, Rows)
% Size = 6,
% Rows = [x1,x2..x5],[y1,y2..y5], ...[z1,z2..z5]
%
% Row var : 
% b / q / n 
% b = B
% q = Q
% n = '_'
% ------------------------

% -----------------------------------------------------------------------------------------------------------------
% 							 Generic Auxilliary Predicates 								 
% -----------------------------------------------------------------------------------------------------------------


max(A, B, A) :- 
	A > B,!.

max(A, B, B) :- 
	B >= A,!.


% -----------------------------------------------------------------------------------------------------------------
% 								 UI Predicates  								 
% -----------------------------------------------------------------------------------------------------------------


% ---------------------------------------------------------------------------------- 
% Predicate- print_board
% Summary  - prints a the game board in the UI format
% ---------------------------------------------------------------------------------- 
print_board(Board) :-
	print_all_board(Board),!.

print_all_board(b(Size, Rows)) :-
	print_border_line_seperator(Size),nl,!,
	print_board_rest(b(Size, Rows)).

print_board_rest(b(Size, [])) :- 
	print_border_line_seperator(Size),nl,!.

print_board_rest(b(Size, [Row|OtherRows])) :-
	Size > 0,
	write('|'),!,
	print_row(Row),nl,
	(OtherRows = [] ; print_line_seperator(Size),nl),
	print_board_rest(b(Size, OtherRows)).



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
	write(Var),!.

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






% -----------------------------------------------------------------------------------------------------------------
% 								 Board Manipulating Predicates  						 
% -----------------------------------------------------------------------------------------------------------------



% ---------------------------------------------------------------------------------- 
% Predicate- get_board_columns
% Summary  - gets a list of lists that holds the board columns
% ---------------------------------------------------------------------------------- 

get_board_columns([[]|_], []) :- !.

get_board_columns(b(Size, Rows), [C|Columns]) :-
	get_column(Rows, NewRows, C),
	get_board_columns(NewRows, Columns),!.

get_board_columns(Rows, [C|Columns]) :-
	get_column(Rows, NewRows, C),
	get_board_columns(NewRows, Columns),!.

get_column([],[],[]) :- !.

get_column([[Var|RowRest]|OtherRows],[RowRest|NewRows], [Var|ColRest]) :-
	get_column(OtherRows, NewRows, ColRest). 	
	



% ---------------------------------------------------------------------------------- 
% Predicate- max_line
% Summary  - Find the line (row / column represented as a list) with the most 
% 		 continuous instances of the given Sign
% ---------------------------------------------------------------------------------- 

max_line(Rows, Var, MaxRow, MaxCount) :-
	max_line(Rows, Var, _, 0, MaxRow, MaxCount),!.

max_line([], _ , MaxRow, MaxCount, MaxRow, MaxCount) :- !.

max_line([Row|OtherRows], Var , CurrentMaxRow, CurrentMaxCount, MaxRow, MaxCount) :-
	max_continuous_vars_in_row(Row, Var, Count),
	(
	Count > CurrentMaxCount,!, 
	max_line(OtherRows, Var, Row, Count, MaxRow, MaxCount)
	;
	max_line(OtherRows, Var, CurrentMaxRow, CurrentMaxCount, MaxRow, MaxCount)
	).



% ---------------------------------------------------------------------------------- 
% Predicate- max_continuous_vars_in_row
% Summary  - Find the maximal continuous insances of a given var in a row
% ---------------------------------------------------------------------------------- 

max_continuous_vars_in_row([V1|RowRest], Var, MaxCount) :- 
	(
	V1 = Var,!, max_continuous_vars_in_row(RowRest, Var, V1, 1, 0, MaxCount)
	;
	max_continuous_vars_in_row(RowRest, Var, V1, 0, 0, MaxCount),!
	).
	

max_continuous_vars_in_row([], _ , _, CurrentCount, CurrentMaxCount, MaxCount) :- 
	(CurrentMaxCount > CurrentCount,!, 
	MaxCount = CurrentMaxCount
	;
	MaxCount = CurrentCount),!.
	

max_continuous_vars_in_row([V|RowRest], Var, LastVar, CurrentCount, CurrentMaxCount, MaxCount) :- 
	V \= Var,!,
	(CurrentCount > CurrentMaxCount,!,
	max_continuous_vars_in_row(RowRest, Var, V, 0 , CurrentCount, MaxCount)
	;
	CurrentCount =< CurrentMaxCount,!,
	max_continuous_vars_in_row(RowRest, Var, V, 0 , CurrentMaxCount, MaxCount)
	).

max_continuous_vars_in_row([V|RowRest], Var, LastVar, CurrentCount, CurrentMaxCount, MaxCount) :- 
	V = LastVar,!,
	V = Var,
	CurrentCount1 is CurrentCount + 1,
	max_continuous_vars_in_row(RowRest, Var, V, CurrentCount1, CurrentMaxCount, MaxCount).


max_continuous_vars_in_row([V|RowRest], Var, LastVar, CurrentCount, CurrentMaxCount, MaxCount) :- 
	V \= LastVar,!,
	V = Var,
	(CurrentCount > CurrentMaxCount,!,
	max_continuous_vars_in_row(RowRest, Var, V, 1 , CurrentCount, MaxCount)
	;
	CurrentCount =< CurrentMaxCount,!,
	max_continuous_vars_in_row(RowRest, Var, V, 1 , CurrentMaxCount, MaxCount)
	).









% ---------------------------------------------------------------------------------- 
% Predicate- 
% Summary  - 
% ---------------------------------------------------------------------------------- 
place_in_board(Board, Sign, NewBoard) :-
	(Sign = q ; Sign = b),
	Board = b(Size, Rows),

	% Take a row from the board, replace a blank spot with the given sign
	list_vars(Rows, Before_Row, Row, After_Row),
	replace_in_line(Row, _, Sign, NewRow),

	% Assemble the new board with the new row inplaced
	append(Before_Row, [NewRow], Temp),
	append(Temp, After_Row, NewBoardRows),
	NewBoard = b(Size, NewBoardRows).



% ---------------------------------------------------------------------------------- 
% Predicate- replace_in_line
% Summary  - Place given Sign in all the avialable places in the line
% ---------------------------------------------------------------------------------- 
replace_in_line([n,B,C,D,E,F], 1, VarToInplace, [VarToInplace,B,C,D,E,F]).
replace_in_line([A,n,C,D,E,F], 2, VarToInplace, [A,VarToInplace,C,D,E,F]).
replace_in_line([A,B,n,D,E,F], 3, VarToInplace, [A,B,VarToInplace,D,E,F]).
replace_in_line([A,B,C,n,E,F], 4, VarToInplace, [A,B,C,VarToInplace,E,F]).
replace_in_line([A,B,C,D,n,F], 5, VarToInplace, [A,B,C,D,VarToInplace,F]).
replace_in_line([A,B,C,D,E,n], 6, VarToInplace, [A,B,C,D,E,VarToInplace]).



% ---------------------------------------------------------------------------------- 
% Predicate- list_vars
% Summary  - This predicate iterates through the variables of a list in backtraking,
%  		 Each iteration returns a different var of the list.	
%		 It does that using append(conc) and finding sublists of the given list.
% ---------------------------------------------------------------------------------- 
list_vars(List, BeforeVar, Var, AfterVar) :-

	% Find sublist of length 1
	append(P1, P2, List),
	append(V1, V2, P1),	
	length(V2, 1),
	V2 = [Var],
	BeforeVar = V1,
	AfterVar = P2.






	
	



% -----------------------------------------------------------------------------------------------------------------
% 								 Game Predicates  								 
% -----------------------------------------------------------------------------------------------------------------

% ---------------------------
% Game State Format :
% s(Player, Board)
%
% Player = player_b / player_q
% Board = b(Size, Rows)
% ---------------------------




% ---- TESTS ------

/*
ExampleBoard = b(6,[[b,b,q,b,n,n],[b,b,b,n,q,q],[b,q,q,q,b,n],[n,n,n,n,q,q],[b,b,b,b,q,q],[n,q,q,b,b,q]]),
*/



 


% ---------------------------------------------------------------------------------- 
% Predicate- 
% Summary  - 
% ---------------------------------------------------------------------------------- 
















