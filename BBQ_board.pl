% -----------------------------------------------------------------------------------------------------------------
% 								  Module documentation 								 
% -----------------------------------------------------------------------------------------------------------------

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
% 								 Board Handling Predicates  						 
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
% Predicate- place_in_board
% Summary  - Places the given sign in a free spot in the board
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
% Predicate- place_in_board_index
% Summary  - Places the given sign in the given index column/row
% ---------------------------------------------------------------------------------- 
place_in_board_index(Board, Sign, Index, NewBoard) :-

	% Validate Board, Sign, Index
	Board = b(Size, Rows),
	(Sign = q ; Sign = b),
	
	Index = R/C,
	(integer(R), atom(C),!,
	name(C, [CAscii]),
	RowIndex is CAscii - 96,
	ColIndex is R
	;
	integer(C), atom(R),!,
	name(R, [RAscii]),
	RowIndex is RAscii - 96,
	ColIndex is C
	),

	RowIndex > 0, RowIndex =< Size,
	ColIndex > 0, ColIndex =< Size,

	% Take the matching row given in the index
	% Replace the empty spot with the given var
	RowIndex1 is RowIndex -1,
	random_access_list(Rows, RowIndex1 , BerforeRow, Row , AfterRow),
	replace_in_line(Row, ColIndex, Sign, NewRow),

	% Assemble the new board with the new row inplaced
	append(BerforeRow, [NewRow], Temp),
	append(Temp, AfterRow, NewBoardRows),
	NewBoard = b(Size, NewBoardRows),!.
	
	

% ---------------------------------------------------------------------------------- 
% Predicate- replace_in_line
% Summary  - Place given Sign (b/q) in all the avialable spots in the line
% ---------------------------------------------------------------------------------- 
replace_in_line([n,B,C,D,E,F], 1, VarToInplace, [VarToInplace,B,C,D,E,F]).
replace_in_line([A,n,C,D,E,F], 2, VarToInplace, [A,VarToInplace,C,D,E,F]).
replace_in_line([A,B,n,D,E,F], 3, VarToInplace, [A,B,VarToInplace,D,E,F]).
replace_in_line([A,B,C,n,E,F], 4, VarToInplace, [A,B,C,VarToInplace,E,F]).
replace_in_line([A,B,C,D,n,F], 5, VarToInplace, [A,B,C,D,VarToInplace,F]).
replace_in_line([A,B,C,D,E,n], 6, VarToInplace, [A,B,C,D,E,VarToInplace]).

	

% ---------------------------------------------------------------------------------- 
% Predicate- board_empty_spots_left
% Summary  - Counts how many empty spots are left in the board
% ---------------------------------------------------------------------------------- 
board_empty_spots_left(b(_, []), 0) :- !.

board_empty_spots_left(b(_, [Row|Rows]), Count) :-
	board_empty_spots_left(b(_, Rows), Count0),
	findall(V, (member(V,Row), V = n) , NArray), len(NArray, RowCount),
	Count is Count0 + RowCount.

	

	



























