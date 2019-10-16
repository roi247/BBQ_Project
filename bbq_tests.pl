cl :- compile_all,!.

compile_all :-
	compile(bbq_aux),
	compile(bbq_board),

	compile(bbq_ui),
	compile(bbq_gameplay_hard).
	

% ---- TESTS ------
% NumsBoard = b(6,[[1,2,3,1,2,9],[4,5,6,4,5,9],[7,8,9,7,8,9],[7,8,9,7,8,9],[7,8,9,7,8,9],[7,8,9,7,8,9]]), print_board(NumsBoard).
% EmptyBoard = b(6,[[n,n,n,n,n,n],[n,n,n,n,n,n],[n,n,n,n,n,n],[n,n,n,n,n,n],[n,n,n,n,n,n],[n,n,n,n,n,n]]), print_board(EmptyBoard).

% Board = b(6,[[b,b,q,n,n,n],[b,n,n,n,b,n],[b,n,n,n,n,n],[b,n,n,n,n,q],[n,n,q,q,q,q],[n,n,n,n,n,n]]), print_board(Board).

% IndexBoard = b(6,[[1/1,1/2,1/3,1/4,1/5,1/6],[2/1,2/2,2/3,2/4,2/5,2/6],[3/1,3/2,3/3,3/4,3/5,3/6],[4/1,4/2,4/3,4/4,4/5,4/6],[5/1,5/2,5/3,5/4,5/5,5/6],[6/1,6/2,6/3,6/4,6/5,6/6]]), print_board(IndexBoard).
% IndexBoard = b(6,[[a/1,1/2,1/3,1/4,1/5,1/6],[a/1,2/2,2/3,2/4,2/5,2/6],[a/1,3/2,3/3,3/4,3/5,3/6],[a/1,4/2,4/3,4/4,4/5,4/6],[a/1,5/2,5/3,5/4,5/5,5/6],[a/1,6/2,6/3,6/4,6/5,6/6]]), print_board(IndexBoard).
% 	

% ExampleBoard = b(6,[[n,n,n,b,n,n],[n,n,n,b,n,n],[b,n,n,n,n,n],[n,n,n,n,q,q],[q,n,n,b,n,n],[n,n,q,n,n,n]]),
% ExampleBoard = b(6,[[b,b,n,b,n,n],[b,n,n,b,n,n],[b,n,n,b,n,n],[n,n,n,n,q,q],[q,n,n,b,n,q],[n,n,q,n,n,q]]),
% ExampleBoard = b(6,[[b,b,q,b,n,n],[b,n,n,b,q,q],[b,b,b,b,n,n],[n,n,n,n,q,q],[q,n,q,b,q,q],[n,q,q,n,n,q]]),
% ExampleBoard = b(6,[[b,b,q,b,n,n],[b,b,b,n,q,q],[b,q,q,q,b,n],[n,n,n,n,q,q],[b,b,b,b,q,q],[n,q,q,b,b,q]]),


/*

ExampleBoard = b(6,[[b,b,q,b,n,n],[b,b,b,n,q,q],[b,q,q,q,b,n],[n,n,n,n,q,q],[b,b,b,b,q,q],[n,q,q,b,b,q]]),
board_longest_line(ExampleBoard, b, LL, C), print_board(ExampleBoard).

*/


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







/*

Board = b(6,[[b,b,q,n,n,n],[b,n,n,n,b,n],[b,n,n,n,n,n],[b,n,n,n,n,q],[n,n,q,q,q,q],[n,n,n,n,n,n]]), print_board(Board),
staticval(s(player_q, 1, Board), Val).


Board = b(6,[[b,b,q,n,n,n],[b,n,n,n,b,n],[b,n,n,n,n,n],[b,n,n,n,n,q],[q,n,n,q,q,q],[n,n,n,n,n,n]]), print_board(Board),
staticval(s(player_q, 1, Board), Val).

*/





test1(NewBoard) :-
	ExampleBoard = b(6,[[b,b,q,b,n,n],[b,b,b,n,q,q],[b,q,q,q,b,n],[n,n,n,n,q,q],[b,b,b,b,q,q],[n,q,q,b,b,q]]),
	print_board(ExampleBoard),nl,nl,nl,
	place_in_board(ExampleBoard, b, NewBoard), print_board(NewBoard).

test2 :-
	ExampleBoard = b(6,[[b,b,n,b,n,n],[b,n,n,b,n,n],[b,n,n,b,n,n],[n,n,n,n,q,q],[q,n,n,b,n,q],[n,n,q,n,n,q]]),
	print_board(ExampleBoard).


test3(Sign, Index, NB) :-
	ExampleBoard = b(6,[[b,b,q,b,n,n],[b,b,b,n,q,q],[b,q,q,q,b,n],[n,n,n,n,q,q],[b,b,b,b,q,q],[n,q,q,b,b,q]]),
	print_board(ExampleBoard),nl,nl,
	place_in_board_index(ExampleBoard, Sign, Index, NB), print_board(NB).


test4(Count) :-
	%ExampleBoard = b(6,[[b,b,q,b,n,n],[b,b,b,n,q,q],[b,q,q,q,b,n],[n,n,n,n,q,q],[b,b,b,b,q,q],[n,q,q,b,b,q]]),
	ExampleBoard = b(6,[[n,n,n,b,n,n],[n,n,n,b,n,n],[b,n,n,n,n,n],[n,n,n,n,q,q],[q,n,n,b,n,n],[n,n,q,n,n,n]]),
	print_board(ExampleBoard),
	board_empty_spots_left(ExampleBoard, Count).


test5(Row, Count) :-
	row_potential_spots_left(Row,b,Count).














