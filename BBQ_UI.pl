% -----------------------------------------------------------------------------------------------------------------
% 								 UI Predicates  								 
% -----------------------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------- 
% Predicate- compile_all
% Summary  - compiles all the neccesery files for the game
% ---------------------------------------------------------------------------------- 
cl :- compile_all,!.

compile_all :-
	compile(bbq_aux),
	compile(bbq_tests),
	compile(bbq_board),
	compile(bbq_gameplay_hard).


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




% -----------------------------------------------------------------------------------------------------------------
% 								 Gameplay UI Predicates  							 
% -----------------------------------------------------------------------------------------------------------------




start_game :-
	EmptyBoard = b(6,[[n,n,n,n,n,n],[n,n,n,n,n,n],[n,n,n,n,n,n],[n,n,n,n,n,n],[n,n,n,n,n,n],[n,n,n,n,n,n]]),
	game(EmptyBoard),!.


game(CurrentBoard) :-
	
	% Print current board
	write('Board: '),nl,nl,
	print_board(CurrentBoard),
	print_ui_separetor,

	% Print instructions to player
	write('Please Enter where you would like to place your Q'),nl,
	write('For any other instructions, please enter the relevent command. type: "help" For help'),nl,

	
	
	% Read the user next instruction
	% If special instruction - handle accordingly 
	% otherwise-  if valid play - set the new board accordingly 
	read(Instruction),

	(handle_command(Instruction, CurrentBoard),!,
	 game(CurrentBoard)
	 ;
	 % TODO - HANDLE INVALID INPUT !!!!@@@@@@@@@@@@@@ 
	 place_in_board_index(CurrentBoard, q , Instruction, NewBoard)
	 ),
 	print_board(NewBoard),
	print_ui_separetor,
	nl,nl,


	% Computer play , use alpha-beta algorithem to pick the best play,
	% Print this play and start another loop..
	write('Computer Turn ! ...'),nl,nl,
	cumputer_thinking_animation,

	Pos = s(player_b, 0, NewBoard),
	alphabeta(Pos, -999, 999, GoodPos, Value),

	%write('DEBUG ~~~~~~~~~~~~~~~~ alpha-beta value is: ~~~~~~~~~~~~~~~~ '),
	%write(Value),nl,
	
	GoodPos = s(_,_, UpdatedBoard),
	game(UpdatedBoard),!.
		

handle_command(help, _) :-
	nl,write('###########################################################################'),nl,
	write('#		 			Game Help Menu 					  #'),nl,
	write('#                                                                         #'),nl,
	write('# 1. For Help, Enter - help.								  #'),nl,
	write('# 2. To  Quit, Enter - quit.								  #'),nl,
	write('# 3. To  Give up Enter - giveup							  #'),nl,

	write('# 4. To Enter your next play - Enter Coordinates in the format:  X/N.	  #'),nl,
	write('# (X - Row , Y - Column).  X = {a/b/c/d/e/f}, Y = {1/2/3/4/5/6}		  #'),nl,
	write('#                                                                         #'),nl,
	write('###########################################################################'),nl,nl,!.


announce_winner(CurrentBoard) :- !.
 

cumputer_thinking_animation :-
	write('.'),flush,
	sleep(600),
	write('.'),flush,
	sleep(600),
	write('.'),flush,
	sleep(600),
	write('.'),flush,
	sleep(600),
	write('.'),flush,
	nl,!.


print_ui_separetor :-
	write(' -------------------------------------------------------------------------------- '),nl,!.

handle_command(quit) :-
	abort.










