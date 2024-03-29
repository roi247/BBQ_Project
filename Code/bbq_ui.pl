
% -----------------------------------------------------------------------------------------------------------------
% 								 Game Documentation  							 
% -----------------------------------------------------------------------------------------------------------------

/*
												
Programmer -  Roi Talbi									
File Name -   bbq_ui.pl									
												
Description - Game Main flow file. 							
		  start_game predicate starts the game routine, it receives 
	 	  the game level desired as an input.				
		  										
		  The Game goal is to end up with the longest row/column 	
		  Computer is player B , player is player Q .			
		  Your goal is to fill the board with 2 Qs at a turn (same for the computer)
		  The player that wins is the one that ends up with the longest row or column
		  at the board (if equal ==> game ends with a TIE).		
														
												
Input - Game  Level desired (easy / medium / hard)				
Output- Game  process (Board UI representation and menu instructions)   
												
Synopsys -    ****  HOW TO RUN THE GAME?  ****					
	 	  1. compile this file using Crl+L 					
		  2. Enter: 								
		     start_game(easy).  OR  start_game(medium).  OR  start_game(hard).
		    (Choose the game level you want).				
		     Game instructions will be shown, enter help. to reach the help menu
		     Enter endgame. to end the game at this point and see who 
	 	     is the winner							
												
												
*/


% -----------------------------------------------------------------------------------------------------------------
% 								 Gameplay UI Predicates  							 
% -----------------------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------- 
% Predicate- compile_game
% Summary  - compiles all the neccesery files for the game in the given difficulty
% ---------------------------------------------------------------------------------- 
compile_game(Difficulty) :-
	compile(bbq_aux),
	compile(bbq_board),

	(Difficulty = hard,!,
  	 abolish_files(bbq_gameplay_medium),
	 abolish_files(bbq_gameplay_easy),
	 compile(bbq_gameplay_hard)
	 ;
	 Difficulty = medium,!,
 	 abolish_files(bbq_gameplay_hard),
	 abolish_files(bbq_gameplay_easy),
	 compile(‏‏bbq_gameplay_medium)
	 ;
	 Difficulty = easy,!,
	 abolish_files(bbq_gameplay_hard),
	 abolish_files(bbq_gameplay_medium),
	 compile(‏‏‏‏bbq_gameplay_easy)
	).




% ---------------------------------------------------------------------------------- 
% Predicate- start_game						
% Summary  - start the game main flow with a new board. 
%		 compiles the relevent gameplay file according to the difficulty required
% ---------------------------------------------------------------------------------- 
start_game(Difficulty) :-
	compile_game(Difficulty),
	EmptyBoard = b(6,[[n,n,n,n,n,n],[n,n,n,n,n,n],[n,n,n,n,n,n],[n,n,n,n,n,n],[n,n,n,n,n,n],[n,n,n,n,n,n]]),
	game(EmptyBoard),!.



start_game :-
	start_game(medium).

% ---------------------------------------------------------------------------------- 
% Predicate- game
% Summary  - Main flow of the game : read input, play the next move or handle any special command
%		 and continue with that procedure untill the game is over
% ---------------------------------------------------------------------------------- 
game(CurrentBoard) :-
	
	% Print current board
	write('Board: '),nl,nl,
	print_board(CurrentBoard),
	print_ui_separetor,

	% Check if game is over and annouce winner if so
	(game_over(CurrentBoard),!,announce_winner(CurrentBoard); true),

	% Print instructions to player
	write('Please Enter where you would like to place your Qs'),nl,
	write('For any other instructions, please enter the relevent command. type: "help" For help'),nl,

	% Read the user next instruction
	% If special instruction - handle accordingly. If valid play- set the new board accordingly 
	write('---> '), read(Input),
	(handle_command(Input, CurrentBoard),!, game(CurrentBoard)
	 ;
	 Input= Mov1+Mov2,
	 place_in_board_index(CurrentBoard, q , Mov1, NewBoard1),
	 place_in_board_index(NewBoard1, q , Mov2, NewBoard),
	 !
	 ;
	 write('********** INVALID INPUT! .Invalid Board Coordinates / invalid command entered **********'),nl,
	 cumputer_thinking_animation, game(CurrentBoard)
	 ),

 	print_board(NewBoard), print_ui_separetor, nl, nl,
	
	% Check if game is over and annouce winner if so
	(game_over(NewBoard),!,announce_winner(NewBoard); true),


	% Computer play , use alpha-beta algorithem to pick the best play,
	% Print this play and start another loop..
	write('***	Computer Turn!	***'),nl,nl,
	cumputer_thinking_animation,

	Pos = s(player_b, 1, NewBoard),
	alphabeta(Pos, -999, 999, GoodPos, Value),	
	GoodPos = s(_,_, UpdatedBoard),
	game(UpdatedBoard),!.
	

% ---------------------------------------------------------------------------------- 	
% Predicate- handle_command										 	
% Summary  - handle all the sorts of special commands in the game (help/quit/giveup etc..)
% ---------------------------------------------------------------------------------- 	
handle_command(help, _) :-
	nl,write('##############################################################################'),nl,
	write('#		 			Game Help Menu 				 	     #'),nl,
	write('#                                                                            #'),nl,
	write('# 1. For Help, Enter - help.								     #'),nl,
	write('# 2. To  Quit, Enter - quit.								     #'),nl,
	write('# 3. To  End Game Enter - endgame.							     #'),nl,

	write('# 4. To Enter your next play- Enter Coordinates in the format:  X1/N1+X2/N2. #'),nl,
	write('# (X - Row , N - Column).  X = {a/b/c/d/e/f}, N = {1/2/3/4/5/6}		     #'),nl,
	write('#                                                                            #'),nl,
	write('##############################################################################'),nl,nl,!.


handle_command(endgame, CurrentBoard) :-
	announce_winner(CurrentBoard).

handle_command(quit, _) :-
	abort.



% ---------------------------------------------------------------------------------- 
% Predicate- announce_winner
% Summary  - print cool UI game over animation. announce who is the winner (or tie)
% ---------------------------------------------------------------------------------- 

announce_winner(CurrentBoard) :-
	nl,
	write(' ______________________________________________________________'),nl,
	write(' ______________________________________________________________'),nl,nl,
	write(' ||		 					  	 		 ||'),nl,
	write(' ||	'),write(' __ _  __ _ _ __ ___   ___    _____   _____ _ __       ||'),nl,
	write(' ||	'),write('  / _  |/ _| | |_ | _ \ / _ \  / _ \ \ / / _ \ .__| 	 ||'),nl,
	write(' ||	'),write(' | (_| | (_| | | | | | |  __/ | (_) \ V /  __/ |   	 ||'),nl,
	write(' ||	'),write('  \__, |\__,_|_| |_| |_|\___|  \___/ \_/ \___|_|   	 ||'),nl,
	write(' ||	'),write('   __/ |                                           	 ||'),nl,
	write(' ||	'),write('  |___/  								 ||'),nl,
	write(' ||		 					  	 		 ||'),nl,
	write(' ||		 					  	 		 ||'),nl,

	game_winner(CurrentBoard, Winner),
	(Winner = player_b,!,
	 write(' ||  Winner is .............. Player B !!!!!!!!		       ||'),nl
	 ; 
	 Winner = player_q,!,
	 write(' ||  Winner is .............. Player Q !!!!!!!!			 ||'),nl
	 ; 
	 Winner = tie,!,
	 write(' ||  Game has ended with a TIE  !!!!!!!!!!!!!!			 ||'),nl
	),
	
	write(' ||		 					  	 		 ||'),nl,
	write(' ||		 					  	 		 ||'),nl,
	write(' ||		 					  	 		 ||'),nl,
	write(' ||		 					  	 		 ||'),nl,
	write(' ||		 					  	 		 ||'),nl,
	write(' ______________________________________________________________'),nl,
	write(' ______________________________________________________________'),nl,nl,
	abort.
	

	
% ---------------------------------------------------------------------------------- 
% Predicate- cumputer_thinking_animation
% Summary  - prints some dots and sleeps a bit, make it look like the computer is 
%		 thinking about the next movment
% ---------------------------------------------------------------------------------- 
cumputer_thinking_animation :-
	write('.'),flush,
	sleep(600),
	write('.'),flush,
	sleep(600),
	write('.'),flush,
	sleep(700),
	write('.'),flush,
	sleep(700),
	write('.'),flush,
	nl,!.



print_ui_separetor :-
	write(' -------------------------------------------------------------------------------- '),nl,!.


% -----------------------------------------------------------------------------------------------------------------
% 								 Aux UI Predicates  								 
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












