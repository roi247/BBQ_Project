
% -----------------------------------------------------------------------------------------------------------------
% 								 Related Predicates  								 
% -----------------------------------------------------------------------------------------------------------------

cl :- compile_all,!.

compile_all :-
	compile(bbq_aux),
	compile(bbq_board),

	compile(bbq_ui),
	compile(bbq_gameplay_hard).


% -----------------------------------------------------------------------------------------------------------------
% 								 Game Predicates  								 
% -----------------------------------------------------------------------------------------------------------------

% ---------------------------
% Game State Format :
% s(Player, Depth, Board)
%
% Player = player_b / player_q
% Depth = current search depth 
% Board = b(Size, Rows)
% ---------------------------


/*

MAX - player_q
MIN - player_b


*/


% ---------------------------------------------------------------------------------- 
% Predicate- 
% Summary  - 
% ---------------------------------------------------------------------------------- 



opposite_player(player_b, player_q) :- !.
opposite_player(player_q, player_b) :- !.


player_sign(player_b, b) :- !.
player_sign(player_q, q) :- !.


move(Pos, NextPos) :- 
	Pos = s(Player, Depth, Board),
	player_sign(Player, Sign),	

	% Place the player sign and retrive the new board with that sign
	place_in_board(Board, Sign, NewBoard),

	% Construct Next Position
	opposite_player(Player, OppositePlayer),
	Depth1 is Depth + 1,
	NextPos = s(OppositePlayer, Depth1 , NewBoard).


stop_search(s(_, Depth, Board)) :- 
	(Depth >= 1 ; board_empty_spots_left(Board, 0)),!.


moves(Pos, PosList):-
    ( stop_search(Pos), fail;
	  bagof(Pos1,move(Pos,Pos1),PosList)
	).
	

max_to_move(s(player_q,_,_)).

min_to_move(s(player_b,_,_)).


% ---------------------------------------------------------------------------------- 
% Predicate- staticval											
% Summary  - huristic static evaluation function of a given Pos, 				
%		 would be positive and maximzed for MAX player and netative minimized for MIN player
%		 The better the Pos for a player - the greater the score is.		 
%		 												
%	Huristic function is calculated in this way:						
%	For player Q: Factor_Q - Factor_B								
%	For player B: -(Factor_B - Factor_Q)							
%														
%	Factor is the player current longest row TIME the potential maxmimum line that can be achived 
%			 											
%	Factor_X = PlayerX_longest_line * (PlayerX_longest_line + PlayerX_potential_empty_spots_left)
%		 												
%		 												
% ----------------------------------------------------------------------------------	
staticval(Pos, Val):-
	Pos = s(Player, _, Board),!,
	Board = b(Size, Rows),

	board_longest_line(Board, q, Q_LongestLine, Q_LineLength),
	row_potential_spots_left(Q_LongestLine, q, Q_SpotsLeftCount),

	board_longest_line(Board, b, B_LongestLine, B_LineLength),
	row_potential_spots_left(B_LongestLine, b, B_SpotsLeftCount),

	Factor_Q is (Q_LineLength * (Q_LineLength + Q_SpotsLeftCount)),
	Factor_B is (B_LineLength * (B_LineLength + B_SpotsLeftCount)),

	(Player = player_q,!,
	 Val is Factor_Q - Factor_B
	;
	Player = player_b,!,
	 Val is -1 * (Factor_B - Factor_Q)),!.


	
% The alpha-beta algorithm
% http://media.pearsoncmg.com/intl/ema/ema_uk_he_bratko_prolog_3/prolog/ch22/fig22_5.txt 

alphabeta( Pos, Alpha, Beta, GoodPos, Val)  :-
	moves( Pos, PosList), !,
	boundedbest( PosList, Alpha, Beta, GoodPos, Val);
	staticval( Pos, Val).                              % Static value of Pos 

boundedbest( [Pos | PosList], Alpha, Beta, GoodPos, GoodVal)  :-
	alphabeta( Pos, Alpha, Beta, _, Val),
	goodenough( PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal).

goodenough( [], _, _, Pos, Val, Pos, Val)  :-  !.    % No other candidate

goodenough( _, Alpha, Beta, Pos, Val, Pos, Val)  :-
	min_to_move( Pos), Val > Beta, !                   % Maximizer attained upper bound
	;
	max_to_move( Pos), Val < Alpha, !.                 % Minimizer attained lower bound

goodenough( PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal)  :-
	newbounds( Alpha, Beta, Pos, Val, NewAlpha, NewBeta),    % Refine bounds  
	boundedbest( PosList, NewAlpha, NewBeta, Pos1, Val1),
	betterof( Pos, Val, Pos1, Val1, GoodPos, GoodVal).

newbounds( Alpha, Beta, Pos, Val, Val, Beta)  :-
	min_to_move( Pos), Val > Alpha, !.                 % Maximizer increased lower bound 

newbounds( Alpha, Beta, Pos, Val, Alpha, Val)  :-
	max_to_move( Pos), Val < Beta, !.                 % Minimizer decreased upper bound 

newbounds( Alpha, Beta, _, _, Alpha, Beta).          % Otherwise bounds unchanged 

betterof( Pos, Val, Pos1, Val1, Pos, Val)  :-        % Pos better than Pos1 
  	min_to_move( Pos), Val > Val1, !
	;
	max_to_move( Pos), Val < Val1, !.

betterof( _, _, Pos1, Val1, Pos1, Val1).             % Otherwise Pos1 better


























