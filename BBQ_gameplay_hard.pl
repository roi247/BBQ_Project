
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



game_over(s(_, Depth, Board)) :- 
	(Depth > 1 ; board_empty_spots_left(Board, 0)),!.






moves(Pos, PosList):-
    ( game_over(Pos), fail;
	  bagof(Pos1,move(Pos,Pos1),PosList)
	).
	

max_to_move(s(player_q,_)).

min_to_move(s(player_b,_)).
	
staticval(Pos, Val):-
    max_to_move(Pos), Val is -1 .
staticval(Pos, Val):-
    min_to_move(Pos), Val is 1 .


	
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


























