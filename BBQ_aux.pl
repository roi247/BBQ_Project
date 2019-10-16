% -----------------------------------------------------------------------------------------------------------------
% 							 Generic Auxilliary Predicates 								 
% -----------------------------------------------------------------------------------------------------------------


max(A, B, A) :- 
	A > B,!.

max(A, B, B) :- 
	B >= A,!.


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


% ---------------------------------------------------------------------------------- 
% Predicate- random_access_list
% Summary  - random access to list at given index, returns the var at this index, and lists of 
%		 before and after this var.
%		 It does that using append(conc) and finding sublists of the given list.
% ---------------------------------------------------------------------------------- 
random_access_list(List, Index, BeforeVar, Var, AfterVar) :-

	% Find matching sublists according to the index
	append(P1, P2, List),
	append(V1, V2, P1),	
	length(V2, 1),
	V2 = [Var],
	length(V1, Index),
	BeforeVar = V1,
	AfterVar = P2,!.


% ---------------------------------------------------------------------------------- 
% Predicate- list_of_same_var
% Summary  - True if given list has the same var Len times
% ---------------------------------------------------------------------------------- 
list_of_same_var([], Var, 0) :- !.

list_of_same_var([Var|List], Var, Len) :-
	Len1 is Len - 1,	
	list_of_same_var(List, Var , Len1).	
	



% ---------------------------------------------------------------------------------- 
% Predicate- Value Relation
% Summary  - Generic prediacte for getting the value of a list variable 
% Could be integer, list, etc...
% ---------------------------------------------------------------------------------- 
value(Number, Number) :-
	integer(Number),!.

value(List, Value) :-
	list(List),
	sum(List, Value),!.

% ---------------------------------------------------------------------------------- 
% Predicate- List maximum predicate
% Summary  - Generic prediacte for achiving list maximum using value predicate
% ---------------------------------------------------------------------------------- 
find_list_maximum([], CurrentMaxVar, CurrentMaxValue, CurrentMaxVar, CurrentMaxValue) :- !.

find_list_maximum([Var|OtherVars], CurrentMaxVar, CurrentMaxValue, AbsuluteMaxVar , AbsuluteMaxValue) :-
	value(Var, VarValue),
	VarValue >= CurrentMaxValue,!, 	% RED CUT! DONT REMOVE!
	find_list_maximum(OtherVars, Var, VarValue, AbsuluteMaxVar, AbsuluteMaxValue).
	

find_list_maximum([Var|OtherVars], CurrentMaxVar, CurrentMaxValue, AbsuluteMaxVar, AbsuluteMaxValue) :-
	find_list_maximum(OtherVars, CurrentMaxVar, CurrentMaxValue, AbsuluteMaxVar, AbsuluteMaxValue).


% +++++++ EXTERNAL USE +++++++++
find_list_maximum([V|ListRest], MaxVar, MaximumValue) :-
	find_list_maximum(ListRest,V , V, MaxVar, MaximumValue).



% ---------------------------------------------------------------------------------- 
% Predicate- sleep
% Summary  - sleep for a given amout of milliseconds
% ---------------------------------------------------------------------------------- 
sleep( Milliseconds ) :-
	ms((repeat,wait(0)), Duration),
	Duration >= Milliseconds,
	!.






