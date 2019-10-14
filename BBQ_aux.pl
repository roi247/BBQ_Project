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




















