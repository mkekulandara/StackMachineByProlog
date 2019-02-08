% Version 1.0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- ['preamble.pl'].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% complete semantics of the source language:
%
%  C ::= skip
%	  | push(x)
%	  | push(n)
%	  | push(true)
%	  | push(false)
%	  | pop
%	  | pop(x)
%	  | duplicate
%	  | put
%	  | get
%	  | add
%	  | sub
%	  | mult
%	  | eq
%	  | le
%	  | not
%	  | and
%	  | or
%	  | seq(C,C)
%	  | if(C,C)
%	  | whiledo(C)
%
% for convenience sake make seq infix and left associative
:- op(1200,yfx,seq).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% semantics of expressions

(C,_) -->> C :-                    		% constants
    int(C),!.
	
(X,State) -->> Val :-              		% variables
    atom(X),
    lookup(X,State,Val),!.	

(true,_) -->> true :- !.              	% constants

(false,_) -->> false :- !.             	% constants
	
(skip,Stack) -->> Stack :- !.      		% skip

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% semantics of stack operations

(push(X),Stack) -->> NewStack :-		% push variable X
	atom(X),
    lookup(X,_,Val),
	push_stack(Val,Stack,NewStack),!.
	
(push(n),Stack) -->> NewStack :-      	% push constant N
	push_stack(int(n),Stack,NewStack),!.
	
(push(true),Stack) -->> NewStack :-		% push boolean true
	(true,_) -->> true,
	push_stack(true,Stack,NewStack),!.
	
(push(false),Stack) -->> NewStack :-   	% push boolean false
	(false,_) -->> false,
	push_stack(false,Stack,NewStack),!.
	
(pop,Stack) -->> NewStack :-			% pop the stack
	is_empty(Stack)-->>false,
	pop_stack(Stack,_,NewStack),!.
	
(pop(X),Stack) -->> NewStack :-			% pop the stack and save the value to X
	is_empty(Stack)-->>false,
	pop_stack(Stack,Value,_),
	put(X,Value,Stack,NewStack),!.
	
(duplicate,Stack) -->> NewStack :-		% duplicate the top of the stack
	is_empty(Stack)-->>false,
	pop_stack(Stack,Value,_),
	push_stack(Value,Stack,NewStack),!.
	
(put,Stack) -->> Val :-					% put the first value to terminal
	is_empty(Stack)-->>false,
	pop_stack(Stack,Val,_),
	write(Val),!.
	
(get,Stack) -->> NewStack :-			% get the input value and push to stack
	read(X),
	push_stack(X,Stack,NewStack),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% semantics of arithmetic expressions
	
(add,Stack) -->> NewStack :-			% addition
	is_empty(Stack)-->>false,
	pop_stack(Stack,Val1,NewStack1),
	is_empty(NewStack1)-->>false,
	pop_stack(NewStack1,Val2,NewStack2),
	Val xis Val2 + Val1,
	push_stack(Val,NewStack2,NewStack),!.
	
(sub,Stack) -->> NewStack :-			% subtraction
	is_empty(Stack)-->>false,
	pop_stack(Stack,Val1,NewStack1),
	is_empty(NewStack1)-->>false,
	pop_stack(NewStack1,Val2,NewStack2),
	Val xis Val2 - Val1,
	push_stack(Val,NewStack2,NewStack),!.
	
(mult,Stack) -->> NewStack :-			% multiplication
	is_empty(Stack)-->>false,
	pop_stack(Stack,Val1,NewStack1),
	is_empty(NewStack1)-->>false,
	pop_stack(NewStack1,Val2,NewStack2),
	Val xis Val2 * Val1,
	push_stack(Val,NewStack2,NewStack),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% semantics of boolean expressions
	
(eq,Stack) -->> NewStack :-				% equality
	is_empty(Stack)-->>false,
	pop_stack(Stack,Val1,NewStack1),
	is_empty(NewStack1)-->>false,
	pop_stack(NewStack1,Val2,NewStack2),
	Val xis (Val2 =:= Val1),
	push_stack(Val,NewStack2,NewStack),!.
	
(le,Stack) -->> NewStack :-				% le
	is_empty(Stack)-->>false,
	pop_stack(Stack,Val1,NewStack1),
	is_empty(NewStack1)-->>false,
	pop_stack(NewStack1,Val2,NewStack2),
	Val xis (Val2 =< Val1),
	push_stack(Val,NewStack2,NewStack),!.
	
(and,Stack) -->> NewStack :-			% and
	is_empty(Stack)-->>false,
	pop_stack(Stack,Val1,NewStack1),
	is_empty(NewStack1)-->>false,
	pop_stack(NewStack1,Val2,NewStack2),
	Val xis (Val2 and Val1),
	push_stack(Val,NewStack2,NewStack),!.
	
(or,Stack) -->> NewStack :-				% or
	is_empty(Stack)-->>false,
	pop_stack(Stack,Val1,NewStack1),
	is_empty(NewStack1)-->>false,
	pop_stack(NewStack1,Val2,NewStack2),
	Val xis (Val2 or Val1),
	push_stack(Val,NewStack2,NewStack),!.	

(not(_),Stack) -->> NewStack :-		% not
	is_empty(Stack)-->>false,
	pop_stack(Stack,Val1,NewStack1),
	Val xis (not Val1),
	push_stack(Val,NewStack1,NewStack),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% semantics of commands
	
(seq(C0,C1),Stack) -->> NewStack :-      	% composition, seq
    (C0,Stack) -->> NewStack1,
    (C1,NewStack1) -->> NewStack,!.
	
(if(C1,_),Stack) -->> NewStack :-     	% if
	is_empty(Stack)-->>false,
	pop_stack(Stack,Val,NewStack1),
	(Val,_) -->> true,
    (C1,NewStack1) -->> NewStack,!.

(if(_,C2),Stack) -->> NewStack :-     	% if
	is_empty(Stack)-->>false,
	pop_stack(Stack,Val,NewStack1),
	(Val,_) -->> false,
    (C2,NewStack1) -->> NewStack,!.
	
(whiledo(_),Stack) -->> NewStack :-    	% while
    is_empty(Stack)-->>false,
	pop_stack(Stack,Val,NewStack1),
	(Val,_) -->> false,
	NewStack1=NewStack,!.

(whiledo(C),Stack) -->> NewStack :-    	% while
    is_empty(Stack)-->>false,
	pop_stack(Stack,Val,NewStack1),
	(Val,_) -->> true,
    (C,NewStack1) -->> NS,
    (whiledo(C),NS) -->> NewStack,!. 
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'lookup(+Variable,+State,-Value)' looks up
% the variable in the state and returns its bound value.
:- dynamic lookup/3.                % modifiable predicate

lookup(_,s0,0).

lookup(X,state([],S),Val) :-
    lookup(X,S,Val).

lookup(X,state([bind(Val,X)|_],_),Val).

lookup(X,state([_|Rest],S),Val) :- 
    lookup(X,state(Rest,S),Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'put(+Variable,+Value,+Stack,-FinalStack)' adds
% a binding term to the state.
:- dynamic put/4.                   % modifiable predicate

put(X,Val,state(L,S),state([bind(Val,X)|L],S)).

put(X,Val,S,state([bind(Val,X)],S)) :- 
    atom(S).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'is_empty(+Stack)' check a stack empty or not empty
% the predicate 'pop_stack(+Stack,+Value,-FinalStack)' take the first element out from the stack.
% the predicate 'push_stack(+Value,+Stack,-FinalStack)' insert a value to the first element of the stack.

is_empty([]).

pop_stack(Stack,Value,NewStack) :- 
	Stack = [H|T], 
	Value = H, 
	NewStack = T.

push_stack(Value,Stack,NewStack) :- 
	NewStack = [Value|Stack].