% max_two_numbers.pl
:- ['preamble.pl'].
:- >>> 'prove that the value is equal to x (assume x>y) when'.
:- >>> 'the following program p:'.
:- >>> ' get seq'.
:- >>> ' duplicate seq'.
:- >>> ' pop(x) seq'.
:- >>> ' get seq'.
:- >>> ' duplicate seq'.
:- >>> ' pop(y) seq'.
:- >>> ' le seq'.
:- >>> ' if('.
:- >>> ' (push(y) seq put),'.
:- >>> ' (push(x) seq put))'.
:- >>> 'is run in the context of any state'.
% We need to prove
% (forall s)(exists NS)[(p,s)-->>NS ^(get(x),s)-->>NF^=(NS,NF)]
% proof
:- ['sem-stack.pl'].
% A nice coding trick for long proofs:
% 'program' is a predicate that holds our program
program(get seq
duplicate seq
pop(x) seq
get seq
duplicate seq
pop(y) seq
le seq
if(
(push(y) seq put),
(push(x) seq put))).
:- program(P),(P,s)-->>NS,(get(x),s)-->>NF,NS=NF.

