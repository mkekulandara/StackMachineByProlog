% adding_two_numbers.pl
:- ['preamble.pl'].
:- >>> 'prove that the value is equal to 3 when'.
:- >>> 'the following program p:'.
:- >>> ' push(1) seq'.
:- >>> ' push(2) seq'.
:- >>> ' add'.
:- >>> 'is run in the context of any state'.
% We need to prove
% (forall s)(exists NS)[(p,s)-->>NS^(push(3),s)-->>NF ^ =(NS,NF)]
% proof
:- ['sem-stack.pl'].
% A nice coding trick for long proofs:
% 'program' is a predicate that holds our program
program(push(1) seq
push(2) seq
add).
:- program(P),(P,s)-->>NS,(P,s)-->>NF,NS=NF.
