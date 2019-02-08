% int_list.pl
:- ['preamble.pl'].
:- >>> 'prove that the value is equal to [0,1,2,3] when(assume input is 3)'.
:- >>> 'the following program p:'.
:- >>> ' get seq'.
:- >>> ' duplicate seq'.
:- >>> ' push(0) seq'.
:- >>> ' eq seq'.
:- >>> ' not seq'.
:- >>> ' while('.
:- >>> ' duplicate seq'.
:- >>> ' put seq'.
:- >>> ' push(1) seq'.
:- >>> ' sub seq'.
:- >>> ' duplicate seq'.
:- >>> ' push(0) seq'.
:- >>> ' eq seq'.
:- >>> ' not)'.
:- >>> 'is run in the context of any state'.
% We need to prove
% (forall s)(exists NS)[(p,s)-->>NS ^([0,1,2,3],s)-->>NF =(NS,NF)]
% proof
:- ['sem-stack.pl'].
% A nice coding trick for long proofs:
% 'program' is a predicate that holds our program
program(get seq
duplicate seq
push(0) seq
eq seq
not seq
while(
duplicate seq
put seq
push(1) seq
sub seq
duplicate seq
push(0) seq
eq seq
not)).
:- program(P),(P,s)-->>NS,([0,1,2,3],s)-->>NF,NS=NF.