% proof-comm.pl
:- ['preamble.pl'].
:- >>> 'the following program p:'.
:- >>> ' push(a) seq'.
:- >>> ' push(b) seq'.
:- >>> ' add'.
:- >>> 'is run in the context of any state'.
:- >>> 'and'.
:- >>> 'the following program q:'.
:- >>> ' push(b) seq'.
:- >>> ' push(a) seq'.
:- >>> ' add'.
:- >>> 'is run in the context of any state'.
:- >>> 'prove that the (p,s) ~ (q,s)'.
%
% show that
% (forall s,a,b)(exists V0,V1)
% [sem(push(a) seq push(b) seq add,s,V0)^sem(push(b) seq push(a) seq add,s,V1)^=(V0,V1)]
% assuming
% (a,s) -->> va0.
% (b,s) -->> va1.
% load semantics
:- ['sem-stack.pl'].
% assumptions on semantic values of expressions
:- asserta((a,s)-->>va0).
:- asserta((b,s)-->>va1).
% assumption on integer addition commutativity
:- asserta(comm(A + B, B + A)).
% proof
:- (push(a) seq push(b) seq add,s)-->>V0, (push(b) seq push(a) seq add,s)-->>V1,comm(V0,VC0),VC0=V1.