(* ::Package:: *)

(** 
 ** Inv.m
 ** The operator inverse operation
 ** John Marohn
 ** 2025/12/24
 **)


BeginPackage["Inv`",{"Global`","OpQ`","Mult`"}]

Inv::usage="Inv[a] returns the inverse of the expression."

(*~ START ~*)

(*@ 
Start with bottom-out and empty cases. % 
@*)

Inv[a$sym_?ScalarQ]:=1/a$sym
Inv[]:=1

(*@ 
\VerbCmd{Inv[b] b} and \VerbCmd{b Inv[b]} in an expression should reduce %
to 1 if \VerbCmd{b} is an operator. In the following expressions, the % 
\VerbCmd{HoldPattern[]} is important; without it, \VerbCmd{Inv[]} will %
get evaluated before the pattern is defined.
@*)

HoldPattern[Mult[a$sym___, Inv[b$sym_?OperatorQ], b$sym_?OperatorQ, c$sym___]] := Mult[a$sym, c$sym]
HoldPattern[Mult[a$sym___, b$sym_?OperatorQ, Inv[b$sym_?OperatorQ], c$sym___] ] := Mult[a$sym, c$sym]

(*@ 
Try to explain to \emph{Mathematica} how to factor out scalars.  %
When you have a scalar times an operator, the inverse is the product of the two inverses, % 
with the inverse of the scalar being one over the scalar.
@*)

Inv[Times[a$sym_?ScalarQ, b$sym_?OperatorQ]] := Times[Inv[a$sym], Inv[b$sym]]
Inv[Times[a$sym_?OperatorQ, b$sym_?ScalarQ]] := Times[Inv[b$sym], Inv[a$sym]]

(*@ 
The inverse applied to a \VerbCmd{Mult[]} of operators should % 
give a list of \VerbCmd{Inv[]} operators multiplied together, %
with the list order \emph{reversed}.
@*)

Inv[a$sym___, b$sym_Mult, c$sym___]:=Mult@@(Inv[a$sym, #, c$sym]&)/@ Reverse[List@@b$sym]

(*@ 
The inverse \emph{does not} distribute over addition.
@*)

(*~ END ~*)

EndPackage[]

If[$VerboseLoad == True,
    Message[Inv::usage];
]












