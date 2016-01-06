(* ::Package:: *)

(**
 ** Comm.m
 ** Commutator algebra
 ** John A. Marohn
 ** 2016/01/06
 **)

BeginPackage["Comm`",{"Global`","NC`","NCAlgebra`","OpCreate`","Mult`"}]

Comm::usage="Comm[a,b] calculates the commutator of two operators.";

(*~ START ~*)

CommQ = NonCommutativeMultiply`CommutativeQ;

(*@ 
Limiting cases:
@*)

Comm[a$sym_?CommQ, b$sym_] := 0
Comm[a$sym_, b$sym_?CommQ] := 0
Comm[a$sym_, a$sym_] := 0

(*@
The commutator distributes over \VerbFcn{Plus}: 
@*)

Comm[a$sym__, b$sym_Plus] := Plus @@ (Comm[a$sym, #] & ) /@ List @@ b$sym
Comm[a$sym_Plus, b$sym__] := Plus @@ (Comm[#, b$sym] & ) /@ List @@ a$sym

(*@
Rules for factoring out scalars:
@*)

Comm[a$sym__ b$sym_?CommQ, c$sym__] := b$sym Comm[a$sym, c$sym]
Comm[a$sym__, b$sym_?CommQ c$sym__] := b$sym Comm[a$sym, c$sym]

(*@
Two rules for simplifying commutators involving products:
@*)

Comm[a$sym_NonCommutativeMultiply, C$sym_] := 
   Module[{A$sym,B$sym},
      A$sym = (List @@ a$sym)[[1]]  ;
      B$sym = NonCommutativeMultiply @@ Rest[List @@ a$sym] ;
      NonCommutativeMultiply[A$sym, Comm[B$sym, C$sym]] 
		+ NonCommutativeMultiply[Comm[A$sym, C$sym], B$sym]
    ]

Comm[A$sym_, b$sym_NonCommutativeMultiply]  := 
   Module[{B$sym,C$sym},
      B$sym = (List @@ b$sym)[[1]]  ;
      C$sym = NonCommutativeMultiply @@ Rest[List @@ b$sym] ;
      NonCommutativeMultiply[Comm[A$sym, B$sym], C$sym] 
		+ NonCommutativeMultiply[B$sym, Comm[A$sym, C$sym]]
   ]

(*@
Finally, a commutator between different operators of a different \emph{phyla} is zero:
@*)

Comm[a$sym_,b$sym_] := 0 /; phylum[a$sym] != phylum[b$sym];

(*~ END ~*)

EndPackage[]

If[$VerboseLoad == True,
    Message[Comm::usage];
]


(* ::Input:: *)
(**)
