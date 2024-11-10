(* ::Package:: *)

(**
 ** Comm.m
 ** Commutator algebra
 ** John A. Marohn
 ** 2016/01/06
 **)

BeginPackage["Comm`",{"Global`","OpQ`","Mult`"}]

Comm::usage="Comm[a,b] calculates the commutator of two operators."

(*~ START ~*)

Comm[a$sym_?ScalarQ, b$sym_] := 0
Comm[a$sym_, b$sym_?ScalarQ] := 0
Comm[a$sym_, a$sym_] := 0

(*@
The commutator distributes over \VerbFcn{Plus}: 
@*)

Comm[a$sym__, b$sym_Plus] := Plus @@ (Comm[a$sym, #] & ) /@ List @@ b$sym
Comm[a$sym_Plus, b$sym__] := Plus @@ (Comm[#, b$sym] & ) /@ List @@ a$sym

(*@
Rules for factoring out scalars:
@*)

Comm[a$sym__ b$sym_?ScalarQ, c$sym__] := b$sym Comm[a$sym, c$sym]
Comm[a$sym__, b$sym_?ScalarQ c$sym__] := b$sym Comm[a$sym, c$sym]

(*@
Two rules for simplifying commutators involving products:
@*)

Comm[a$sym_Mult, C$sym_] := 
   Module[{A$sym,B$sym},
      A$sym = (List @@ a$sym)[[1]]  ;
      B$sym = Mult @@ Rest[List @@ a$sym] ;
      Mult[A$sym, Comm[B$sym, C$sym]] + Mult[Comm[A$sym, C$sym], B$sym]
    ]

Comm[A$sym_, b$sym_Mult]  := 
   Module[{B$sym,C$sym},
      B$sym = (List @@ b$sym)[[1]]  ;
      C$sym = Mult @@ Rest[List @@ b$sym] ;
      Mult[Comm[A$sym, B$sym], C$sym] + Mult[B$sym, Comm[A$sym, C$sym]]
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
