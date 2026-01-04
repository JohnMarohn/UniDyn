(* ::Package:: *)

(** 
 ** Evolve.m
 ** Unitary evolution of operators
 ** John Marohn
 ** 2026/01/03
 **)

BeginPackage["Evolve`",{"Global`","OpQ`","Mult`","Comm`","Spins`","Osc`","Inv`"}]

Evolve::usage="Evolve[H, t, \[Rho](0)] calculates \[Rho](t) = Exp[-I H t] \[Rho](0) Exp[+I H t], assuming that H is time independent, according to the commutation rules followed by \[Rho](0) and H.";

AllCommutingQ::usage="A test to see if all the terms in a sum commute with each other.";

VisualComplexity::usage="A cost function to coax Mathematica into writing simpler-looking answers.";

Begin["Private`"]

(*~ START ~*)

(*@ 
The evolution operator distribute over \VerbFcn{Plus} and \VerbFcn{Mult}.
@*)

Clear[Evolve];
Evolve[H$sym__, t$sym__, rho$sym_Plus] := 
	Plus @@ (Evolve[H$sym, t$sym, #]&) /@ List @@ rho$sym
	
Evolve[H$sym__, t$sym__, rho$sym_Mult] := 
	Mult @@ (Evolve[H$sym, t$sym, #]&) /@ List @@ rho$sym

(*@
Scalars in front of the density operator should be pulled out front.
@*)

Evolve[H$sym__, t$sym__, Times[a_?ScalarQ, rho$sym__]] := a Evolve[H$sym, t$sym, rho$sym]

(*@
A test to see if all the terms in a sum commute with each other. %
It is important to include an \VerbFcn{AllCommutingQ::usage} statement %
at the top of this package so that this function is available in the \verb+General`+ %
context of the notebook.
@*)

AllCommutingQ[H$sym_] := Module[{H$list, Comm$matrix},
  If [Head[H$sym] === Plus,
    H$list = List @@ H$sym;
	Comm$matrix = Outer[Comm, H$list, H$list];
	Return[And @@ ((# === 0)& /@ Flatten[Comm$matrix])],
  Return[False]
  ]
]

(*@
\emph{If} all the terms in the Hamiltonian commute, then we may distribute %
the \VerbFcn{Evolve} operator over the terms in the Hamiltonian. %
@*)

Evolve[H$sym_?AllCommutingQ, t$sym_, rho$sym_] := 
	Mult @@ (Evolve[#, t$sym, rho$sym]&) /@ List @@ H$sym

(*@
A function to coerce \emph{Mathematica} into writing simpler looking expressions, from %
http://mathematica.stackexchange.com/questions/5403/how-to-get-fullsimplify-to-fully-simplify-my-expression-with-custom-complexity-f
@*)

VisualComplexity:=(Count[ToBoxes[#],Except[" "|"("|")",_String],Infinity]&)

(*~ END ~*)

End[]

EndPackage[]

If[$VerboseLoad == True,
    Message[Evolve::usage]
    Message[AllCommutingQ::usage]
    Message[VisualComplexity::usage]
];
