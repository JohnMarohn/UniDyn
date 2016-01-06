(* ::Package:: *)

(** 
 ** Evolve.m
 ** Unitary evolution of operators
 ** John Marohn
 **)

BeginPackage["Evolve`",{"Global`","NC`","NCAlgebra`","OpCreate`","Mult`","Comm`","Spins`","Osc`"}]

Evolve::usage="Evolve[H,t,\[Rho]$0] calculates Exp[-I H t] \[Rho]$0 Exp[+I H t] assuming that H is time independent."

AllCommutingQ::usage="A test to see if all the terms in an expression commute.  Returns False for symbols."

Begin["Private`"]

(*~ START ~*)

(*@ 
The evolution operator distribute over \VerbFcn{Plus} and \VerbFcn{NonCommutativeMultiply}.
@*)

Clear[Evolve];
Evolve[H__, t__, rho_Plus] := Plus @@ (Evolve[H, t, #]&) /@ List @@ rho

Evolve[H__, t__, rho_NonCommutativeMultiply] := 
	NonCommutativeMultiply @@ (Evolve[H, t, #]&) /@ List @@ rho

(*@
Scalars in front of the density operator should be pulled out front.
@*)

Evolve[H__, t__, Times[a_?NonCommutativeMultiply`CommutativeQ, rho__]] := a Evolve[H, t, rho]

(*@
A test to see if all the terms in a sum commute with each other. %
It is important to include an \VerbFcn{AllCommutingQ::usage} statement %
at the top of this package so that this function is available in the \verb+General`+ %
context of the notebook.
@*)

AllCommutingQ[H_] := Module[{H$list,Comm$matrix},
  If [Head[H] === Plus,
    H$list = List @@ H;
	Comm$matrix = Outer[Comm,H$list, H$list];
	Return[And @@ ((# === 0)& /@ Flatten[Comm$matrix])],
  Return[False]
  ]
]

(*@
\emph{If} all the terms in the Hamiltonian commute, then we may distribute %
the \verbFcn{Evolve} operator over the terms in the Hamiltonian.
@*)

Evolve[H_?AllCommutingQ,t_, rho_] := NonCommutativeMultiply @@ (Evolve[#, t, rho]&) /@ List @@ H

(*~ END ~*)

End[]

EndPackage[]

If[$VerboseLoad == True,
    Message[Evolve::usage]
]



