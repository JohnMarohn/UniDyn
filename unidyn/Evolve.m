(* ::Package:: *)

(** 
 ** Evolve.m
 ** Unitary evolution of operators
 ** John Marohn
 **)

BeginPackage["Evolve`",{"Global`","NC`","NCAlgebra`","OpCreate`","Mult`","Comm`","Spins`","Osc`"}]

Evolve::usage="Evolve[H,t,\[Rho]] represents unitary evolution of the density operator \[Rho] for a time t under the Hamiltonian H.  This function expands according to simplification rules but leaves the evolution unevaluated."

AllCommutingQ::usage="A test to see if all the terms in an expression commute.  Returns False for symbols."

Evolver::usage="Evolver[H,t,\[Rho](0)] calculates \[Rho](t) = Exp[-I H t] \[Rho](0) Exp[+I H t], assuming that H is time independent, according to the commutation rules followed by \[Rho](0) and H."

Evolver::unsolvable="Unrecognized evolution";

VisualComplexity::usage="A cost function to coax Mathematica into writing simpler-looking answers."

Begin["Private`"]

(*~ START ~*)

(*@ 
The evolution operator distribute over \VerbFcn{Plus} and \VerbFcn{NonCommutativeMultiply}.
@*)

Clear[Evolve];
Evolve[H$sym__, t$sym__, rho$sym_Plus] := Plus @@ (Evolve[H$sym, t$sym, #]&) /@ List @@ rho$sym

Evolve[H$sym__, t$sym__, rho$sym_NonCommutativeMultiply] := 
	NonCommutativeMultiply @@ (Evolve[H$sym, t$sym, #]&) /@ List @@ rho$sym

(*@
Scalars in front of the density operator should be pulled out front.
@*)

Evolve[H$sym__, t$sym__, Times[a_?NonCommutativeMultiply`CommutativeQ, rho$sym__]] := a Evolve[H$sym, t$sym, rho$sym]

(*@
A test to see if all the terms in a sum commute with each other. %
It is important to include an \VerbFcn{AllCommutingQ::usage} statement %
at the top of this package so that this function is available in the \verb+General`+ %
context of the notebook.
@*)

AllCommutingQ[H$sym_] := Module[{H$list,Comm$matrix},
  If [Head[H$sym] === Plus,
    H$list = List @@ H$sym;
	Comm$matrix = Outer[Comm,H$list, H$list];
	Return[And @@ ((# === 0)& /@ Flatten[Comm$matrix])],
  Return[False]
  ]
]

(*@
\emph{If} all the terms in the Hamiltonian commute, then we may distribute %
the \VerbFcn{Evolve} operator over the terms in the Hamiltonian. %
@*)

Evolve[H$sym_?AllCommutingQ,t$sym_, rho$sym_] := 
	NonCommutativeMultiply @@ (Evolve[#, t$sym, rho$sym]&) /@ List @@ H$sym

(*@
A function to coerce \emph{Mathematica} into writing simpler looking expressions, from %
http://mathematica.stackexchange.com/questions/5403/how-to-get-fullsimplify-to-fully-simplify-my-expression-with-custom-complexity-f
@*)

VisualComplexity:=(Count[ToBoxes[#],Except[" "|"("|")",_String],Infinity]&)

(*@
Main algorithm
@*)

Options[Evolver] = {quiet -> True};

Evolver[H$sym_, t$sym_, rho$sym$0_, opts : OptionsPattern[]] :=

Module[{k, a$vect, q, r, r$value, X, x4, x3, x2, x1, time, system, sol},
  Clear[rho$sym];
  rho$sym[0] = rho$sym$0;
  Do[
	rho$sym[k+1] = NCExpand[-I Comm[H$sym,rho$sym[k]]] 
      // FullSimplify[#,ComplexityFunction->VisualComplexity]&,
    {k,0,4}
  ];
  If[OptionValue[quiet] == False, Print["r = ",rho$sym[#]& /@ {0,1,2,3,4} // MatrixForm]];
  a$vect={0,0,0,0};
  r = Null;
  r = Catch[
    Do[

      q = NCExpand[NonCommutativeMultiply`inv[
        rho$sym[k]] ** rho$sym[3]
       ] // FullSimplify[#,ComplexityFunction->VisualComplexity]& ;

      If[NonCommutativeMultiply`CommutativeQ[q]==True,Throw[{3-k,q}]],
      {k,0,2}
    ]
  ];
  If[r=== Null,
    Message[Evolver::unsolvable];
    Return[rho$sym[#]& /@ {0,1,2,3,4}],
    r$value=r
  ];
  a$vect[[r$value[[1]]]] = r$value[[2]];
  A = {a$vect,{1, 0 ,0, 0},{0, 1, 0, 0}, {0, 0 ,1, 0}};
  If[OptionValue[quiet] == False,Print["A = ", A // MatrixForm]];
  X[time_] = {x4[time], x3[time], x2[time], x1[time]};
  system = {D[X[time],time] == A . X[time],
    x4[0]== rho$sym[3], x3[0]== rho$sym[2], x2[0]== rho$sym[1], x1[0]== rho$sym[0]};
  sol = DSolve[system,{x1,x2,x3,x4},time];
  Return[x1[time]  /. sol[[1]] /. time -> t$sym];
];

(*~ END ~*)

End[]

EndPackage[]

If[$VerboseLoad == True,
    Message[Evolve::usage]
    Message[Evolver::usage]
]




