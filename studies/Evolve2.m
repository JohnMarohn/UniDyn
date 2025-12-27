(* ::Package:: *)

(** 
 ** Evolve2.m
 ** Unitary evolution of operators
 ** John Marohn
 ** 2025/12/26
 **)
 
BeginPackage["Evolve2`",{"Global`","OpQ`","Mult`","Comm`","Spins`","Osc`","Inv`"}]
 
Evolve::usage="Evolve[H, t, \[Rho]] represents unitary evolution of the density operator \[Rho] for a time t under the Hamiltonian H.  This function expands according to simplification rules but leaves the evolution unevaluated.";
  
AllCommutingQ::usage="A test to see if all the terms in an expression commute.  Returns False for symbols.";

Evolver::usage="Evolver[H, t, \[Rho](0)] calculates \[Rho](t) = Exp[-I H t] \[Rho](0) Exp[+I H t], assuming that H is time independent, according to the commutation rules followed by \[Rho](0) and H.";

Evolver::unsolvable="Unrecognized evolution";

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

(*@
The \VerbFcn{Evolver2} function follows. By default, the function will not %
print out intermediate results during the computation.  %
@*)

Options[Evolver] = {quiet -> True};

Evolver[H$sym_, t$sym_, rho$sym$0_, opts : OptionsPattern[]] :=

Module[{rho$sym,divisions, commutators,\[Rho],w},

  rho$sym[0] = rho$sym$0;

(*@ Evalute the derivates of the density operator by repeatedly applying the %
commutator. Simplify them as much as possible. Getting the simplication right %
is tricky.  A special function has to be fed to \VerbFcn{FullSimplify} to get %
it to return useful results.  @*)

  Do[
	rho$sym[k+1] = (-I Comm[H$sym, rho$sym[k]] /. Mult -> SortedMult)
	      // FullSimplify[#, ComplexityFunction->VisualComplexity]&,
    {k,0,4}
  ];

If[OptionValue[quiet] == False, Print["\!\(\*SuperscriptBox[\(\[Rho]\), \((n)\)]\)= ", rho$sym[#]& /@ {0,1,2,3}]];

If[rho$sym[1] == 0, 
If[OptionValue[quiet] == False,Print["Case 0"]];
Return[rho$sym[0]]];

If[Comm[rho$sym[1],H$sym ] == 0,
If[OptionValue[quiet] == False,Print["Case 1L"]];
Return[rho$sym[0] + rho$sym[1] t$sym]];

commutators = {rho$sym[1], 
Comm[rho$sym[0],rho$sym[1]],
Comm[rho$sym[0],rho$sym[2]],
Comm[rho$sym[1],rho$sym[3]]};

If[OptionValue[quiet] == False,Print["commutators = ",commutators]];

divisions ={Mult[Inv[rho$sym[0]],rho$sym[1]],
  Mult[Inv[rho$sym[0]],rho$sym[2]],
  Mult[Inv[rho$sym[1]],rho$sym[3]]};

If[OptionValue[quiet] == False, Print["divisions = ",divisions]];

If[commutators[[2]] == 0,
If[OptionValue[quiet] == False,Print["Case 1E"]]; 
\[Rho]= Mult[Exp[MultSort[divisions[[1]]] t$sym],rho$sym[0]];
Return[\[Rho]]];
 
If[commutators[[3]] == 0,
If[OptionValue[quiet] == False,Print["Case 2E"]];
w = PowerExpand[Sqrt[-MultSort[divisions[[2]]]]];
If[OptionValue[quiet] == False,Print["w = ",w]];
\[Rho] = Mult[ Cos[w t$sym],rho$sym[0]] + Mult[ Sin[w t$sym], commutators[[1]]/w];
Return[\[Rho]]];

If[commutators[[4]] == 0,
If[OptionValue[quiet] == False,Print["Case 3E"]];
w = PowerExpand[Sqrt[-MultSort[divisions[[3]]]]];
If[OptionValue[quiet] == False,Print["w = ",w]];
\[Rho] = rho$sym[0] + Mult[ Sin[w t$sym], rho$sym[1]/w] + Mult[ 1-Cos[w t$sym], rho$sym[2]/w^2];
Return[\[Rho]]];

Message[Evolver::unsolvable];
Return[{rho$sym[#]& /@ {0,1,2,3},commutators,divisions}];
];

End[]

EndPackage[]

If[$VerboseLoad == True,
    Message[Evolve::usage]
    Message[Evolver::usage]
];




