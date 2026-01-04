(* ::Package:: *)

(** 
 ** Evolver2.m
 ** Unitary evolution of operators
 ** John Marohn
 ** 2025/12/26
 **)
 
BeginPackage["Evolver2`",{"Global`","OpQ`","Mult`","Comm`","Spins`","Osc`","Inv`","Evolve`"}]
 
Evolver2::usage="Evolver2[H, t, \[Rho](0)] calculates \[Rho](t) = Exp[-I H t] \[Rho](0) Exp[+I H t], assuming that H is time independent, according to the commutation rules followed by \[Rho](0) and H.";

Evolver2::unsolvable="Unrecognized evolution";

Begin["Private`"]

(*~ START ~*)

(*@
The \VerbFcn{Evolver2} function, by default, will not %
print out intermediate results during the computation.  %
@*)

Options[Evolver2] = {quiet -> True};

Evolver2[H$sym_, t$sym_, rho$sym$0_, opts : OptionsPattern[]] :=

Module[{rho$sym, divisions, commutators, \[Rho], w},

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

divisions ={Mult[Inv[rho$sym[0]],Inv[Inv[rho$sym[1]]]],
  Mult[Inv[rho$sym[0]],Inv[Inv[rho$sym[2]]]],
  Mult[Inv[rho$sym[1]],Inv[Inv[rho$sym[3]]]]};

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

Message[Evolver2::unsolvable];
Return[{rho$sym[#]& /@ {0,1,2,3},commutators,divisions}];
];

(*~ END ~*)

End[]

EndPackage[]

If[$VerboseLoad == True,
    Message[Evolver2::usage]
];







