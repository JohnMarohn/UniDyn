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

Module[{rho$sym, divisions, commutators, rho$calc, omega$sym},

  rho$sym[0] = rho$sym$0;

(*@ 
Evalute the derivatives of the density operator $\rho^{(n)}$ by repeatedly applying the %
commutator. Simplify the derivatives as much as possible. Getting the simplication right %
is tricky; a special function has to be fed to \VerbFcn{FullSimplify} to get %
it to return useful results. %
@*)

  Do[
	rho$sym[k+1] = (-I Comm[H$sym, rho$sym[k]] /. Mult -> SortedMult)
	      // FullSimplify[#, ComplexityFunction->VisualComplexity]&,
    {k,0,4}
  ];

If[OptionValue[quiet] == False, 
Print["\!\(\*SuperscriptBox[\(\[Rho]\), \((n)\)]\)= ", rho$sym[#]& /@ {0,1,2,3}]];

(*@
Work through cases 0 and 1L.
@*)

If[rho$sym[1] == 0, 
  If[OptionValue[quiet] == False, Print["Case 0"]];
  Return[rho$sym[0]]
];

If[Comm[rho$sym[1],H$sym ] == 0,
  If[OptionValue[quiet] == False, Print["Case 1L"]];
  Return[rho$sym[0] + rho$sym[1] t$sym]
];

(*@
Compute the commutators $c_n$. 
@*)

commutators = {
  rho$sym[1], 
  Comm[rho$sym[0], rho$sym[1]],
  Comm[rho$sym[0], rho$sym[2]],
  Comm[rho$sym[1], rho$sym[3]]
};

If[OptionValue[quiet] == False, Print["commutators = ",commutators]];

(*@
Compute the ratios $d_n$. 
@*)

divisions = {
  Mult[Inv[rho$sym[0]],Inv[Inv[rho$sym[1]]]],
  Mult[Inv[rho$sym[0]],Inv[Inv[rho$sym[2]]]],
  Mult[Inv[rho$sym[1]],Inv[Inv[rho$sym[3]]]]
};

If[OptionValue[quiet] == False, Print["divisions = ",divisions]];

(*@
Work through cases 1E, 2E, and 3E.
@*)

If[commutators[[2]] == 0,
  If[OptionValue[quiet] == False, Print["Case 1E"]]; 
  
  rho$calc = Mult[Exp[MultSort[divisions[[1]]] t$sym], rho$sym[0]];
  Return[rho$calc]
];
 
If[commutators[[3]] == 0,
  If[OptionValue[quiet] == False, Print["Case 2E"]];
  
  omega$sym = PowerExpand[Sqrt[-MultSort[divisions[[2]]]]];
  If[OptionValue[quiet] == False, Print["\[Omega] = ", omega$sym]];
  
  rho$calc = Mult[Cos[omega$sym t$sym], rho$sym[0]] + Mult[Sin[omega$sym t$sym], commutators[[1]]/omega$sym];
  Return[rho$calc]
];

If[commutators[[4]] == 0,
  If[OptionValue[quiet] == False, Print["Case 3E"]];
  
  omega$sym = PowerExpand[Sqrt[-MultSort[divisions[[3]]]]];
  If[OptionValue[quiet] == False, Print["\[Omega] = ", omega$sym]];
  
  rho$calc = rho$sym[0] + Mult[Sin[omega$sym t$sym], rho$sym[1]/omega$sym] + Mult[1 - Cos[omega$sym t$sym], rho$sym[2]/omega$sym^2];
  Return[rho$calc]
];

(*@
If we get this far, then the evolution is unsolvable. In this case, %
return the derivatives of the density operator $\rho^{(n)}$ for debugging purposes. %
@*)

Message[Evolver2::unsolvable];
Return[{rho$sym[#]& /@ {0,1,2,3},commutators,divisions}];
];

(*~ END ~*)

End[]

EndPackage[]

If[$VerboseLoad == True,
    Message[Evolver2::usage]
];













