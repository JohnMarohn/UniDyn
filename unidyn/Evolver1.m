(* ::Package:: *)

(** 
 ** Evolver1.m
 ** Unitary evolution of operators
 ** John Marohn
 ** Updated 2026/01/03
 **)

BeginPackage["Evolver1`",{"Global`","OpQ`","Mult`","Comm`","Spins`","Osc`","Inv`","Evolve`"}]

Evolver1::usage="Evolver1[H, t, \[Rho](0)] calculates \[Rho](t) = Exp[-I H t] \[Rho](0) Exp[+I H t], assuming that H is time independent, according to the commutation rules followed by \[Rho](0) and H."

Evolver1::unsolvable="Unrecognized evolution";

Begin["Private`"]

(*~ START ~*)

(*@
The \VerbFcn{Evolver1} function, by default, will not %
print out intermediate results during the computation.  %
@*)

Options[Evolver1] = {quiet -> True};

Evolver1[H$sym_, t$sym_, rho$sym$0_, opts : OptionsPattern[]] :=

Module[{k, a$vect, q, r, r$value, X, x4, x3, x2, x1, time, system, sol},

  Clear[rho$sym];
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

(*@ Print out the vector of density-operator derivatives if asked. @*)

  If[OptionValue[quiet] == False, Print["\[Rho] matrix = ", rho$sym[#]& /@ {0,1,2,3,4} // MatrixForm]];

(*@ Look for an entry in the $(\rho^{(2)}, \rho^{(1)}, \rho^{(0)})$ list that is % 
proportional to $\rho^{(3)}$. Stop when you find it.  Determining %
\emph{proportional to} is tricky.  Here we use the ability of the \verb+NCALgebra+ %
package to compute a symbolic inverse of an operator.  When %
$(\rho^{(n)})^{-1}**\rho^{(3)}$ is a scalar, then we have found a match. % 
We are implicity assuming that the entries $(\rho^{(2)}, \rho^{(1)}, \rho^{(0)})$ %
\emph{have} an inverse.  This will be true of then entries involve Hermitian %
operators.  If the entries involve \emph{non-Hermitian operators}, however, like %
$I_{+}$ or $I_{-}$ then the entries might not have a proper inverse.  % 
We do not, at present, test whether the operators in the list are Hermitian or not. % 
@*) 

  a$vect={0,0,0,0};
  r = Null;
  r = Catch[
    Do[
      q = Mult[
         Divide[rho$sym[3],Mult[rho$sym[k]]]
       ] // FullSimplify[#, ComplexityFunction->VisualComplexity]& ;

      If[ScalarQ[q]==True, Throw[{3-k, q}]],
      {k,0,2}
    ]
  ];

(*@
If we have not found a match by now, then throw up our hands and exit the function. %
Spit out the full $(\rho^{(3)}, \rho^{(2)}, \rho^{(1)}, \rho^{(0)})$ vector, %
in case the user can spot the \emph{proportional to} condition.
@*)

  If[r=== Null,
    Message[Evolver1::unsolvable];
    Return[rho$sym[#]& /@ {0,1,2,3,4}],
    r$value=r
  ];

(*@ 
Set up the coupling matrix $\bm{\Omega}$ based on the matching condition. %
@*)

  a$vect[[r$value[[1]]]] = r$value[[2]];
  A = {a$vect,{1, 0 ,0, 0},{0, 1, 0, 0}, {0, 0 ,1, 0}};

(*@
If asked, spit out the coupling matrix for inspection. %
@*)

  If[OptionValue[quiet] == False, Print["\[CapitalOmega] = ", A // MatrixForm]];

(*@
Set up the four coupled equations and solve them.  Respect \emph{Mathematica} %
version 8 standards here and feed \VerbFcn{DSolve[]} a list where each element %
of the list is an equation.  First set up the equations. % 
@*)

  X[time_] = {x4[time], x3[time], x2[time], x1[time]};
  lhs$list = D[X[time],time];
  rhs$list = A . X[time];
  eqns = (lhs$list[[#]] == rhs$list[[#]])& /@ {1,2,3,4};
 
  system = {eqns,
    x4[0]== rho$sym[3], x3[0]== rho$sym[2], x2[0]== rho$sym[1], x1[0]== rho$sym[0]};

  If[OptionValue[quiet] == False, Print["system of equations = ", system // MatrixForm]];

(*@
Now solve them and print out the solution with the private variables replaced %
by public ones. %
@*)  

  sol = DSolve[system,{x1,x2,x3,x4},time];

  If[OptionValue[quiet] == False, Print["1st solution = ", sol[[1]][[1]] ]];
  If[OptionValue[quiet] == False, Print["1st solution w/ substitution = ", x1[time]  /. sol[[1]][[1]] /. time -> t$sym ]];

(*@ 
Return only the first element of the solution, $\lambda_1(t)$.  
@*)

  Return[FullSimplify[x1[time]  /. sol[[1]] /. time -> t$sym]];
];

(*~ END ~*)

End[]

EndPackage[]

If[$VerboseLoad == True,
    Message[Evolver1::usage]
]




