(* ::Package:: *)

(**
 ** Comm-tests.m
 ** Commutator algebra unit tests  
 ** John Marohn
 ** 2016/01/06
 **)

If[$VersionNumber < 10.,

  vtest[label_,test_] :=
    If[test === True, 
      Print["Pass"],
      Print["Fail > ", StringJoin["Comm > test",ToString[label]]]],

  vtest[label_,test_] := 
      VerificationTest[test,
          True,
          TestID-> StringJoin[
              "Comm > test",
              ToString[label]]]
]


Clear[a$sym, b$sym, c$sym, d$sym];
Clear[Ix$sym, Iy$sym, Iz$sym, Sx$sym, Sy$sym, Sz$sym];
Clear[A$sym, B$sym, C$sym, D$sym];

(*~ START ~*)

CreateScalar[{a$sym, b$sym, c$sym, d$sym}];
CreateOperator[{{Ix$sym, Iy$sym, Iz$sym},{Sx$sym, Sy$sym, Sz$sym}}];

(*@
The commutator is distributive.  The commutator of a scalar with an operator % 
is zero.  The commutator of an operator with iteslf is zero.  The commutator % 
between two operators of different phyla is zero.  The commutator of two operators % 
of the same phyla is left unevaluated. %
@*)

vtest["01a", Comm[Ix$sym, Iy$sym + Iz$sym] === Comm[Ix$sym, Iy$sym] + Comm[Ix$sym, Iz$sym]]
vtest["01b", Comm[a$sym, Ix$sym] === 0]
vtest["01c", Comm[Ix$sym, Ix$sym] === 0]
vtest["01d", Comm[Ix$sym, Sy$sym] === 0]
vtest["01e", Comm[Sx$sym, Sy$sym] === Comm[Sx$sym, Sy$sym]]

(*@
Scalars are properly factored out of commutators.
@*)

vtest["02a", Comm[a$sym Sx$sym, Sy$sym] === a$sym Comm[Sx$sym, Sy$sym]]
vtest["02b", Comm[Sx$sym, b$sym Sy$sym] === b$sym Comm[Sx$sym, Sy$sym]]
vtest["02c", Comm[a$sym Sx$sym, b$sym Sy$sym] === a$sym b$sym Comm[Sx$sym, Sy$sym]]

(*@
Test the $[A B, C]$ and $[A, B C]$ expansion rules, adding in scalars to both positions. % 
In test 03b, we see that the $S_x$ operator in the second place on the commutator can % 
be pulled out front since it commutes with the operator in the first place of commutator.  %
@*)

vtest["03a", Comm[a$sym Mult[Sx$sym, Sy$sym], b$sym Sz$sym] 
	=== a$sym b$sym (Mult[Sx$sym, Comm[Sy$sym, Sz$sym]] + Mult[Comm[Sx$sym, Sz$sym], Sy$sym])]
vtest["03b", Comm[a$sym Sx$sym, b$sym Mult[Sy$sym, Sz$sym]] 
	=== a$sym b$sym (Mult[Sy$sym, Comm[Sx$sym, Sz$sym]] + Mult[Comm[Sx$sym, Sy$sym], Sz$sym])]
vtest["03c", Comm[Sx$sym, Mult[Sx$sym, Sy$sym]] 
	=== Mult[Sx$sym, Comm[Sx$sym, Sy$sym]]]

(*@
Test the Jacobi identity.  For this identity to resolve to zero, we must tell %
\emph{Mathematica} that [A,B] equals A**B-B**A.  First, check that this substitution does %
what we expect, then test the Jacobi identity. %  
@*)

vtest["04a", (Comm[Ix$sym, Iy$sym] //. Comm[Sx$sym_, Sy$sym_] -> Mult[Sx$sym, Sy$sym] - Mult[Sy$sym, Sx$sym]) 
	=== Mult[Ix$sym, Iy$sym] - Mult[Iy$sym, Ix$sym]]

vtest["04b", ((Comm[Ix$sym, Comm[Iy$sym, Iz$sym]] + 
    Comm[Iy$sym,  Comm[Iz$sym, Ix$sym]] + Comm[Iz$sym, Comm[Ix$sym, Iy$sym]])
	//. Comm[Sx$sym_, Sy$sym_] -> Mult[Sx$sym, Sy$sym] - Mult[Sy$sym, Sx$sym])
	=== 0] 

(*@
For other Commutator identities, see % 
\href{https://en.wikipedia.org/wiki/Commutator}{https://en.wikipedia.org/wiki/Commutator}. %
Let us test one of these advanced identities.
@*)

CreateOperator[{{A$sym, B$sym, C$sym, D$sym}}];

vtest["05a", Comm[Mult[A$sym, B$sym, C$sym], D$sym]
	=== Mult[A$sym, B$sym, Comm[C$sym, D$sym]] 
	+ Mult[A$sym, Comm[B$sym, D$sym], C$sym] + Mult[Comm[A$sym, D$sym], B$sym, C$sym]]

(*~ END ~*)

Clear[a$sym, b$sym, c$sym, d$sym];
Clear[Ix$sym, Iy$sym, Iz$sym, Sx$sym, Sy$sym, Sz$sym];
Clear[A$sym, B$sym, C$sym, D$sym];















