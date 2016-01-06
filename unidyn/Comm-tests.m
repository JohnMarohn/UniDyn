(* ::Package:: *)

(**
 ** Comm-tests.m
 ** Commutator algebra unit tests  
 ** John Marohn
 ** 2016/01/06
 **)

vtest[label_,test_] := 
    VerificationTest[test,
        True,
        TestID-> StringJoin[
            "OpQ > test",
            ToString[label]]]

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

vtest["02a", Comm[a$sym Sx$sym, b$sym Sy$sym] === a$sym b$sym Comm[Sx$sym, Sy$sym]]

(*@
Test the $[A B, C]$ and $[A, B C]$ expansion rules, adding in scalars to both positions. % 
In test 03b, we see that the $S_x$ operator in the second place on the commutator can % 
be pulled out front since it commutes with the operator in the first place of commutator.  %
@*)

vtest["03a", Comm[(a$sym Sx$sym)**Sy$sym, b$sym Sz$sym] === a$sym b$sym (Sx$sym**Comm[Sy$sym, Sz$sym] + Comm[Sx$sym, Sz$sym]**Sy$sym)]
vtest["03b", Comm[a$sym Sx$sym, b$sym Sy$sym**Sz$sym] === a$sym b$sym (Sy$sym**Comm[Sx$sym, Sz$sym] + Comm[Sx$sym, Sy$sym]**Sz$sym)]
vtest["03c", Comm[Sx$sym, Sx$sym**Sy$sym] === Sx$sym**Comm[Sx$sym, Sy$sym]]

(*@
Test the Jacobi identity.  For this identity to resolve to zero, we must tell %
\emph{Mathematica} that [A,B] equals A**B-B**A.  First, check that this substitution does %
what we expect, then test the Jacobi identity. %  
@*)

vtest["04a", (Comm[Ix$sym, Iy$sym] //. Comm[Sx$sym_, Sy$sym_] -> Sx$sym**Sy$sym - Sy$sym**Sx$sym) 
	=== Ix$sym**Iy$sym - Iy$sym**Ix$sym]

vtest["04b", NCExpand[((Comm[Ix$sym, Comm[Iy$sym, Iz$sym]] + 
    Comm[Iy$sym, Comm[Iz$sym, Ix$sym]] + Comm[Iz$sym, Comm[Ix$sym, Iy$sym]])
	//. Comm[Sx$sym_, Sy$sym_] -> Sx$sym**Sy$sym - Sy$sym**Sx$sym)]
	=== 0] 

(*@
For other Commutator identities, see % 
\href{https://en.wikipedia.org/wiki/Commutator}{https://en.wikipedia.org/wiki/Commutator}. %
Let us test one of these advanced identities.
@*)

CreateOperator[{{A$sym, B$sym, C$sym, D$sym}}];

vtest["05a", NCExpand[Comm[A$sym**B$sym**C$sym, D$sym]]
	=== A$sym**B$sym**Comm[C$sym, D$sym] + A$sym**Comm[B$sym, D$sym]**C$sym + Comm[A$sym, D$sym]**B$sym**C$sym]

(*~ END ~*)

Clear[a$sym, b$sym, c$sym, d$sym];
Clear[Ix$sym, Iy$sym, Iz$sym, Sx$sym, Sy$sym, Sz$sym];
Clear[A$sym, B$sym, C$sym, D$sym];



