(* ::Package:: *)

(**
 ** Evolve-tests.m
 ** Unitary evolution tests 
 ** John Marohn
 ** 2026/01/03
 **)
 
(*@
Create a shorthand function for creating unit tests.
@*)

If[$VersionNumber < 10.,

  vtest[label_,test_] :=
    If[test === True, 
      Print["Pass"],
      Print["Fail > ", StringJoin["Osc > test",ToString[label]]]],

  vtest[label_,test_] := 
      VerificationTest[test,
          True,
          TestID-> StringJoin[
              "Osc > test",
              ToString[label]]]
]

(*~ START ~*)

(*@
Show that the \VerbFcn{Evolve} operator is distributive over both sums %
and non-commutative products of operators.  Show that scalars are pulled %
out front, even if buried in a oomplicated product of operators and scalars. %
For this test, we'll make three sets of two commuting operators representing, %
for example, three indepdendent harmonic oscillators. %
@*)

Clear[H, t, H1, H2, H3, Q, R, S, U, V, W, q, r, s, u, v, w];

CreateOperator[{{Q,R},{S,U},{V,W}}]
CreateScalar[{q,r,s,u,v,w}]

vtest["01a > distribute addition", 
	Evolve[H, t, Q + R + S] 
	=== Evolve[H, t, Q] + Evolve[H, t, R] + Evolve[H, t, S]]
vtest["01b > distribute multiplication", 
	Evolve[H, t, Mult[Q, R, S]] 
	=== Mult[Evolve[H, t, Q], Evolve[H, t, R], Evolve[H, t, S]]]
vtest["01c > distribute complicated expression", 
	Evolve[H, t, Mult[(Q q), (r R), (s S)] + u U]
    === u Evolve[H, t, U] + q r s Mult[Evolve[H, t, Q], Evolve[H, t, R], Evolve[H, t, S]]]

(*@
Make up some Hamiltonians and see if they pass the all-terms-commuting test.  %
The first test is particularly important.  In test 03a and 03b below, nothing %
happens; the Hamiltonian is either so simple that it can be broken into pieces, 03a, %
or contains terms which do not commut, 03b.   In test 03c we have a Hamiltonian %
whose three terms commute, and in this case the \VerbFcn{Evolve} operator can be %
expanded. %
@*)

H0 = Q;
H1 = q Mult[Q, R] + s Mult[S, U] + Mult[U, S] + Mult[V, V, W];
H2 = q Q + s S + v Mult[Q, S];

vtest["02a > commuting test 1", AllCommutingQ[H0] === False]
vtest["02b > commuting test 2", AllCommutingQ[H1] === False]
vtest["02c > commuting test 3", AllCommutingQ[H2] === True]

vtest["03a > Evolve expand test 1", Evolve[H0, t, Q] === Evolve[Q, t, Q]]
vtest["03b > Evolve expand test 2", Evolve[H1, t, Q] 
	=== Evolve[q Mult[Q,R] + s Mult[S,U] + Mult[U, S] + Mult[V, V, W], t, Q]]
vtest["03c > Evolve expand test 3", Evolve[H2, t, Q] 
	=== Mult[Evolve[q Q, t, Q], Evolve[s S, t, Q], Evolve[v Mult[Q,S], t, Q]]]

Clear[H0, H1, H2]
Clear[Q, R, S, U, V, W, q, r, s, u, v, w]





