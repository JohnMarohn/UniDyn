(* ::Package:: *)

(**
 ** Evolve-tests.m
 ** Unitary evolution tests 
 ** John Marohn
 ** 2016/01/06
 **)

(*@
Create a shorthand function for creating unit tests.
@*)

vtest[label_,test_] := 
    VerificationTest[test,
        True,
        TestID-> StringJoin[
            "Osc > test",
            ToString[label]]]

(*~ START ~*)

(*@
Show that the \VerbFcn{Evolve} operator is distributive over both sums %
and non-commutative products of operators.  Show that scalars are pulled %
out front, even if buried in a oomplicated product of operators and scalars. %
@*)

Clear[H,t,H1,H2,H3,Q,R,S,U,V,W,q,r,s,u,v,w];

CreateOperator[{{Q,R},{S,U},{V,W}}]
CreateScalar[{q,r,s,u,v,w}]

vtest["01a", Evolve[H, t, Q + R + S] === Evolve[H,t,Q]+Evolve[H,t,R]+Evolve[H,t,S]]
vtest["01b", Evolve[H, t, Q**R**S] === Evolve[H,t,Q]**Evolve[H,t,R]**Evolve[H,t,S]]
vtest["01c", Evolve[H,t,(Q q)**(r R)**(s S) + u U]
    === u Evolve[H,t,U] + q r s Evolve[H,t,Q]**Evolve[H,t,R]**Evolve[H,t,S]]

(*@
Make up some Hamiltonians and see if they pass the all-terms-commuting test.  %
The first test is particularly important.
@*)

H0 = Q;
H1 = q Q**R + s S**U + U**S + V**V**W;
H2 = q Q +s S + v Q**S;

vtest["02a", AllCommutingQ[H0] === False]
vtest["02b", AllCommutingQ[H1] === False]
vtest["02c", AllCommutingQ[H2] === True]

vtest["03a", Evolve[H0,t,Q] === Evolve[Q,t,Q]]  (* nothing happens *)
vtest["03b", Evolve[H1,t,Q] === Evolve[q Q**R+s S**U+U**S+V**V**W,t,Q]]  (* again, nothing *)
vtest["03c", Evolve[H2,t,Q] === Evolve[q Q,t,Q]**Evolve[s S,t,Q]**Evolve[v Q**S,t,Q]]

Clear[H0,H1,H2]
Clear[Q,R,S,U,V,W,q,r,s,u,v,w]

(*~ END ~*)



