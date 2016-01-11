(* ::Package:: *)

(**
 ** Evolve-tests.m
 ** Unitary evolution tests 
 ** John Marohn
 ** 2016/01/06
 **)

Off[SpinSingle$CreateOperators::comm]
Off[SpinSingle$CreateOperators::simplify]
Off[SpinSingle$CreateOperators::nocreate]
Off[OscSingle$CreateOperators::comm]
Off[OscSingle$CreateOperators::create]

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
For this test, we'll make three sets of two commuting operators representing, %
for example, three indepdendent harmonic oscillators. %
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
The first test is particularly important.  In test 03a and 03b below, nothing %
happens; the Hamiltonian is either so simple that it can be broken into pieces, 03a, %
or contains terms which do not commut, 03b.   In test 03c we have a Hamiltonian %
whose three terms commute, and in this case the \VerbFcn{Evolve} operator can be %
expanded. %
@*)

H0 = Q;
H1 = q Q**R + s S**U + U**S + V**V**W;
H2 = q Q +s S + v Q**S;

vtest["02a", AllCommutingQ[H0] === False]
vtest["02b", AllCommutingQ[H1] === False]
vtest["02c", AllCommutingQ[H2] === True]

vtest["03a", Evolve[H0,t,Q] === Evolve[Q,t,Q]]
vtest["03b", Evolve[H1,t,Q] === Evolve[q Q**R+s S**U+U**S+V**V**W,t,Q]]
vtest["03c", Evolve[H2,t,Q] === Evolve[q Q,t,Q]**Evolve[s S,t,Q]**Evolve[v Q**S,t,Q]]

Clear[H0,H1,H2]
Clear[Q,R,S,U,V,W,q,r,s,u,v,w]

(*@
To test the unitary evolution operator on spins, let us set up a model two-spin system.  % 
Ths system is comprised of an $L = 1/2$ $I$ spin and an unspecified-$L$ $S$ spin. %
@*)

Clear[Ix$sym, Iy$sym, Iz$sym, Sx$sym, Sy$sym, Sz$sym, \[Omega], d$sym, \[CapitalDelta], rho$sym, t$sym]

CreateScalar[\[Omega], d$sym, \[CapitalDelta]];
CreateOperator[{{Ix$sym, Iy$sym, Iz$sym},{Sx$sym, Sy$sym, Sz$sym}}]

SpinSingle$CreateOperators[Ix$sym, Iy$sym, Iz$sym, L=1/2];
SpinSingle$CreateOperators[Sx$sym, Sy$sym, Sz$sym, L=1/2];

(*@
Free evolution of $I_x$: 
@*)

vtest["04a > free evolution of Ix", 
  Evolver[\[Omega] Iz$sym, t$sym, Ix$sym] === Ix$sym Cos[\[Omega] t$sym] + Iy$sym Sin[\[Omega] t$sym]]

(*@
On-resonance nutation of $I_z$: 
@*)

vtest["04b > on-resonance nutation of Iz", 
  Evolver[\[Omega] Ix$sym, t$sym, Iz$sym] === Iz$sym Cos[\[Omega] t$sym] - Iy$sym Sin[\[Omega] t$sym]]

(*@
Free evolution of $I_{+}$:
@*)

vtest["04c > free evolution of I+", 
  Simplify[TrigToExp[Evolver[\[Omega] Iz$sym, t$sym, Ix$sym] + I Evolver[\[Omega] Iz$sym, t$sym, Iy$sym]]] 
  === Exp[-I \[Omega] t$sym](Ix$sym + I Iy$sym)]

(*@
Evolution under a scalar coupling:
@*)

vtest["04d > scalar-coupling evolution of Ix", 
  Simplify[Evolver[d$sym Iz$sym ** Sz$sym, t$sym, Ix$sym]] 
  === Ix$sym Cos[d$sym t$sym/2]+2 Iy$sym**Sz$sym Sin[d$sym t$sym/2]]

(*@
Off-resonance nutation of $I_z$.  It is important to carefully set the assumptions used by %
\VerbFcn{Simplify}.
@*)

$Assumptions = {Element[\[CapitalDelta], Reals], \[CapitalDelta] > 0, Element[\[Omega], Reals], \[Omega] >= 0};

constant1 = (\[CapitalDelta]^2)/(\[CapitalDelta]^2 + \[Omega]^2);
constant2 = (\[CapitalDelta] \[Omega])/(\[CapitalDelta]^2 + \[Omega]^2);
constant3 = (\[Omega]^2)/(\[CapitalDelta]^2 + \[Omega]^2);
constant4 = \[Omega]/Sqrt[\[CapitalDelta]^2 + \[Omega]^2];
omega$eff = Sqrt[\[CapitalDelta]^2 + \[Omega]^2];

rho$known = Collect[
  constant1 Iz$sym + constant2 Ix$sym + (constant3 Iz$sym - constant2 Ix$sym) Cos[omega$eff t$sym] - constant4 Iy$sym Sin[omega$eff t$sym] // 
  Expand, {Ix$sym, Iy$sym, Iz$sym}];

rho$calc = Collect[
  Evolver[\[CapitalDelta] Iz$sym + \[Omega] Ix$sym , t$sym, Iz$sym] // 
  Simplify // ExpToTrig // FullSimplify, {Ix$sym, Iy$sym, Iz$sym}]; 

vtest["04e > Off-resonance nutation of of Iz", rho$calc == rho$known]

(*@
Clean up:
@*)

Clear[Ix$sym, Iy$sym, Iz$sym, Sx$sym, Sy$sym, Sz$sym, \[Omega], d$sym, \[CapitalDelta], rho$sym, t$sym]

(*@
Harmonic oscillator evolution.  First, create the harmonic oscillator Hamiltonian in symmetric form.  Evolve % 
the lowering operator and confirm that it picks up the expected phase factor. % 
@*)

Clear[aL$sym, aR$sym, \[Omega], Q$sym, P$sym, H$sym, Q, P];
CreateScalar[\[Omega]];
OscSingle$CreateOperators[aL$sym, aR$sym];

H$sym = \[Omega] (aL$sym**aR$sym + aR$sym**aL$sym)/2;

vtest["05a > free evolution of a", 
  Evolver[H$sym, t$sym, aL$sym] === aL$sym Exp[I \[Omega] t$sym]]

(*@
Evolve the position operator.  To do this, write the position operator in terms of the raising and lowering % 
operators, calculate the evolution, and rewite the answer in terms of the position and momentum operator. %
For the re-write step, create a \emph{new} set of non-commuting operators, to avoid chasing our tail. %
@*)

CreateOperator[{{Q,P}}];
{Q$sym, P$sym} = {(aR$sym + aL$sym)/Sqrt[2], I (aR$sym - aL$sym)/Sqrt[2]};
QP$rules = {aR$sym -> (Q + I P)/Sqrt[2], aL$sym -> (Q - I P)/Sqrt[2]};

vtest["05b > free evolution of Q", 
  Simplify[Evolver[H$sym, t$sym, Q$sym] /. QP$rules] === Q Cos[\[Omega] t$sym] + P Sin[\[Omega] t$sym]]

(*@ Clean up: @*)

Clear[aL$sym, aR$sym, \[Omega], Q$sym, P$sym, H$sym, Q, P];

(*~ END ~*)

On[SpinSingle$CreateOperators::comm]
On[SpinSingle$CreateOperators::simplify]
On[SpinSingle$CreateOperators::nocreate]
On[OscSingle$CreateOperators::comm]
On[OscSingle$CreateOperators::create]



