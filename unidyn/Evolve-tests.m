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
Global debugging flag to force \VerbFcn{Evolver} to be noisy instead of quiet.
@*)

quiet$query = True;

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
Clear[A, r, lhs$list, rhs$list, time, eqns, system, rho$calc, rho$known, X]

CreateScalar[\[Omega], d$sym, \[CapitalDelta]];
CreateOperator[{{Ix$sym, Iy$sym, Iz$sym},{Sx$sym, Sy$sym, Sz$sym}}]

SpinSingle$CreateOperators[Ix$sym, Iy$sym, Iz$sym, L=1/2];
SpinSingle$CreateOperators[Sx$sym, Sy$sym, Sz$sym, L=1/2];

(*@
Test the differential equation solver first.  In \emph{Mathematica} version 10 %
gives more leeway in how the equations are set up -- you can set one list %
equal to another, for example.  In \emph{Mathematica} version 8, in contrast %
the syntax is not so forgiving.  Let us mock-up an equation by hand and feed %
it to the solver. %
@*)

A = {{0,-\[CapitalDelta]^2,0,0},{1,0,0,0},{0,1,0,0},{0,0,1,0}};
r = {Ix$sym, Iy$sym \[CapitalDelta], -Ix$sym \[CapitalDelta]^2, -Iy$sym \[CapitalDelta]^3, Ix$sym \[CapitalDelta]^4};
(rho$sym[#-1] = r[[#]])& /@ {1,2,3,4,5}; 

X[time_] = {x4[time], x3[time], x2[time], x1[time]};
lhs$list = D[X[time],time];
rhs$list = A . X[time];
eqns = (lhs$list[[#]] == rhs$list[[#]])& /@ {1,2,3,4}; 

system = {eqns,
  x4[0]== rho$sym[3], x3[0]== rho$sym[2], x2[0]== rho$sym[1], x1[0]== rho$sym[0]};
  sol = DSolve[system,{x1,x2,x3,x4},time];

rho$calc = (x1[time] /. sol[[1]] /. time -> t$sym) // Expand // ExpToTrig // FullSimplify;
rho$known = Ix$sym Cos[t$sym \[CapitalDelta]] + Iy$sym Sin[t$sym \[CapitalDelta]];

vtest["04a > DSolve test", rho$calc === rho$known]

(*@
Free evolution of $I_x$: 
@*)

vtest["05a > free evolution of Ix", 
  FullSimplify[ExpToTrig[Expand[
    Evolver[\[Omega] Iz$sym, t$sym, Ix$sym, quiet -> quiet$query]]]]
  === Ix$sym Cos[\[Omega] t$sym] + Iy$sym Sin[\[Omega] t$sym]]

(*@
On-resonance nutation of $I_z$: 
@*)

vtest["05b > on-resonance nutation of Iz", 
  FullSimplify[ExpToTrig[Expand[
    Evolver[\[Omega] Ix$sym, t$sym, Iz$sym, quiet -> quiet$query]]]]
  === Iz$sym Cos[\[Omega] t$sym] - Iy$sym Sin[\[Omega] t$sym]]

(*@
Free evolution of $I_{+}$:
@*)

vtest["05c > free evolution of I+", 
  FullSimplify[Expand[Evolver[\[Omega] Iz$sym, t$sym, Ix$sym, quiet -> quiet$query] 
    + I Evolver[\[Omega] Iz$sym, t$sym, Iy$sym, quiet -> quiet$query]]]
  === Exp[-I \[Omega] t$sym](Ix$sym + I Iy$sym)]

(*@
Evolution under a scalar coupling:
@*)

vtest["05d > scalar-coupling evolution of Ix", 
  Expand[ExpToTrig[FullSimplify[
    Evolver[d$sym Iz$sym ** Sz$sym, t$sym, Ix$sym, quiet -> quiet$query]]]]
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
  Evolver[\[CapitalDelta] Iz$sym + \[Omega] Ix$sym , t$sym, Iz$sym, quiet -> quiet$query] // 
    Simplify // ExpToTrig // FullSimplify, {Ix$sym, Iy$sym, Iz$sym}]; 

vtest["05e > Off-resonance nutation of of Iz", rho$calc == rho$known]

(*@
Clean up:
@*)

Clear[Ix$sym, Iy$sym, Iz$sym, Sx$sym, Sy$sym, Sz$sym, \[Omega], d$sym, \[CapitalDelta], rho$sym, t$sym]
Clear[A, r, lhs$list, rhs$list, time, eqns, system, rho$calc, rho$known, X]

(*@
Harmonic oscillator evolution.  First, create the harmonic oscillator Hamiltonian in symmetric form.  Evolve % 
the lowering operator and confirm that it picks up the expected phase factor. % 
@*)

Clear[aL$sym, aR$sym, \[Omega], Q$sym, P$sym, H$sym, Q, P];
CreateScalar[\[Omega]];
OscSingle$CreateOperators[aL$sym, aR$sym];

H$sym = \[Omega] (aL$sym**aR$sym + aR$sym**aL$sym)/2;

vtest["06a > free evolution of a", 
  Simplify[TrigToExp[Expand[Evolver[H$sym, t$sym, aL$sym, quiet -> quiet$query]]]]
    === aL$sym Exp[I \[Omega] t$sym]]

(*@
Evolve the position operator.  To do this, write the position operator in terms of the raising and lowering % 
operators, calculate the evolution, and rewite the answer in terms of the position and momentum operator. %
For the re-write step, create a \emph{new} set of non-commuting operators, to avoid chasing our tail. %
@*)

CreateOperator[{{Q,P}}];
{Q$sym, P$sym} = {(aR$sym + aL$sym)/Sqrt[2], I (aR$sym - aL$sym)/Sqrt[2]};
QP$rules = {aR$sym -> (Q + I P)/Sqrt[2], aL$sym -> (Q - I P)/Sqrt[2]};

vtest["06b > free evolution of Q", 
  Simplify[ExpToTrig[Expand[Evolver[H$sym, t$sym, Q$sym, quiet -> quiet$query] /. QP$rules]]]
    == Q Cos[\[Omega] t$sym] + P Sin[\[Omega] t$sym]]

(*@ Clean up: @*)

Clear[aL$sym, aR$sym, \[Omega], Q$sym, P$sym, H$sym, Q, P];

(*~ END ~*)

On[SpinSingle$CreateOperators::comm]
On[SpinSingle$CreateOperators::simplify]
On[SpinSingle$CreateOperators::nocreate]
On[OscSingle$CreateOperators::comm]
On[OscSingle$CreateOperators::create]
