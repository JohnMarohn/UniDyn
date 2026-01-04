(* ::Package:: *)

(**
 ** Evolver1-tests.m
 ** Unitary evolution tests 
 ** John Marohn
 ** 2016/01/06
 ** Updated 2026/01/03
 **)

Off[SpinSingle$CreateOperators::comm]
Off[SpinSingle$CreateOperators::simplify]
Off[SpinSingle$CreateOperators::nocreate]
Off[OscSingle$CreateOperators::comm]
Off[OscSingle$CreateOperators::create]

(*@
Create a shorthand function for creating unit tests.
@*)

If[$VersionNumber < 10.,

  vtest[label_,test_] :=
    If[test === True, 
      Print["Pass"],
      Print["Fail > ", StringJoin["Evolver1 > test",ToString[label]]]],

  vtest[label_,test_] := 
      VerificationTest[test,
          True,
          TestID-> StringJoin[
              "Evolver1 > test",
              ToString[label]]]
]

(*~ START ~*)

(*@
Global debugging flag to force \VerbFcn{Evolver1} to be noisy instead of quiet.
Set to False for noisy output and to True for quiet output.
@*)

quiet$query = True;

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

vtest["01 > DSolve test", rho$calc === rho$known]

(*@
Free evolution of $I_x$: 
@*)

vtest["02a > free evolution of Ix", 
  FullSimplify[ExpToTrig[Expand[
    Evolver1[\[Omega] Iz$sym, t$sym, Ix$sym, quiet -> quiet$query]]]]
  === Ix$sym Cos[\[Omega] t$sym] + Iy$sym Sin[\[Omega] t$sym]]

(*@
On-resonance nutation of $I_z$: 
@*)

vtest["02b > on-resonance nutation of Iz", 
  FullSimplify[ExpToTrig[Expand[
    Evolver1[\[Omega] Ix$sym, t$sym, Iz$sym, quiet -> quiet$query]]]]
  === Iz$sym Cos[\[Omega] t$sym] - Iy$sym Sin[\[Omega] t$sym]]

(*@
Free evolution of $I_{+}$:
@*)

vtest["02c > free evolution of I+", 
  FullSimplify[Evolver1[\[Omega] Iz$sym, t$sym, Ix$sym, quiet -> quiet$query] 
    + I Evolver1[\[Omega] Iz$sym, t$sym, Iy$sym, quiet -> quiet$query]]
  === Exp[-I \[Omega] t$sym](Ix$sym + I Iy$sym)]

(*@
Evolution under a scalar coupling:
@*)

vtest["02d > scalar-coupling evolution of Ix", 
  Evolver1[d$sym Mult[Iz$sym, Sz$sym], t$sym, Ix$sym, quiet -> quiet$query]
  === Ix$sym Cos[d$sym t$sym/2] + 2 Mult[Iy$sym, Sz$sym] Sin[d$sym t$sym/2]]

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
  Evolver1[\[CapitalDelta] Iz$sym + \[Omega] Ix$sym , t$sym, Iz$sym, quiet -> quiet$query] 
  // FullSimplify, {Ix$sym, Iy$sym, Iz$sym}, Expand]; 

vtest["05e > Off-resonance nutation of of Iz", rho$calc === rho$known]

(*@
Clean up:
@*)

Clear[Ix$sym, Iy$sym, Iz$sym, Sx$sym, Sy$sym, Sz$sym, \[Omega], d$sym, \[CapitalDelta], rho$sym, t$sym]
Clear[A, r, lhs$list, rhs$list, time, eqns, system, rho$calc, rho$known, X]

(*@
Harmonic oscillator evolution.  First, create the harmonic oscillator Hamiltonian in symmetric form.  Evolve % 
the lowering operator and confirm that it picks up the expected phase factor. Evolve the raising operator %
and confirm that it picks up the expected (conjugate) phase factor. % 
@*)

Clear[aL$sym, aR$sym, \[Omega], Q$sym, P$sym, H$sym, Q, P, delta$q$sym, delta$p$sym];
CreateScalar[\[Omega], delta$x$sym, delta$p$sym];
OscSingle$CreateOperators[aL$sym, aR$sym];

H$sym = \[Omega] (Mult[aL$sym, aR$sym] + Mult[aR$sym, aL$sym])/2;

vtest["03a1 > free evolution of lowering operator", 
  Simplify[TrigToExp[Expand[Evolver1[H$sym, t$sym, aL$sym, quiet -> quiet$query]]]]
    === aL$sym Exp[I \[Omega] t$sym]]

vtest["03a2 > free evolution of raising operator", 
  Simplify[TrigToExp[Expand[Evolver1[H$sym, t$sym, aR$sym, quiet -> quiet$query]]]]
    === aR$sym Exp[-I \[Omega] t$sym]]

(*@
Evolve the position operator.  To do this, write the position operator in terms of the raising and lowering % 
operators, calculate the evolution, and rewite the answer in terms of the position and momentum operator. %
For the re-write step, create a \emph{new} set of non-commuting operators, to avoid chasing our tail. %
@*)

CreateOperator[{{Q,P}}];
{Q$sym, P$sym} = {(aR$sym + aL$sym)/Sqrt[2], I (aR$sym - aL$sym)/Sqrt[2]};
QP$rules = {aR$sym -> (Q - I P)/Sqrt[2], aL$sym -> (Q + I P)/Sqrt[2]};

(*@
Write Q and P in terms of raising and lower operators, write the raising and lowering % 
operators in terms of position and momentum, and you should get the original %
Q and P back again.
@*)

vtest["03b > test Q definition", Simplify[Q$sym /. QP$rules] === Q]
vtest["03c > test P definition", Simplify[P$sym /. QP$rules] === P]

(*@
Now we are ready to evolve position and momentum.
@*)

vtest["03d > free evolution of Q", 
  Simplify[ExpToTrig[Expand[Evolver1[H$sym, t$sym, Q$sym, quiet -> quiet$query] /. QP$rules]]]
    == Q Cos[\[Omega] t$sym] - P Sin[\[Omega] t$sym]]

vtest["03e > free evolution of P", 
  Simplify[ExpToTrig[Expand[Evolver1[H$sym, t$sym, P$sym, quiet -> quiet$query] /. QP$rules]]]
    == P Cos[\[Omega] t$sym] + Q Sin[\[Omega] t$sym]]

(*@
Check that the operator $e^{-i \delta q \, P}$ delivers a position kick and that %
the operator $e^{-i \delta p \, X}$ delivers a momentum kick.  These are examples %
of evolution where the commuting series terminates.  %
@*)

vtest["03f > position kick",
	Simplify[Evolver1[delta$q$sym P$sym, t, Q$sym] /. QP$rules ~Join~ {t-> 1}] 
	 == Q - delta$q$sym]

vtest["03g > momentum kick",
	Simplify[Evolver1[delta$p$sym Q$sym, t, P$sym] /. QP$rules ~Join~ {t-> 1}] 
	 == P + delta$p$sym]

(*@ Clean up: @*)

Clear[aL$sym, aR$sym, \[Omega], Q$sym, P$sym, H$sym, Q, P, delta$q$sym, delta$p$sym];

(*~ END ~*)

On[SpinSingle$CreateOperators::comm]
On[SpinSingle$CreateOperators::simplify]
On[SpinSingle$CreateOperators::nocreate]
On[OscSingle$CreateOperators::comm]
On[OscSingle$CreateOperators::create]






