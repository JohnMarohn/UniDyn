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
whose three terms commute, and in this case the \VerbFcn{Evolve} operator an be %
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

Clear[Ix$sym, Iy$sym, Iz$sym, Sx$sym, Sy$sym, Sz$sym, omega$sym, d$sym, delta$sym,  theta$sym, phi$sym, rho$sym, t$sym]

CreateScalar[omega$sym, d$sym, delta$sym, theta$sym, phi$sym];
CreateOperator[{{Ix$sym, Iy$sym, Iz$sym},{Sx$sym, Sy$sym, Sz$sym}}]

SpinSingle$CreateOperators[Ix$sym, Iy$sym, Iz$sym, L=1/2];
SpinSingle$CreateOperators[Sx$sym, Sy$sym, Sz$sym, L=1/2];

(*@
Free evolution of $I_x$: 
@*)

vtest["04a > free evolution of Ix", 
  Evolver[omega$sym Iz$sym, t$sym, Ix$sym] === Ix$sym Cos[omega$sym t$sym] + Iy$sym Sin[omega$sym t$sym]]

(*@
On-resonance nutation of $I_z$: 
@*)

vtest["04b > on-resonance nutation of Iz", 
  Evolver[omega$sym Ix$sym, t$sym, Iz$sym] === Iz$sym Cos[omega$sym t$sym] - Iy$sym Sin[omega$sym t$sym]]

(*@
Free evolution of $I_{+}$:
@*)

vtest["04c > free evolution of I+", 
  Simplify[TrigToExp[
    Evolver[omega$sym Iz$sym, t$sym, Ix$sym] + I Evolver[omega$sym Iz$sym, t$sym, Iy$sym]
  ]] 
  === Exp[-I omega$sym t$sym](Ix$sym + I Iy$sym)]

(*@
Evolution under a scalar coupling:
@*)

vtest["04d > scalar-coupling evolution of Ix", 
  Simplify[Evolver[d$sym Iz$sym ** Sz$sym, t$sym, Ix$sym]] === Ix$sym Cos[d$sym t$sym/2]+2 Iy$sym**Sz$sym Sin[d$sym t$sym/2]]

(*@
Off-resonance nutation of $I_z$ --- still needs a check:
@*)

\[Rho] = Collect[
  (Evolver[delta$sym Iz$sym + omega$sym Ix$sym , t$sym, Iz$sym] 
    // Simplify[#, Assumptions->{Element[delta$sym, Reals], delta$sym > 0, Element[omega$sym, Reals], omega$sym >= 0}]&  
    // ExpToTrig // FullSimplify), {Ix$sym, Iy$sym, Iz$sym}]; 

\[Rho] /. {t$sym -> t, 
  delta$sym -> Subscript[\[Omega],0],
  omega$sym -> Subscript[\[Omega],1],
  Ix$sym -> Ix, Iy$sym -> Iy, Iz$sym -> Iz}


(*@
Off-resonance variable-phase nutation of $I_z$ -- still needs a check:
@*)

\[Rho] = Collect[ 
  (Evolver[delta$sym Iz$sym + omega$sym (Cos[phi$sym] Ix$sym + Sin[phi$sym] Iy$sym), t$sym, Iz$sym]
    // Simplify[#, Assumptions->{Element[delta$sym, Reals], delta$sym > 0, Element[omega$sym, Reals], omega$sym >= 0}]&  
    // ExpToTrig // FullSimplify), {Ix$sym, Iy$sym, Iz$sym}]; 

\[Rho] /. {t$sym -> t,  phi$sym -> \[Phi],
  delta$sym -> Subscript[\[Omega],0], omega$sym -> Subscript[\[Omega],1], 
  Ix$sym -> Ix, Iy$sym -> Iy, Iz$sym -> Iz}

(*@
Clean up:
@*)

Clear[Ix$sym, Iy$sym, Iz$sym, Sx$sym, Sy$sym, Sz$sym, omega$sym, d$sym, delta$sym,  theta$sym, phi$sym, rho$sym, t$sym]

(*@
Harmonic oscillator evolution.  First, create the harmonic oscillator Hamiltonian in symmetric form.  Evolve % 
the lowering operator and confirm that it picks up the expected phase factor. % 
@*)

Clear[aL$sym, aR$sym, omega$sym, Q$sym, P$sym, H$sym, Q, P];
CreateScalar[omega$sym];
OscSingle$CreateOperators[aL$sym, aR$sym];

H$sym = omega$sym (aL$sym**aR$sym + aR$sym**aL$sym)/2;

vtest["05a > free evolution of a", 
  Evolver[H$sym, t$sym, aL$sym] === aL$sym Exp[I omega$sym t$sym]]

(*@
Evolve the position operator.  To do this, write the position operator in terms of the raising and lowering % 
operators, calculate the evolution, and rewite the answer in terms of the position and momentum operator. %
For the re-write step, create a \emph{new} set of non-commuting operators, to avoid chasing our tail. %
@*)

CreateOperator[{{Q,P}}];
{Q$sym, P$sym} = {(aR$sym + aL$sym)/Sqrt[2], I (aR$sym - aL$sym)/Sqrt[2]};
QP$rules = {aR$sym -> (Q + I P)/Sqrt[2], aL$sym -> (Q - I P)/Sqrt[2]};

vtest["05b > free evolution of Q", 
  Simplify[Evolver[H$sym, t$sym, Q$sym] /. QP$rules] === Q Cos[omega$sym t$sym] + P Sin[omega$sym t$sym]]

(*~ END ~*)

On[SpinSingle$CreateOperators::comm]
On[SpinSingle$CreateOperators::simplify]
On[SpinSingle$CreateOperators::nocreate]
On[OscSingle$CreateOperators::comm]
On[OscSingle$CreateOperators::create]






