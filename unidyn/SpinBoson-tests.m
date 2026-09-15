(* ::Package:: *)

(**
 ** SpinBoson-tests.m
 ** Test the creation of spin 1/2 angular momentum operators, 
 **  harmonic oscillator operators, including raising and lowering operators
 ** John Marohn
 ** 2025/12/24 add SpinBoson$CreateOperators tests
 ** 2026/09/15 add TwoSpinBoson$CreateOperators tests
 **)


Off[SpinSingle$CreateOperators::create]
Off[SpinSingle$CreateOperators::nocreate]
Off[SpinSingle$CreateOperators::comm]
Off[SpinSingle$CreateOperators::simplify]
Off[SpinSingle$CreateOperators::nosimplify]

Off[OscSingle$CreateOperators::create]
Off[OscSingle$CreateOperators::nocreate]
Off[OscSingle$CreateOperators::comm]

Off[SpinBoson$CreateOperators::create]
Off[SpinBoson$CreateOperators::nocreate]
Off[SpinBoson$CreateOperators::comm]
Off[SpinBoson$CreateOperators::simp]
Off[SpinBoson$CreateOperators::normord]

Off[TwoSpinBoson$CreateOperators::create]
Off[TwoSpinBoson$CreateOperators::nocreate]
Off[TwoSpinBoson$CreateOperators::comm]
Off[TwoSpinBoson$CreateOperators::simp]
Off[TwoSpinBoson$CreateOperators::normord]

(*@ Create a shorthand function for creating unit tests.
@*)

If[$VersionNumber < 10.,

  vtest[label_,test_] :=
    If[test === True, 
      Print["Pass"],
      Print["Fail > ", StringJoin["SpinBoson > test",ToString[label]]]],

  vtest[label_,test_] := 
      VerificationTest[test,
          True,
          TestID-> StringJoin[
              "SpinBoson > test",
              ToString[label]]]
]

Clear[Ix$sym, Iy$sym, Iz$sym, Ip$sym, Im$sym, aR$sym, aL$sym];

(*~ START ~*)

(*@
Create operators to play with: one two-level atom and a cavity mode.
@*)

SpinBoson$CreateOperators[Ix$sym, Iy$sym, Iz$sym, Ip$sym, Im$sym, aR$sym, aL$sym];

(*@
Check that we get the expected Hamiltonian after normal ordering.
@*)

vtest["01", Simplify[1/2 (Mult[aL$sym, aR$sym] + Mult[aR$sym, aL$sym])] == Mult[aR$sym, aL$sym] + 1/2]

(*@
A normal-ordering check for products of two creation/annihilation operators.
@*)

vtest["02a", Mult[aL$sym, aR$sym] == Mult[aR$sym, aL$sym] + 1]

(*@
A normal-ordering check for products of three creation/annihilation operators.
@*)

vtest["03a", Mult[aL$sym, aR$sym, aL$sym] == Mult[aR$sym, aL$sym, aL$sym] + aL$sym]
vtest["03b", Mult[aL$sym, aL$sym, aR$sym] == Mult[aR$sym, aL$sym, aL$sym] + 2 aL$sym]
vtest["03c", Mult[aR$sym, aL$sym, aR$sym] == Mult[aR$sym, aR$sym, aL$sym] + aR$sym]
vtest["03d", Mult[aL$sym, aR$sym, aR$sym] == Mult[aR$sym, aR$sym, aL$sym] + 2 aR$sym]

(*@
Two angular momentum identities.
@*)

vtest["04a", Mult[Im$sym, Ip$sym] == Simplify[3/4 - Mult[Iz$sym, Iz$sym] - Iz$sym]]
vtest["04b", Mult[Ip$sym, Im$sym] == Simplify[3/4 - Mult[Iz$sym, Iz$sym] + Iz$sym]]

(*@
Create operators to play with: two two-level atoms and a cavity mode.
@*)

Clear[Ix$sym, Iy$sym, Iz$sym, Ip$sym, Im$sym, Sx$sym, Sy$sym, Sz$sym, Sp$sym, Sm$sym, aR$sym, aL$sym];

TwoSpinBoson$CreateOperators[Ix$sym, Iy$sym, Iz$sym, Ip$sym, Im$sym, Sx$sym, Sy$sym, Sz$sym, Sp$sym, Sm$sym, aR$sym, aL$sym];

(*@
Check that I and S spin operators commute.
@*)

vtest["05a", Comm[Ix$sym, Sx$sym + Sy$sym + Sz$sym] == 0]
vtest["05b", Comm[Iy$sym, Sx$sym + Sy$sym + Sz$sym] == 0]
vtest["05c", Comm[Iz$sym, Sx$sym + Sy$sym + Sz$sym] == 0]

(*@
Two angular momentum identities.
@*)

vtest["06a", Mult[Sm$sym, Sp$sym] == Simplify[3/4 - Mult[Sz$sym, Sz$sym] - Sz$sym]]
vtest["06b", Mult[Sp$sym, Sm$sym] == Simplify[3/4 - Mult[Sz$sym, Sz$sym] + Sz$sym]]



(*~ END ~*)

Clear[Ix$sym, Iy$sym, Iz$sym, Ip$sym, Im$sym, aR$sym, aL$sym];

On[SpinSingle$CreateOperators::create]
On[SpinSingle$CreateOperators::nocreate]
On[SpinSingle$CreateOperators::comm]
On[SpinSingle$CreateOperators::simplify]
On[SpinSingle$CreateOperators::nosimplify]

On[OscSingle$CreateOperators::create]
On[OscSingle$CreateOperators::nocreate]
On[OscSingle$CreateOperators::comm]

On[SpinBoson$CreateOperators::create]
On[SpinBoson$CreateOperators::nocreate]
On[SpinBoson$CreateOperators::comm]
On[SpinBoson$CreateOperators::simp]
On[SpinBoson$CreateOperators::normord]

On[TwoSpinBoson$CreateOperators::create]
On[TwoSpinBoson$CreateOperators::nocreate]
On[TwoSpinBoson$CreateOperators::comm]
On[TwoSpinBoson$CreateOperators::simp]
On[TwoSpinBoson$CreateOperators::normord]






