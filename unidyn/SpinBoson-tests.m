(* ::Package:: *)

(**
 ** SpinBoson-tests.m
 ** Test the creation of spin 1/2 angular momentum operators, 
 **  harmonic oscillator operators, including raising and lowering operators
 ** John Marohn
 ** 2025/12/24
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
Create some operators to play with.
@*)

SpinBoson$CreateOperators[Ix$sym, Iy$sym, Iz$sym, Ip$sym, Im$sym, aR$sym, aL$sym];

(*@
Check that we get the expected Hamiltonian after normal ordering.
@*)

vtest["01", Simplify[1/2 (Mult[aL$sym, aR$sym] + Mult[aR$sym, aL$sym])] == Mult[aR$sym, aL$sym] + 1/2]

(*@
Another normal ordering check.
@*)

vtest["02", Mult[aL$sym, aR$sym] == Mult[aR$sym, aL$sym] + 1]

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



