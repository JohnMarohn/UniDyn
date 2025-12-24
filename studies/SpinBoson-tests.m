(* ::Package:: *)

(**
 ** SpinBoson-tests.m
 ** Test the creation of spin 1/2 angular momentum operators, 
 **  harmonic oscillator operators, including raising and lowering operators
 ** John Marohn
 ** 2025/12/24
 **)


(*~ START ~*)

(*@ Create a shorthand function for creating unit tests.
@*)

If[$VersionNumber < 10.,

  vtest[label_,test_] :=
    If[test === True, 
      Print["Pass"],
      Print["Fail > ", StringJoin["OpQ > test",ToString[label]]]],

  vtest[label_,test_] := 
      VerificationTest[test,
          True,
          TestID-> StringJoin[
              "OpQ > test",
              ToString[label]]]
]

Clear[Ix$sym, Iy$sym, Iz$sym, Ip$sym, Im$sym, aR$sym, aL$sym];

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



