(* ::Package:: *)

(**
 ** Inv-tests.m
 ** Operator inverse units tests
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

Clear[Ix$sym, Iy$sym, Iz$sym, aL$sym, aR$sym];
Clear[\[Omega], \[CapitalDelta]];

(*@
Create some operators and scalars to play with.
@*)

CreateOperator[{{Ix$sym, Iy$sym, Iz$sym},{aL$sym, aR$sym}}];
SpinSingle$CreateOperators[Ix$sym, Iy$sym, Iz$sym, 1/2];
OscSingle$CreateOperators[aL$sym, aR$sym];

CreateScalar[{\[Omega], \[CapitalDelta]}];
$Assumptions= {Element[\[Omega], Reals], Element[\[CapitalDelta], Reals]};

(*@
Bottoming out cases.
@*)

vtest["01", Inv[1] == 1]
vtest["02", Inv[\[Omega]] == 1/\[Omega]]

(*@
Check that \VerbCmd{Mult[Inv[b], b]} and \VerbCmd{Mult[b, Inv[b]]} in an expression %
reduces to 1, if \VerbCmd{b} is an operator, when the expressions are %
flanked by operators fore and aft.  It is important in these tests that %
the flanking operators, when multiplied by \VerbCmd{b}, do not form a %
simpler expression.
@*)

vtest["03a", Mult[Inv[Iz$sym], Iz$sym] == 1]
vtest["03b", Mult[Iz$sym, Inv[Iz$sym]] == 1]

vtest["04a", Mult[aR$sym, Inv[Iz$sym], Iz$sym] == aR$sym]
vtest["04b", Mult[aR$sym, Iz$sym, Inv[Iz$sym]] == aR$sym]

vtest["05a", Mult[Inv[Iz$sym], Iz$sym, aR$sym] == aR$sym]
vtest["05b", Mult[Iz$sym, Inv[Iz$sym], aR$sym] == aR$sym]

vtest["06a", Mult[aR$sym, Inv[Iz$sym], Iz$sym, Sx$sym] == Mult[aR$sym, Sx$sym]]
vtest["06b", Mult[aR$sym, Inv[Iz$sym], Iz$sym, Sx$sym] == Mult[aR$sym, Sx$sym]]

(*@
Factor out scalars from the inverse.
@*)

vtest["07a", Inv[Ix$sym \[Omega]] == Inv[Ix$sym]/\[Omega]]
vtest["07b", Inv[\[Omega] Ix$sym] == Inv[Ix$sym]/\[Omega]]

vtest["08a", Mult[Inv[\[Omega] Ix$sym], Ix$sym] == 1/\[Omega]]
vtest["08b", Mult[Inv[Ix$sym], \[Omega] Ix$sym] == \[Omega]]
vtest["08c", Mult[Inv[\[Omega] Ix$sym], \[Omega] Ix$sym] == 1]

(*@ 
Distribute the inverse over addition.
@*)

vtest["09", Inv[\[Omega] Ix$sym + \[CapitalDelta] Iy$sym] == Inv[Ix$sym]/\[Omega] + Inv[Iy$sym]/\[CapitalDelta]]

(*@ 
The inverse applied to a \VerbCmd{Mult[]} of operators should give % 
a list of \VerbCmd{Inv[]} operators multiplied together, %
with the list order reversed.
@*)

vtest["10", Inv[Mult[Iy$sym, aL$sym]] == Mult[Inv[aL$sym], Inv[Iy$sym]]]
vtest["11", Inv[Mult[\[Omega] Iy$sym, \[CapitalDelta] aL$sym]] == Mult[Inv[aL$sym], Inv[Iy$sym]]/(\[Omega] \[CapitalDelta])]
vtest["12", Mult[Inv[Mult[aL$sym, Iy$sym]], aL$sym, Iy$sym] == 1]

(*@ 
The inverse of product of operators times the operators is 1. %
@*)

vtest["13", Mult[Inv[Mult[aL$sym, Iz$sym]], Mult[aL$sym, Iz$sym]] == 1]
vtest["14", Mult[Inv[Mult[aL$sym, Iz$sym]], aL$sym, Iz$sym] == 1]

vtest["15", Mult[Mult[aL$sym, Iz$sym], Inv[Mult[aL$sym, Iz$sym]]] == 1]
vtest["16", Mult[Inv[Mult[aL$sym, Iz$sym]], aL$sym, Iz$sym] == 1]


(*~ END ~*)

Clear[Ix$sym, Iy$sym, Iz$sym, aL$sym ,aR$sym];
Clear[\[Omega], \[CapitalDelta]];



