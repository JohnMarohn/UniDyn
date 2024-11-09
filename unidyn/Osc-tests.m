(* ::Package:: *)

(**
 ** Oscillators-tests.m
 ** Spin angular momentum operator tests 
 ** John Marohn
 ** 2016/01/06
 **)

Off[OscSingle$CreateOperators::create]
Off[OscSingle$CreateOperators::nocreate]
Off[OscSingle$CreateOperators::comm]

Off[SpinSingle$CreateOperators::create]
Off[SpinSingle$CreateOperators::nocreate]
Off[SpinSingle$CreateOperators::comm]
Off[SpinSingle$CreateOperators::simplify]
Off[SpinSingle$CreateOperators::nosimplify]

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
Create raising and lowering operators for a single harmonic oscillator. %
Check that the ooperators are indeed created. %
Check that they have the expected commutation relations. % 
@*)

Clear[aL$sym, aR$sym, a$sym, Nop];

CreateScalar[{a$sym}];
OscSingle$CreateOperators[aL$sym, aR$sym];

vtest["01a", OperatorQ /@ {aL$sym, aR$sym} == {True, True}]
vtest["01b", Comm[aL$sym, aR$sym] === 1]
vtest["01c", Comm[aR$sym, aL$sym] === -1]

(*@
Check that scalars are factored out of the commutations relations %
involving the harmonic oscillator raising and lower operators.
@*)

vtest["01d", Comm[a$sym aL$sym, aR$sym] === a$sym]
vtest["01e", Comm[aL$sym, a$sym aR$sym] === a$sym]

(*@
Test the commutation relations by defining the number operator, % 
Nop = $N = a^{\dagger} a$, and checking the commutation relations % 
$[N,a^{\dagger}] = a^{\dagger}$ and $[N,a]=-a$. %
@*)

Nop = Mult[aR$sym, aL$sym];

vtest["01d", Comm[Nop, aR$sym] === aR$sym]
vtest["01e", Comm[Nop, aL$sym] === -aL$sym]

Clear[aL$sym, aR$sym, a$sym, Nop];

(*@
Check that we can define harmonic oscillator operators ``on top of'' %
an existing operator that commutes with the harmonic-oscillator operators.
@*)
   
Clear[aL$sym, aR$sym, a, Q];

CreateScalar[{a}];
CreateOperator[{{Q},{aR$sym, aL$sym}}]
OscSingle$CreateOperators[aL$sym, aR$sym];

vtest["02a", Comm[aL$sym, aR$sym] === 1]
vtest["02b", Comm[aR$sym, aL$sym] === -1]
vtest["02c", Comm[a, Q] == 0]
vtest["02d", Comm[a, aL$sym] == 0]
vtest["02e", Comm[Q, aL$sym] === 0]
vtest["02f", MultSort[Mult[aL$sym, Q]] === Mult[Q, aL$sym]]

Clear[aL$sym, aR$sym, a, Q];
       
(*@
Test the commutations for the position and momentum operators. 
@*) 

Clear[aL$sym, aR$sym, Q$sym, P$sym];

OscSingle$CreateOperators[aL$sym, aR$sym];
Q$sym = (aR$sym + aL$sym)/Sqrt[2];
P$sym = I (aR$sym - aL$sym)/Sqrt[2];

vtest["03a", Comm[Q$sym, P$sym] === I]
vtest["03b", Comm[P$sym, Q$sym] === -I]

Clear[aL$sym, aR$sym, Q$sym, P$sym];

(*@
Abother check that we can define harmonic oscillator operators ``on top of'' % 
existing operators. In the following tests we prove that an operator like %
$I_x$ (not defined as a spin operator, just an operator) commutes with % 
one of the harmonic oscillator operators while the harmonic oscillator %
commutation relations are retained. %  
@*)

Clear[Ix$sym, Iy$sym, Iz$sym, aL$sym, aR$sym, Nop];
CreateOperator[{{Ix$sym, Iy$sym, Iz$sym},{aR$sym, aL$sym}}]
OscSingle$CreateOperators[aL$sym, aR$sym];
Nop = Mult[aR$sym, aL$sym];

vtest["04a", Not[Comm[Ix$sym, Iy$sym] === 0]]
vtest["04b", Comm[Ix$sym, aL$sym] == 0]
vtest["04c", Comm[Mult[Ix$sym, Iy$sym], aL$sym] == 0]
vtest["04d", Comm[aL$sym, aR$sym] === 1]
vtest["04e", Comm[Nop, aR$sym] === aR$sym]
vtest["04f", Comm[Nop, aL$sym] === -aL$sym]

(*@
We defined the $I$ operators to have higher precedence than the harmonic %
oscillator operators.  Check that \VerbFcn{MultSort} pulls the $I$ operators %
out front as expected. %
@*)

vtest["05", MultSort[Mult[aL$sym, Ix$sym, aR$sym, aL$sym, Iy$sym]] 
    === Mult[Ix$sym, Iy$sym, aL$sym, aR$sym, aL$sym]]

Clear[Ix$sym, Iy$sym, Iz$sym, aL$sym, aR$sym, Nop];

(*~ END ~*)

On[OscSingle$CreateOperators::create]
On[OscSingle$CreateOperators::nocreate]
On[OscSingle$CreateOperators::comm]

On[SpinSingle$CreateOperators::create]
On[SpinSingle$CreateOperators::nocreate]
On[SpinSingle$CreateOperators::comm]
On[SpinSingle$CreateOperators::simplify]
On[SpinSingle$CreateOperators::nosimplify]












