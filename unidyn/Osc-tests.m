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
If there is one one or both of the operators in the requested list of two oscillator %
operators that is not defined, then create both operators afresh.  Here we check % 
that the test does what we want. If one or none of the operators is defined already, % 
the test should come out true. %
@*)

Clear[aL$sym, aR$sym];
tests = NonCommutativeMultiply`CommutativeQ /@ {aL$sym, aR$sym};
vtest["00a", Or @@ tests == True]

Clear[aL$sym, aR$sym];
CreateOperator[{{aL$sym}}];
tests = NonCommutativeMultiply`CommutativeQ /@ {aL$sym, aR$sym};
vtest["00b", Or @@ tests == True]

(*@
If, on the other hand, both harmonic oscillator operators have been defined already, % 
then the test should come out false. % 
@*)

Clear[aL$sym, aR$sym];
CreateOperator[{{aL$sym, aR$sym}}];
tests = NonCommutativeMultiply`CommutativeQ /@ {aL$sym, aR$sym};
vtest["00c", Or @@ tests == False]

(*@
Create raising and lowering operators for a single harmonic oscillator.  %
Create a scalar and another operator 
@*)
   
Clear[aL$sym, aR$sym, a, Q];
CreateScalar[a];
CreateOperator[{{Q}}];
OscSingle$CreateOperators[aL$sym, aR$sym];

vtest["01a", Comm[a, Q] == 0]
vtest["01b", Comm[a, aL$sym] == 0]
vtest["01c", Not[Comm[Q, aL$sym] === 0]]
vtest["01d", aL$sym**Q === aL$sym**Q]

Clear[aL$sym, aR$sym, a, Q];
         
(*@
Test the commutation relations by defining the number operator, % 
Nop = $N = a^{\dagger} a$, and checking the commutation relations % 
$[N,a^{\dagger}] = a^{\dagger}$ and $[N,a]=-a$. %
@*)

Clear[aL$sym, aR$sym, a$sym, Nop];
CreateScalar[a$sym];
OscSingle$CreateOperators[aL$sym, aR$sym];
Nop = NonCommutativeMultiply[aR$sym, aL$sym];

vtest["02a", Comm[aL$sym, aR$sym] === 1]
vtest["02b", Comm[a$sym aL$sym, aR$sym] === a$sym]
vtest["02c", Comm[Nop, aR$sym] === aR$sym]
vtest["02d", Comm[Nop, aL$sym] === -aL$sym]

Clear[aL$sym, aR$sym, Nop, a$sym];

(*@
Test the commutations for the position and momentum operators. 
@*) 

Clear[aL$sym, aR$sym, Q$sym, P$sym];
OscSingle$CreateOperators[aL$sym, aR$sym];
Q$sym = (aR$sym+aL$sym)/Sqrt[2];
P$sym = I (aR$sym-aL$sym)/Sqrt[2];

vtest["03a", Comm[Q$sym, P$sym] === I]
vtest["03b", Comm[P$sym, Q$sym] === -I]

Clear[aL$sym, aR$sym, Q$sym, P$sym];

(*@
Check that we can define harmonic oscillator operators ``on top of'' % 
existing operators. In the following tests we prove that an operator like %
$I_x$ (not defined as a spin operator, just an operator) commutes with % 
one of the harmonic oscillator operators while the harmonic oscillator %
commutation relations are retained. %  
@*)

Clear[Ix$sym, Iy$sym, Iz$sym, aL$sym, aR$sym, Nop];
CreateOperator[{{Ix$sym, Iy$sym, Iz$sym},{aR$sym, aL$sym}}]
OscSingle$CreateOperators[aL$sym, aR$sym];
Nop = NonCommutativeMultiply[aR$sym, aL$sym];

vtest["04a", Not[Comm[Ix$sym, Iy$sym] === 0]]
vtest["04b", Comm[Ix$sym, aL$sym] == 0]
vtest["04c", Comm[Ix$sym**Iy$sym, aL$sym] == 0]
vtest["04d", Comm[aL$sym, aR$sym] === 1]
vtest["04e", Comm[Nop, aR$sym] === aR$sym]
vtest["04f", Comm[Nop, aL$sym] === -aL$sym]

(*@
We defined the $I$ operators to have higher precedence than the harmonic %
oscillator operators.  Check that \VerbFcn{MultSort} pulls the $I$ operators %
out front as expected. %
@*)

vtest["05a", MultSort[aL$sym**Ix$sym**aR$sym**aL$sym**Iy$sym] 
    === Ix$sym**Iy$sym**aL$sym**aR$sym**aL$sym]

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






