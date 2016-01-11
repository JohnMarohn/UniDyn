(* ::Package:: *)

(**
 ** Spins-tests.m
 ** Spin angular momentum operator tests 
 ** John Marohn
 ** 2016/01/06
 **)

Off[SpinSingle$CreateOperators::create]
Off[SpinSingle$CreateOperators::nocreate]
Off[SpinSingle$CreateOperators::comm]
Off[SpinSingle$CreateOperators::simplify]
Off[SpinSingle$CreateOperators::nosimplify]

(*@ Create a shorthand function for creating unit tests.
@*)

If[$VersionNumber < 10.,

  vtest[label_,test_] :=
    If[test === True, 
      Print["Pass"],
      Print["Fail > ", StringJoin["Spins > test",ToString[label]]]],

  vtest[label_,test_] := 
      VerificationTest[test,
          True,
          TestID-> StringJoin[
              "Spins > test",
              ToString[label]]]
]

Clear[w, Ix$sym, Iy$sym, Iz$sym, Sx$sym, Sy$sym, Sz$sym];

(*~ START ~*)

(*@
If there is one operator in the requested list of three spin operators that is not defined, % 
then create all three spin operators afresh.  Here we check that the test does what we want. %
If one or two of the operators is defined already, the test should come out true. %
@*)

Clear[Ix$sym, Iy$sym, Iz$sym];
CreateOperator[{{Ix$sym, Iy$sym}}];
tests = NonCommutativeMultiply`CommutativeQ /@ {Ix$sym, Iy$sym, Iz$sym};
vtest["00a", Or @@ tests == True]

(*@
If, on the other hand, all three spin operators have been defined already, then the % 
test should come out false. % 
@*)

Clear[Ix$sym, Iy$sym, Iz$sym];
CreateOperator[{{Ix$sym, Iy$sym, Iz$sym}}];
tests = NonCommutativeMultiply`CommutativeQ /@ {Ix$sym, Iy$sym, Iz$sym};
vtest["00b", Or @@ tests == False]

(*@
We should also check the limiting case that *none* of the operators have been %
defined yet. %
@*)

Clear[Ix$sym, Iy$sym, Iz$sym];
tests = NonCommutativeMultiply`CommutativeQ /@ {Ix$sym, Iy$sym, Iz$sym};
vtest["00c", Or @@ tests == True]

(*@
Create spin angular momentum operators with the total angular momentum unspecified.  % 
Test that the canonical angular momentum commutation relation holds true.  With the % 
total angular momentum unspecified, the product $I_x I_y$ cannot be simplified further.
@*)

Clear[Ix$sym, Iy$sym, Iz$sym]
SpinSingle$CreateOperators[Ix$sym, Iy$sym, Iz$sym];

vtest["01a", Comm[Ix$sym, Iy$sym] === I Iz$sym]
vtest["01b", Mult[Ix$sym, Iy$sym] === Mult[Ix$sym, Iy$sym]]
vtest["01c", Not[Mult[Ix$sym, Iy$sym] === I Iz$sym/2]]
vtest["01d", Not[Mult[Iz$sym, Iz$sym] === 1/4]]

(*@
Create two sets of operators, one set for $I$ spins and one set for $S$ spins. % 
 Assign the $I$-spin operators the properties of $I = 1/2$ angular momentum operators. %
@*)

Clear[Ix$sym, Iy$sym, Iz$sym, Sx$sym, Sy$sym, Sz$sym];
CreateOperator[{{Ix$sym, Iy$sym, Iz$sym},{Sx$sym, Sy$sym, Sz$sym}}];
SpinSingle$CreateOperators[Ix$sym, Iy$sym, Iz$sym, 1/2];

(*@
We have not defined the $S$ operators to be spin operators yet.  Nevertheless, the % 
commutator of one $S$ operator with another should be non-zero.  On the other hand, % 
the commutator of an $S$ operator with an $I$ operator should be zero.
@*)

vtest["02a", Not[Comm[Sx$sym, Sz$sym] === 0]]
vtest["02b", Comm[Iz$sym, Sz$sym] === 0]

(*@
The canonical commutation relations hold true for the $I$-spin operators.  % 
In addition, the product of two spin angular momentum operators can be % 
further simplified. 
@*)

vtest["03a", Comm[Ix$sym, Iy$sym] === I Iz$sym]
vtest["03b", Mult[Ix$sym, Iy$sym] === I Iz$sym/2]
vtest["03c", Mult[Iz$sym, Iz$sym] === 1/4]

(*@
Test that the double commutator of $I_x$ with the free-evolution Hamiltonian % 
returns the expected reult. %
@*)

Clear[h, w, rho0, rho2];
CreateScalar[w];
h = w Iz$sym;
rho0 = Ix$sym;
rho2 = Comm[-I h, Comm[-I h,rho0]];
vtest["04a", rho2 == - w*w**Ix$sym]

(*@
Try this test again with another spin operator on the backend.
@*)

Clear[Ix$sym, Iy$sym, Iz$sym, Sx$sym, Sy$sym, Sz$sym]
CreateOperator[{{Ix$sym, Iy$sym, Iz$sym},{Sx$sym, Sy$sym, Sz$sym}}];
SpinSingle$CreateOperators[Ix$sym, Iy$sym, Iz$sym];
SpinSingle$CreateOperators[Sx$sym, Sy$sym, Sz$sym];

Clear[h, w, rho0, rho2];
CreateScalar[w];
h = w Iz$sym**Sz$sym;
rho0 = Ix$sym;
rho2 = Comm[-I h,Comm[-I h,rho0]];
vtest["04b", rho2 == - w*w**Ix$sym**Sz$sym**Sz$sym]

(*~ END ~*)

Clear[h, w, rho0, rho2];
Clear[Ix$sym, Iy$sym, Iz$sym, Sx$sym, Sy$sym, Sz$sym];

On[SpinSingle$CreateOperators::create]
On[SpinSingle$CreateOperators::nocreate]
On[SpinSingle$CreateOperators::comm]
On[SpinSingle$CreateOperators::simplify]
On[SpinSingle$CreateOperators::nosimplify]









