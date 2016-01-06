(* ::Package:: *)

(** 
 ** Oscillators.m
 ** Harmonic oscillator operators
 ** John Marohn
 ** 2016/01/06
 **)
 
BeginPackage["Osc`",{"Global`","NC`","NCAlgebra`","OpCreate`","Mult`","Comm`"}]

OscSingle$CreateOperators::usage="OscSingle$CreateOperators[aL,aR] creates a raising operator aR and a lowering operator aL for single harmonic oscillator and defines the operator commutation relations."

OscSingle$CreateOperators::create="Creating oscillator operators."

OscSingle$CreateOperators::nocreate="Oscillator operators already exist."

OscSingle$CreateOperators::comm="Adding oscillator commutations relations.";

Begin["`Private`"]

(*~ START ~*)

(*@
The commutation relations are defined as \emph{upvalues} of the lowering and % 
raising operators.  Here aR = $a$, the lowering operator, and aL = $a^{\dagger}$, % 
the raising operator.
@*)

OscSingle$CreateOperators[aL$sym_,aR$sym_] := 

Module[{nonexistent},

(*@
Test if the operators exist; if they do not already exist, then create them.
@*)

nonexistent = Or @@ (NonCommutativeMultiply`CommutativeQ /@ {aL$sym, aR$sym});

If[nonexistent == True,
    Clear[aL$sym, aR$sym];
        CreateOperator[{{aL$sym, aR$sym}}];
        Message[OscSingle$CreateOperators::create];,
    Message[OscSingle$CreateOperators::nocreate];];

aL$sym /: Comm[aL$sym, aR$sym] = 1;
aR$sym /: Comm[aR$sym, aL$sym] = -1;

Message[OscSingle$CreateOperators::comm]

Return[{aL$sym, aR$sym}]
]

(*~ END ~*)

End[]

EndPackage[]

If[$VerboseLoad == True,
    Message[OscSingle$CreateOperators::usage]
]



