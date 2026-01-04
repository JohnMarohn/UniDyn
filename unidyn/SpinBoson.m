(* ::Package:: *)

(** 
 ** SpinBoson.m
 ** Spin 1/2 angular momentum operators, harmonic oscillator operators, 
 **  including raising and lowering operators
 ** John Marohn
 ** 2025/12/24
 **)


BeginPackage["SpinBoson`",{"Global`","OpQ`","Mult`","Comm`","Spins`","Osc`"}]

SpinBoson$CreateOperators::usage="SpinBoson$CreateOperators[Ix,Iy,Iz,Ip,Im,aR,aL] creates Ix, Iy, Iz spin one half angular-momentum operators; the associated spin raising and lowering operators Ip, Im; and harmonic-oscllator raising and lowering operators aR, aL."

SpinBoson$CreateOperators::create="Creating operators."

SpinBoson$CreateOperators::nocreate="Operators already exist."

SpinBoson$CreateOperators::comm="Adding Ip and Im commutations relations."

SpinBoson$CreateOperators::simp="Adding Ip and Im simplification rules."

SpinBoson$CreateOperators::normord="Adding aL and aR normal ordering rule."

Begin["Private`"]

(*~ START ~*)

SpinBoson$CreateOperators[Ix$sym_, Iy$sym_, Iz$sym_, Ip$sym_, Im$sym_, aR$sym_, aL$sym_] :=

Module[{nonexistent},

(*@
Test if all the operators exist; if any of them do not already exist, %
then create all of them.
@*)

nonexistent = 
    Not[OperatorQ[Ix$sym]] || 
    Not[OperatorQ[Iy$sym]] ||
    Not[OperatorQ[Iz$sym]] ||  
    Not[OperatorQ[Ip$sym]] ||  
    Not[OperatorQ[Im$sym]] ||  
    Not[OperatorQ[aR$sym]] ||  
    Not[OperatorQ[aL$sym]];

If[nonexistent == True,
	Clear[Ix$sym, Iy$sym, Iz$sym, Ip$sym, Im$sym, aR$sym, aL$sym];
		CreateOperator[{{Ix$sym, Iy$sym, Iz$sym, Ip$sym, Im$sym}, {aL$sym, aR$sym}}];
		SpinSingle$CreateOperators[Ix$sym, Iy$sym, Iz$sym, 1/2];
		OscSingle$CreateOperators[aL$sym, aR$sym];
		Message[SpinBoson$CreateOperators::create],
	Message[SpinBoson$CreateOperators::nocreate];];
(*
Add raising and lowering operator commutation rules. %
The commutations relations are defined as \emph{upvalues} of the %
spin raising and lowering operators.
*)

Ip$sym /: Comm[Ip$sym, Im$sym] = 2 Iz$sym;
Ip$sym /: Comm[Ip$sym, Iz$sym] = -Ip$sym;
Im$sym /: Comm[Im$sym, Ip$sym] = -2 Iz$sym;
Im$sym /: Comm[Im$sym, Iz$sym] = Im$sym;
Iz$sym /: Comm[Iz$sym, Ip$sym] = Ip$sym;
Iz$sym /: Comm[Iz$sym, Im$sym] = -Im$sym;

Message[SpinBoson$CreateOperators::comm];

(*
Add raising and lowering operator simplification rules.
*)

Ip$sym /: Mult[a___, Ip$sym, Ip$sym, b___] := 0;

Im$sym /: Mult[a___, Im$sym, Im$sym, b___] := 0;

Ip$sym /: Mult[a___, Ip$sym, Iz$sym ,b___] := -(1/2)Mult[a, Ip$sym, b];
Ip$sym /: Mult[a___, Ip$sym, Im$sym,b___] := 1/2 Mult[a, b] + Mult[a, Iz$sym, b];

Im$sym /: Mult[a___, Im$sym, Iz$sym, b___]:= 1/2 Mult[a, Im$sym, b];
Im$sym /: Mult[a___, Im$sym, Ip$sym, b___] := 1/2 Mult[a, b] - Mult[a, Iz$sym, b];

Iz$sym /: Mult[a___, Iz$sym, Ip$sym, b___] := 1/2 Mult[a, Ip$sym, b];
Iz$sym /: Mult[a___, Iz$sym, Im$sym, b___] := -(1/2)Mult[a, Im$sym, b];

Message[SpinBoson$CreateOperators::simp];

(*
Try to achieve normal ordering of the % 
harmonic oscillator raising and lowering operators.
*)

aR$sym /: Mult[a___, aL$sym, aR$sym, b___] := Mult[a, aR$sym, aL$sym, b] + Mult[a, b];

Message[SpinBoson$CreateOperators::normord];

Return[{Ix$sym, Iy$sym, Iz$sym, Ip$sym, Im$sym, aR$sym, aL$sym}]
]

(*~ END ~*)

End[]

EndPackage[]

If[$VerboseLoad == True,
    Message[SpinBoson$CreateOperators::usage]
]



