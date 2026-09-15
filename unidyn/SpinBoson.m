(* ::Package:: *)

(** 
 ** SpinBoson.m
 ** Spin 1/2 angular momentum operators, harmonic oscillator operators, 
 **  including raising and lowering operators
 ** John Marohn
 ** 2025/12/24 add SpinBoson$CreateOperators
 ** 2026/09/15 add TwoSpinBoson$CreateOperators
 **)


BeginPackage["SpinBoson`",{"Global`","OpQ`","Mult`","Comm`","Spins`","Osc`"}]

SpinBoson$CreateOperators::usage="SpinBoson$CreateOperators[Ix,Iy,Iz,Ip,Im,aR,aL] creates Ix, Iy, Iz spin one half angular-momentum operators; the associated spin raising and lowering operators Ip, Im; and harmonic-oscllator raising and lowering operators aR, aL."

SpinBoson$CreateOperators::create="Creating operators."

SpinBoson$CreateOperators::nocreate="Operators already exist."

SpinBoson$CreateOperators::comm="Adding Ip and Im commutations relations."

SpinBoson$CreateOperators::simp="Adding Ip and Im simplification rules."

SpinBoson$CreateOperators::normord="Adding aL and aR normal ordering rule."

TwoSpinBoson$CreateOperators::usage="TwoSpinBoson$CreateOperators[Ix,Iy,Iz,Ip,Im,Sx,Sy,Sz,Sp,Sm,aR,aL] creates spin one half angular-momentum operators Ix, Iy, Iz and the associated spin raising and lowering operators Ip, Im; spin one half angular-momentum operators Sx, Sy, Sz and the associated spin raising and lowering operators Sp, Sm; and harmonic-oscllator raising and lowering operators aR, aL."

TwoSpinBoson$CreateOperators::create="Creating operators."

TwoSpinBoson$CreateOperators::nocreate="Operators already exist."

TwoSpinBoson$CreateOperators::comm="Adding Ip, Im and Sp, Sm commutations relations."

TwoSpinBoson$CreateOperators::simp="Adding Ip, Im and Sp, Sm simplification rules."

TwoSpinBoson$CreateOperators::normord="Adding aL and aR normal ordering rule."

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

(*@
Add raising and lowering operator commutation rules. %
The commutations relations are defined as \emph{upvalues} of the %
spin raising and lowering operators.
@*)

Ip$sym /: Comm[Ip$sym, Im$sym] = 2 Iz$sym;
Ip$sym /: Comm[Ip$sym, Ix$sym] = Iz$sym;
Ip$sym /: Comm[Ip$sym, Iy$sym] = I Iz$sym;
Ip$sym /: Comm[Ip$sym, Iz$sym] = -Ip$sym;

Im$sym /: Comm[Im$sym, Ip$sym] = -2 Iz$sym;
Im$sym /: Comm[Im$sym, Ix$sym] = -Iz$sym;
Im$sym /: Comm[Im$sym, Iy$sym] = I Iz$sym;
Im$sym /: Comm[Im$sym, Iz$sym] = Im$sym;

Iz$sym /: Comm[Iz$sym, Ip$sym] = Ip$sym;
Iz$sym /: Comm[Iz$sym, Im$sym] = -Im$sym;

Message[SpinBoson$CreateOperators::comm];

(*@
Add raising and lowering operator simplification rules. %
@*)

Ip$sym /: Mult[a___, Ip$sym, Ip$sym, b___] := 0;
Im$sym /: Mult[a___, Im$sym, Im$sym, b___] := 0;

Ip$sym /: Mult[a___, Ip$sym, Iz$sym ,b___] := -(1/2)Mult[a, Ip$sym, b];
Ip$sym /: Mult[a___, Ip$sym, Im$sym,b___] := 1/2 Mult[a, b] + Mult[a, Iz$sym, b];

Im$sym /: Mult[a___, Im$sym, Iz$sym, b___]:= 1/2 Mult[a, Im$sym, b];
Im$sym /: Mult[a___, Im$sym, Ip$sym, b___] := 1/2 Mult[a, b] - Mult[a, Iz$sym, b];

Iz$sym /: Mult[a___, Iz$sym, Ip$sym, b___] := 1/2 Mult[a, Ip$sym, b];
Iz$sym /: Mult[a___, Iz$sym, Im$sym, b___] := -(1/2)Mult[a, Im$sym, b];

Message[SpinBoson$CreateOperators::simp];

(*@
Try to achieve normal ordering of the harmonic oscillator raising and lowering operators. %
@*)

aR$sym /: Mult[a___, aL$sym, aR$sym, b___] := Mult[a, aR$sym, aL$sym, b] + Mult[a, b];

Message[SpinBoson$CreateOperators::normord];

Return[{Ix$sym, Iy$sym, Iz$sym, Ip$sym, Im$sym, aR$sym, aL$sym}]
]


TwoSpinBoson$CreateOperators[Ix$sym_, Iy$sym_, Iz$sym_, Ip$sym_, Im$sym_, Sx$sym_, Sy$sym_, Sz$sym_, Sp$sym_, Sm$sym_, aR$sym_, aL$sym_] :=

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
    Not[OperatorQ[Sx$sym]] || 
    Not[OperatorQ[Sy$sym]] ||
    Not[OperatorQ[Sz$sym]] ||  
    Not[OperatorQ[Sp$sym]] ||  
    Not[OperatorQ[Sm$sym]] || 
    Not[OperatorQ[aR$sym]] ||  
    Not[OperatorQ[aL$sym]];

If[nonexistent == True,
	Clear[Ix$sym, Iy$sym, Iz$sym, Ip$sym, Im$sym, 
	      Sx$sym, Sy$sym, Sz$sym, Sp$sym, Sm$sym, 
	      aR$sym, aL$sym];
		CreateOperator[{{Ix$sym, Iy$sym, Iz$sym, Ip$sym, Im$sym}, 
		                {Sx$sym, Sy$sym, Sz$sym, Sp$sym, Sm$sym}, 
		                {aL$sym, aR$sym}}];
		SpinSingle$CreateOperators[Ix$sym, Iy$sym, Iz$sym, 1/2];
		SpinSingle$CreateOperators[Sx$sym, Sy$sym, Sz$sym, 1/2];
		OscSingle$CreateOperators[aL$sym, aR$sym];
		Message[TwoSpinBoson$CreateOperators::create],
	Message[TwoSpinBoson$CreateOperators::nocreate];];

(*@
Add raising and lowering operator commutation rules. %
The commutations relations are defined as \emph{upvalues} of the %
spin raising and lowering operators.
@*)

Ip$sym /: Comm[Ip$sym, Im$sym] = 2 Iz$sym;
Ip$sym /: Comm[Ip$sym, Ix$sym] = Iz$sym;
Ip$sym /: Comm[Ip$sym, Iy$sym] = I Iz$sym;
Ip$sym /: Comm[Ip$sym, Iz$sym] = -Ip$sym;

Im$sym /: Comm[Im$sym, Ip$sym] = -2 Iz$sym;
Im$sym /: Comm[Im$sym, Ix$sym] = -Iz$sym;
Im$sym /: Comm[Im$sym, Iy$sym] = I Iz$sym;
Im$sym /: Comm[Im$sym, Iz$sym] = Im$sym;

Iz$sym /: Comm[Iz$sym, Ip$sym] = Ip$sym;
Iz$sym /: Comm[Iz$sym, Im$sym] = -Im$sym;

Sp$sym /: Comm[Sp$sym, Sm$sym] = 2 Sz$sym;
Sp$sym /: Comm[Sp$sym, Sx$sym] = Sz$sym;
Sp$sym /: Comm[Sp$sym, Sy$sym] = I Sz$sym;
Sp$sym /: Comm[Sp$sym, Sz$sym] = -Sp$sym;

Sm$sym /: Comm[Sm$sym, Sp$sym] = -2 Sz$sym;
Sm$sym /: Comm[Sm$sym, Sx$sym] = -Sz$sym;
Sm$sym /: Comm[Sm$sym, Sy$sym] = I Sz$sym;
Sm$sym /: Comm[Sm$sym, Sz$sym] = Sm$sym;

Sz$sym /: Comm[Sz$sym, Sp$sym] = Sp$sym;
Sz$sym /: Comm[Sz$sym, Sm$sym] = -Sm$sym;

Message[TwoSpinBoson$CreateOperators::comm];

(*@
Add raising and lowering operator simplification rules. %
@*)

Ip$sym /: Mult[a___, Ip$sym, Ip$sym, b___] := 0;
Im$sym /: Mult[a___, Im$sym, Im$sym, b___] := 0;

Ip$sym /: Mult[a___, Ip$sym, Iz$sym ,b___] := -(1/2)Mult[a, Ip$sym, b];
Ip$sym /: Mult[a___, Ip$sym, Im$sym,b___] := 1/2 Mult[a, b] + Mult[a, Iz$sym, b];

Im$sym /: Mult[a___, Im$sym, Iz$sym, b___]:= 1/2 Mult[a, Im$sym, b];
Im$sym /: Mult[a___, Im$sym, Ip$sym, b___] := 1/2 Mult[a, b] - Mult[a, Iz$sym, b];

Iz$sym /: Mult[a___, Iz$sym, Ip$sym, b___] := 1/2 Mult[a, Ip$sym, b];
Iz$sym /: Mult[a___, Iz$sym, Im$sym, b___] := -(1/2)Mult[a, Im$sym, b];

Sp$sym /: Mult[a___, Sp$sym, Sp$sym, b___] := 0;
Sm$sym /: Mult[a___, Sm$sym, Sm$sym, b___] := 0;

Sp$sym /: Mult[a___, Sp$sym, Sz$sym ,b___] := -(1/2)Mult[a, Sp$sym, b];
Sp$sym /: Mult[a___, Sp$sym, Sm$sym,b___] := 1/2 Mult[a, b] + Mult[a, Sz$sym, b];

Sm$sym /: Mult[a___, Sm$sym, Sz$sym, b___]:= 1/2 Mult[a, Sm$sym, b];
Sm$sym /: Mult[a___, Sm$sym, Sp$sym, b___] := 1/2 Mult[a, b] - Mult[a, Sz$sym, b];

Sz$sym /: Mult[a___, Sz$sym, Sp$sym, b___] := 1/2 Mult[a, Sp$sym, b];
Sz$sym /: Mult[a___, Sz$sym, Sm$sym, b___] := -(1/2)Mult[a, Sm$sym, b];

Message[TwoSpinBoson$CreateOperators::simp];

(*@
Try to achieve normal ordering of the harmonic oscillator raising and lowering operators. %
@*)

aR$sym /: Mult[a___, aL$sym, aR$sym, b___] := Mult[a, aR$sym, aL$sym, b] + Mult[a, b];

Message[TwoSpinBoson$CreateOperators::normord];

Return[{Ix$sym, Iy$sym, Iz$sym, Ip$sym, Im$sym, Sx$sym, Sy$sym, Sz$sym, Sp$sym, Sm$sym, aR$sym, aL$sym}]
]

(*~ END ~*)

End[]

EndPackage[]

If[$VerboseLoad == True,
    Message[SpinBoson$CreateOperators::usage];
    Message[TwoSpinBoson$CreateOperators::usage];
]












