(* ::Package:: *)

(** 
 ** Spins.m
 ** Spin angular momentum operators
 ** John Marohn
 ** 2016/01/06
 **)

BeginPackage["Spins`",{"Global`","NC`","NCAlgebra`","OpCreate`","Mult`","Comm`"}]

SpinSingle$CreateOperators::usage="SpinSingle$CreateOperators[Ix,Iy,Iz,L] creates Ix, Iy, and Iz angular momentum operators and defines their commutation relations.  When the total angular momentum L = 1/2, additional rules are defined to simplify products of the angular momentum operators.  When the total angular momentum L is unspecified, no such simplification rules are defined." ;

SpinSingle$CreateOperators::create="Creating spin operators.";

SpinSingle$CreateOperators::nocreate="Spin operators already exist.";

SpinSingle$CreateOperators::comm="Adding spin commutations relations.";

SpinSingle$CreateOperators::simplify="Angular momentum L = 1/2. Adding operator simplification rules.";

SpinSingle$CreateOperators::nosimplify="No angular momentum L undefined.";

Begin["Private`"] (* <<==== IMPORTANT *)

SpinSingle$CreateOperators[Ix$sym_,Iy$sym_,Iz$sym_,L_:Null] := 

Module[{nonexistent},

(*@
Test if the operators exist; if they do not already exist, then create them. % 
If an operator \VerbFcn{Op} has been created already, then \VerbFcn{CommutativeQ[Op]} % 
will return \VerbCmd{True}.  Unless $I_x$, $I_y$, and $I_z$ all already exist as % 
operators, then create all three operators afresh. %
@*)

nonexistent = Or @@ (CommutativeQ /@ {Ix$sym,Iy$sym,Iz$sym});

If[nonexistent == True,
    Clear[Ix$sym,Iy$sym,Iz$sym];
        CreateOperator[{{Ix$sym,Iy$sym,Iz$sym}}];
        Message[SpinSingle$CreateOperators::create],
    Message[SpinSingle$CreateOperators::nocreate];];

(*@
The commutation relations are defined as \emph{upvalues} of the various % 
spin angular momentum operators.  That is, the commutation relations are % 
associated with the operators and not with the \VerbFcn{Comm} function.  % 
The following commutation relations hold for any $L$. %
@*)

Ix$sym /: Comm[Ix$sym,Iy$sym] =  I Iz$sym; 
Ix$sym /: Comm[Ix$sym,Iz$sym] = -I Iy$sym;
Iy$sym /: Comm[Iy$sym,Ix$sym] = -I Iz$sym; 
Iy$sym /: Comm[Iy$sym,Iz$sym] =  I Ix$sym; 
Iz$sym /: Comm[Iz$sym,Iy$sym] = -I Ix$sym;
Iz$sym /: Comm[Iz$sym,Ix$sym] =  I Iy$sym;  

Message[SpinSingle$CreateOperators::comm]

Switch[L,

(*@
When the total angular momentum $L = 1/2$, additional rules are defined % 
to simplify products of the angular momentum operators. %
@*)

1/2,

Iz$sym /: NonCommutativeMultiply[a___,Iz$sym,Iz$sym,b___] := NonCommutativeMultiply[a,b]/4;
Iy$sym /: NonCommutativeMultiply[a___,Iy$sym,Iy$sym,b___] := NonCommutativeMultiply[a,b]/4;
Ix$sym /: NonCommutativeMultiply[a___,Ix$sym,Ix$sym,b___] := NonCommutativeMultiply[a,b]/4;

Iz$sym /:  NonCommutativeMultiply[a___,Iz$sym,Ix$sym,b___] :=  I NonCommutativeMultiply[a,Iy$sym,b]/2;
Iz$sym /:  NonCommutativeMultiply[a___,Iz$sym,Iy$sym,b___] := -I NonCommutativeMultiply[a,Ix$sym,b]/2;

Iy$sym /:  NonCommutativeMultiply[a___,Iy$sym,Ix$sym,b___] := -I NonCommutativeMultiply[a,Iz$sym,b]/2;
Iy$sym /:  NonCommutativeMultiply[a___,Iy$sym,Iz$sym,b___] :=  I NonCommutativeMultiply[a,Ix$sym,b]/2;

Ix$sym /:  NonCommutativeMultiply[a___,Ix$sym,Iz$sym,b___] := -I NonCommutativeMultiply[a,Iy$sym,b]/2;
Ix$sym /:  NonCommutativeMultiply[a___,Ix$sym,Iy$sym,b___] :=  I NonCommutativeMultiply[a,Iz$sym,b]/2;

Message[SpinSingle$CreateOperators::simplify],

(*@
When the total angular momentum $L$ is unspecified, no such % 
simplification rules are defined. %
@*)
    
Null,

Message[SpinSingle$CreateOperators::nosimplify]

];
Return[{Ix$sym,Iy$sym,Iz$sym}] (* <<==== IMPORTANT *)
]

(*~ END ~*)

End[]

EndPackage[]

If[$VerboseLoad == True,
    Message[SpinSingle$CreateOperators::usage]
]



