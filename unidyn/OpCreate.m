(* ::Package:: *)

(**
 ** OpCreate.m
 ** Create operators and scalars
 ** John A. Marohn
 **)

BeginPackage["OpCreate`",{"Global`","NC`","NCAlgebra`"}]

CreateOperator::usage="CreateOperator[] is used to batch-define a bunch of operators. Example: CreateOperator[{{Ix, Iy, Iz},{Sx,Sy,Sz}}] will create six operators;  each of the operators in the first list is meant to commute with each of the operators in the second list."

CreateScalar::usage="CreateScalar[list] is used to batch-define a bunch of scalars. The parameter list can be a single scalar or a list of scalars.  Example: CreateScalar[{w1,w2}]."

(* NO NOT put a Begin["Private`"] here because we want the upvalues to be exposed to Global` *)

Needs["NC`"]
Needs["NCAlgebra`"]

(*~ START ~*)

(*@
We are going to build on \verb+NCAlgebra+ package's modifications to %
\emph{Mathematica}'s \VerbFcn{NonCommutativeMultiply[]} function. The %
\verb+NCAlgebra+ package defines a function \VerbFcn{CommutativeQ[]} % 
which should return \VerbCmd{True} when passsed a non-commutative variable %
and should return \VerbCmd{False} when passed a commutative variable.  To %
speed up computations we are going to define the \emph{upvalue} of % 
\VerbFcn{CommutativeQ[]} to be True for a commutative variable. %
@*)

CommQ = NonCommutativeMultiply`CommutativeQ

(*@ 
You don't really need to ``create'' a scalar, since this is the default category % 
for any symbol, given the above definitions.  Nevertheless, by defining the upvalue % 
of \VerbFcn{ScalarQ} to be \VerbCmd{True}, you can speed up computations which % 
involve testing to see whether or not an object is a scalar.
@*)

Clear[CreateScalar];

CreateScalar[a$sym_Symbol] := (Clear[a$sym]; CommQ[a$sym] ^:= True;)
CreateScalar[a$sym_List] := (CreateScalar /@ a$sym;)
CreateScalar[a$sym_,b$sym__] := (CreateScalar[a$sym]; CreateScalar[b$sym];)

(*@
Passing a matrix to \VerbFcn{SimpleOperator} invokes the following function call.  % 
This function assigns the operators in the matrix a \emph{phylum} and an \emph{order} % 
which is determined by the operators location in the matrix.
@*)

Clear[CreateOperator];

CreateOperator[a$sym_?ListQ] :=

  Module[{val, m, n},
    (Clear[#]; CommQ[#] ^:= False;) & /@ Flatten[a$sym];
    Do[
    Do[val = a$sym[[m]][[n]];
      phylum[val] ^= m;
      order[val] ^= n,
      {n, Dimensions[a$sym[[m]]][[1]]}],{m, Dimensions[a$sym][[1]]}
    ]
  ]

(*@
The idea of an operator having a \emph{phylum} and an \emph{order} can be % 
understood best with an example.  Consider, for example, the function call %
\[
    \mathrm{\VerbFcn{CreateOperator}}[\{\{I_x, I_y, I_z\},\{S_x, S_y, S_z\}\}]
\]
This call will create six operators which, when passed to the functions % 
\VerbFcn{phylum} and \VerbFcn{order}, will return the following values. %
Having a \VerbFcn{phylum} and an \VerbFcn{order} assigned to % 
each operator will be used below to sort operators and to decide whether % 
or not two operators commute. %
\begin{center}
\begin{tabular}{c|c|c}
Op &  \VerbFcn{phylum}[Op] & \VerbFcn{order}[Op] \\ \hline
$I_x$ & 1 & 1 \\
$I_y$ & 1 & 2 \\
$I_z$ & 1 & 3 \\ \hline
$S_x$ & 2 & 1 \\
$S_y$ & 2 & 2 \\
$S_z$ & 2 & 3 
\end{tabular}
\end{center} @*)

(*~ END ~*)

EndPackage[]

If[$VerboseLoad == True,
    Message[CreateOperator::usage];
    Message[CreateScalar::usage]
]



