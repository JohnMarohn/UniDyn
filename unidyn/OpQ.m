(* ::Package:: *)

(**
 ** OpQ.m
 ** Scalars and operators
 ** John A. Marohn
 **)

BeginPackage["OpQ`",{"Global`"}]

ScalarQ::usage="ScalarQ[a] returns True if a is a scalar and False if it is an operator."

CreateScalar::usage="CreateScalar[list] is used to batch-define a bunch of scalars.  The parameter list can be a single scalar or a list of scalars.  Example: CreateScalar[{w1,w2}]."

OperatorQ::usage="OperatorQ[a] returns True if a is an operator and False if a is a scalar."

CreateOperator::usage="CreateOperator[] is used to batch-define a bunch of operators.   Example: CreateOperator[{{Ix, Iy, Iz},{Sx,Sy,Sz}}] will create six operators, where each of the operators in the first list will commute with each of the operators of the second list."

(*~ START ~*)

(*@ 
A \emph{simple operator} is a symbol whose upvalue for \VerbFcn{SimpleOperatorQ[]} % 
is defined to be \VerbCmd{True}.  The default return for \VerbFcn{SimpleOperatorQ} % 
is \VerbCmd{False}, with the result that  unless defined otherwise, all arguments % 
to the query \VerbFcn{SimpleOpertorQ[]} return \VerbCmd{False}. By default then, % 
all quantities are scalars.  
@*)

Clear[SimpleOperatorQ]
SimpleOperatorQ[x$var_] := False;

(*@
An \emph{operator} is any expression one of whose atoms is a simple operator. %
The function call to \VerbCmd{Level} in \VerbFcn{OperatorQ[]} remarkably pulls % 
out a list of  all symbols used in the expression $x$ (all its \emph{atoms}.) %
The function \VerbFcn{ScalarQ[]} tests if its argument is a \emph{scalar}.  %
By scalar we mean \emph{not an operator}.  As the default return for % 
\VerbFcn{OperatorQ[]} is \VerbCmd{False}, the default return for % 
\VerbFcn{ScalarQ[]} is \VerbCmd{True}; all quantities default to scalars.
@*)

Clear[OperatorQ, ScalarQ]
OperatorQ[x$var_] := 
    Apply[Or, 
        Map[SimpleOperatorQ,
            Level[x$var,{-1}]]];
ScalarQ[x$var_]:= !OperatorQ[x$var];

(*@ 
You ``create'' an operator by defining an upvalue for it, e.g., arranging % 
so that \VerbFcn{SimpleOperator[the operator]} returns \VerbCmd{True}.  % 
By using upvalues, you associate this definition not with the function % 
\VerbFcn{SimpleOperator[]}, but with the operator itself. This makes for %
faster computations. The added definitions make it easy to create many % 
operators at once by passing multiple variables or a list of variables % 
to the function \VerbFcn{CreateOperator[]}. 
@*)

Clear[CreateOperator];
CreateOperator[a$sym_Symbol] := 
    (Clear[a$sym];
    SimpleOperatorQ[a$sym] ^:= True)
CreateOperator[a$sym_,b$sym__] := 
    (CreateOperator[a$sym];
    CreateOperator[b$sym];) 
CreateOperator[a$sym_?VectorQ] := 
    (CreateOperator /@ a$sym;)

(*@ 
You don't really need to ``create'' a scalar, since this is the default %
category for any symbol, given the above definitions.  Nevertheless, % 
by defining the upvalue of \VerbFcn{ScalarQ} to be \VerbCmd{True}, you % 
can speed up computations which involve testing to see whether or not an % 
object is a scalar.
@*)

Clear[CreateScalar];
CreateScalar[a$sym_Symbol] :=
    (Clear[a$sym];
    ScalarQ[a$sym] ^:= True)
CreateScalar[a$sym_List] :=
    (CreateScalar /@ a$sym;)
CreateScalar[a$sym_,b$sym__] :=
    (CreateScalar[a$sym];
    CreateScalar[b$sym];)

(*@
Passing a matrix to \VerbFcn{SimpleOperator} invokes the following % 
function call.  This function assigns the operators in the matrix a % 
\emph{phylum} and an \emph{order} which is determined by the operators % 
location in the matrix.
@*)

CreateOperator[a$sym_?ListQ] :=
 Module[{val, m, n},
  (Clear[#]; SimpleOperatorQ[#] ^= True) &
           /@ Flatten[a$sym];
  Do[
   Do[val = a$sym[[m]][[n]];
        phylum[val] ^= m;
        order[val] ^= n,
    {n, Dimensions[a$sym[[m]]][[1]]}],
   {m, Dimensions[a$sym][[1]]}
   ]
  ]
(*@
The idea of an operator having a \emph{phylum} and an \emph{order} can be % 
understood best with an example.  Consider, for example, the function call 
\[
    \text{\VerbFcn{CreateOperator}}[\{\{I_x, I_y, I_z\},\{S_x, S_y, S_z\}\}]
\]
This call will create six operators which, when passed to the functions % 
\VerbFcn{phylum} and \VerbFcn{order}, will return the following values:  

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
\end{center}

Having a \VerbFcn{phylum} and an \VerbFcn{order} assigned to each operator % 
will be used below to sort operators and to decide whether or not two % 
perators commute.
@*)

(*~ END ~*)

EndPackage[]

If[$VerboseLoad == True,
    Message[CreateOperator::usage];
    Message[CreateScalar::usage];
]









