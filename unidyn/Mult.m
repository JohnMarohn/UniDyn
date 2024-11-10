(* ::Package:: *)

(**
 ** Mult.m
 ** Non commutative multiply
 ** John A. Marohn
 ** 2016/01/05
 **)

BeginPackage["Mult`",{"Global`","OpQ`"}]

Mult::usage="Mult[a,b,c] is a function that handles non-commutative multiplication."

NCSort::usage="NCSort[list] sorts the operators in list into canonical order."

SortedMult::usage="SortedMult[list] returns Mult[list$ordered], where list$ordered are the elements of list sorted into canonical order."

MultSort::usage="MultSort[NonCommutativeMultiplyt[list]] returns returns NonCommutativeMultiply[list$ordered], where list$ordered are the elements of list sorted into canonical order."

(*~ START ~*)

(*@ 
First define bottom-out and empty cases:
@*)

Clear[Mult];
Mult[a$sym_] := a$sym
Mult[] := 1

(*@
Mult distributes over \VerbFcn{Plus} and is associative:
@*)

Mult[a$sym___,b$sym_Plus,c$sym___] :=
    Plus @@ (Mult[a$sym, #, c$sym] & ) /@ List @@ b$sym
Mult[a$sym___,Mult[b$sym__,c$sym__],d$sym___] :=
    Mult[a$sym,b$sym,c$sym,d$sym]

(*@
Define rules for factoring out scalars:
@*)

Mult[a$sym___, b$sym_?ScalarQ, c$sym___] :=
    Times[b$sym, Mult[a$sym,c$sym]]
Mult[a$sym___, b$sym_?ScalarQ c$sym__, d$sym___] :=
    Times[b$sym,Mult[a$sym,c$sym,d$sym]]

(*@ 
The function \VerbFcn{NCSort} will sort the operators in a list into % 
canonical order, being careful not to allow non-commuting operators % 
to ``pass'' each other.  The list that you pass to \VerbFcn{NCSort} % 
must contain \emph{only} operators.  How this functions works is % 
probably easiest seen with an example. Suppose the following operators % 
were initialized as follows: %
\[
	\mathrm{\VerbFcn{CreateOperator}}[\{\{I_x, I_y, I_z \}, \{S_x, S_y, S_z \} \}]
\]
Asked to sort the list of operators
\[
	a = \{ S_x, I_y, I_x, S_y \},
\]
we would report the sorted list
\[
	a_{\mathrm{sorted}} = \{ I_y, I_x, S_x, S_y \}.
\]
The $S$ operator, being of a higher phylum that an $I$ operator, should be %
passed through the $I$ operators.  We do not pass $I_x$ to the left of $I_y$ % 
because $I_x$ and $I_y$ do not commute. %
@*)

(*@
In the code below, the statement %
$\mathrm{\VerbFcn{Map}}[\mathrm{\VerbFcn{phylum}}[\#] \&, a]$ will create % 
a list populated by the operators' phyla, in this case $\{2, 1, 1, 2 \}$.  % 
To create $p$ we multiply by the number of operators in the list plus one, % 
5 in this example, and add to this $\{1, 2, 3, 4 \}$.  This gives % 
$p = \{11, 7, 8, 14 \}$.  An operator's ranking in this list depends on % 
both its phylum and on it's location in the original list.  Sorting $p$ will % 
tell us how to order the operators in the original list $a$.  The variable %
 $p_\mathrm{new}$ keeps track of the locations of the elements of $p$ in % 
the \emph{sorted} version of $p$; in this example % 
$p_{\mathrm{new}} = \{3, 1, 2, 4\}$.  We recognize $p_{\mathrm{new}}$ as % 
the order in which the input operators should appear in $a_{\mathrm{sorted}}$. %
@*)

NCSort[a$sym_List] :=
Module[{n$sym, p$sym, a$new$sym, p$new$sym},
    n$sym = Length[a$sym];
    p$sym = (n$sym+1) Map[phylum[#]&, a$sym] + Table[i$sym,{i$sym,1,n$sym}];
    p$new$sym =(Position[Sort[p$sym],#]  [[1,1]])& /@ p$sym;
    a$new$sym =  Table[0,{i$sym,1,n$sym}];
    Do[a$new$sym[[p$new$sym[[i$sym]]]] = a$sym[[i$sym]],{i$sym,1,n$sym}];
    Return[a$new$sym]]

(*@
The function \VerbFcn{SortedMult} is the same as \VerbFcn{Mult} % 
except that it reorders the operators in the call list by applying % 
\VerbFcn{NCSort} before passing the result to \VerbFcn{Mult}. %
@*)

SortedMult[a$sym__] :=
    Mult[Sequence @@ NCSort[List[a$sym]]]

(*@ 
The function \VerbFcn{MultSort} reorders all the operators in % 
a \VerbFcn{NonCommutativeMultiply} call. %
@*)

MultSort[a$sym__] :=
    a$sym /. Mult -> SortedMult

(*~ END ~*)

EndPackage[]

If[$VerboseLoad == True,
    Message[NCSort::usage];
    Message[SortedMult::usage];
    Message[MultSort::usage];
]



