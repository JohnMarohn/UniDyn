(* ::Package:: *)

(*~ START ~*)

vtest[label_,test_] := 
    VerificationTest[test,
        True,
        TestID-> StringJoin[
            "OpCreate > test",
            ToString[label]]]

(*@
The \verb+NCAlgebra+ package reserves roman letters for non-commuting variables % 
by default.  Other lettered variables --- like capital letters, greek letters, and % 
more complicated constructs --- are defined as commuting variables be default.
@*)

Clear[a, \[Alpha], A];

vtest["01a", CommutativeQ[a] == False]
vtest["01b", CommutativeQ[A] == True] 
vtest["01c", CommutativeQ[\[Alpha]] == True] 

(*@
Variables that we have defined as operators return \VerbCmd{False} when % 
queried by \VerbFcn{CommutativeQ[]}.
@*)

Clear[Ix$sym, Iy$sym, Iz$sym, Sx$sym, Sy$sym, Sz$sym];
CreateOperator[{{Ix$sym, Iy$sym, Iz$sym},{Sx$sym, Sy$sym, Sz$sym}}];

vtest["02a", CommutativeQ[Ix$sym] == False]
vtest["02b", CommutativeQ[Sx$sym] == False]

(*@
We can override the default roman-letter behavior by explicitly defining % 
a roman-letter variable to be scalar, e.g., a commutative variable.  Variables % 
that we have defined as scalars return \VerbCmd{True} when queried by % 
\VerbFcn{CommutativeQ[]}:
@*)

Clear[a];
CreateScalar[a];

vtest["02c", CommutativeQ[a] == True]

(*@
Expressions involving a non-commutative operator return \VerbCmd{False} %
when queried by \VerbFcn{CommutativeQ[]}.
@*)

vtest["03a", CommutativeQ[Exp[\[Alpha]]] == True]
vtest["03b", CommutativeQ[Exp[\[Alpha] \[Beta]]] == True]
vtest["03c", CommutativeQ[Exp[\[Alpha] Ix$sym]] == False]

(*@
Operators created in a batch by passing a matrix to the \VerbFcn{CreateOperator[]} %
function are assigned a \emph{phylum} and an \emph{order}.  The phylum and order are % 
numbers determined by an operator's position in the matrix passed to % 
\VerbFcn{CreateOperator[]}.
@*)

vtest["04a", phylum[Ix$sym] == 1]
vtest["04b", order[Ix$sym] == 1]
vtest["04c", phylum[Iy$sym] == 1]
vtest["04d", order[Iy$sym] == 2]
vtest["04e", phylum[Sx$sym] == 2]
vtest["04f", order[Sx$sym] == 1]
vtest["04g", phylum[Ix$sym] != phylum[Sx$sym]]
vtest["04h", order[Ix$sym] == order[Sx$sym]]

(*@
Additional tests of the phylum and order system follow.
@*)

Clear[Ix$sym, Iy$sym, Iz$sym, aL$sym, aR$sym]
CreateOperator[{{Ix$sym, Iy$sym, Iz$sym},{aL$sym, aR$sym}}];

vtest["05a", phylum /@ {Ix$sym, Iy$sym, Iz$sym} === {1, 1, 1}]
vtest["05b", phylum /@ {aL$sym, aR$sym} === {2, 2}]
vtest["05c", order /@ {Ix$sym, Iy$sym, Iz$sym} === {1, 2, 3}]
vtest["05d", order /@ {aL$sym, aR$sym} === {1, 2}]

(*@
The \verb+NCAlgebra+ package uses a double asterisk to represent % 
non-commutative multiplication.
@*)

vtest["06a", Not[aL$sym**aR$sym === aL$sym aR$sym]]
vtest["06b", Not[aL$sym**aR$sym === aR$sym**aL$sym]]

(*~ END ~*)

Clear[a, \[Alpha], b, \[Beta], A];
Clear[Ix$sym, Iy$sym, Iz$sym, Sx$sym, Sy$sym, Sz$sym];
Clear[aR$sym, aL$sym];






