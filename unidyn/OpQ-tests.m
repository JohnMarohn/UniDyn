(* ::Package:: *)

(**
 ** OpQ-tests.m
 ** Operator Queries Unit Tests  
 ** John Marohn
 **)

Clear[a$sym, b$sym, c$sym, d$sym];
Clear[Ix$sym, Iy$sym, Iz$sym, Sx$sym, Sy$sym, Sz$sym];
Clear[aR,aL];

(*~ START ~*)

(*@ Create a shorthand function for creating unit tests.
@*)

If[$VersionNumber < 10.,

  vtest[label_,test_] :=
    If[test === True, 
      Print["Pass"],
      Print["Fail > ", StringJoin["OpQ > test",ToString[label]]]],

  vtest[label_,test_] := 
      VerificationTest[test,
          True,
          TestID-> StringJoin[
              "OpQ > test",
              ToString[label]]]
]

(*@
Any variable will test \VerbCmd{True} when queried by %
\VerbFcn{ScalarQ[]}, whether it has been defined as a scalar or not.
@*)

vtest["1", ScalarQ[a$sym] == True]
vtest["2", CreateScalar[b$sym];
    ScalarQ[b$sym] == True]

(*@
For \VerbFcn{OperatorQ[]} to return \VerbCmd{True}, in contrast, % 
the variable must have been created as an operator first. 
@*)

vtest["3", OperatorQ[c$sym] == False]
vtest["4", CreateOperator[d$sym];
    OperatorQ[d$sym] == True]

(*@
Any expression containing an operator will be queried \VerbCmd{True} % 
by \VerbFcn{OperatorQ[]}.  This is so when the expression contains % 
only an operator or when the expression contains any combination of % 
scalars and operators.  The only case in which \VerbFcn{OperatorQ[]} % 
returns \VerbCmd{False} is is it is passed an expression containing only scalars.
@*)

vtest["5", OperatorQ[Times[Exp[d$sym],d$sym]] == True]
vtest["6", OperatorQ[Times[b$sym,d$sym]] == True]
vtest["7", OperatorQ[Times[a$sym,b$sym]] == False]

(*@
Operators created in a batch using
\[
    \text{\VerbFcn{CreateOperator[matrix]}}
\]
are assigned a \emph{phylum} and an \emph{order} which is determined by their position in the matrix.
@*)

CreateOperator[{{Ix$sym, Iy$sym, Iz$sym},{Sx$sym, Sy$sym, Sz$sym}}];

vtest["8", phylum[Ix$sym] == 1]
vtest["9", order[Ix$sym] == 1]
vtest["10", phylum[Iy$sym] == 1]
vtest["11", order[Iy$sym] == 2]
vtest["12", phylum[Sx$sym] == 2]
vtest["13", order[Sx$sym] == 1]
vtest["14", phylum[Ix$sym] != phylum[Sx$sym]]
vtest["15", order[Ix$sym] == order[Sx$sym]]

(*@
Another, more stringent test of the \emph{phylum} and \emph{order} system:
@*)

CreateOperator[{{Ix$sym, Iy$sym, Iz$sym},{aL$sym, aR$sym}}];

vtest["16", Not[Mult[aL$sym, aR$sym] === aL$sym aR$sym]]
vtest["17", phylum /@ {Ix$sym, Iy$sym, Iz$sym} === {1, 1, 1}]
vtest["18", phylum /@ {aL$sym, aR$sym} === {2, 2}]
vtest["19", order /@ {Ix$sym, Iy$sym, Iz$sym} === {1, 2, 3}]
vtest["20", order /@ {aL$sym, aR$sym} === {1, 2}]

(*@
Very that the operators created in a big list are in fact %
recognized as operators.
@*)

vtest["21", OperatorQ /@ {a$sym, Ix$sym, aL$sym} == {False, True, True}]

(*~ END ~*)

Clear[a$sym, b$sym, c$sym, d$sym];
Clear[Ix$sym, Iy$sym, Iz$sym, Sx$sym, Sy$sym, Sz$sym];
Clear[aR, aL];











