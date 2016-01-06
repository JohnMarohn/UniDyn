(* ::Package:: *)

(**
 ** Mult-tests.m
 ** Multiplication and sorting unit tests  
 ** John Marohn
 ** 2016/01/05
 **)

vtest[label_,test_] := 
    VerificationTest[test,
        True,
        TestID-> StringJoin[
            "OpQ > test",
            ToString[label]]]
 
Clear[Ix$sym, Iy$sym, Iz$sym,Sx$sym, Sy$sym, Sz$sym];
Clear[a$sym, b$sym, c$sym, d$sym];

(*~ START ~*)

(*@
First define some operators and scalars.
@*)

CreateOperator[{{Ix$sym, Iy$sym, Iz$sym},{Sx$sym, Sy$sym, Sz$sym}}];
CreateScalar[{a$sym, b$sym, c$sym, d$sym}];

(*@
In the following test, the left hand side should resolve to % 
\VerbFcn{NonCommutativeMultiply}$[I_x,I_y]$ while the % 
right-hand side should resolve to \VerbFcn{Times}$[I_x,I_y]$. %
These are \emph{not} the same.
@*)

vtest["01a", Not[NonCommutativeMultiply[Ix$sym,Iy$sym] === Ix$sym Iy$sym]]

(*@
Before continuing, define a shorthand:
@*)

Mult = NonCommutativeMultiply;

(*@
Test that \VerbFcn{NonCommutativeMultiply} distributes over addition and is associative.  % 
Note how Mult handles products of scalars and products of operators % 
differently.  Below we test for sameness (\verb+===+) instead of equality (\verb+==+).  % 
This is because if the two sides are \emph{not} the same, then the equality % 
test (using \verb+==+) is undefined --- neither true nor false --- and the unit test % 
does not fail as we wish. %
@*)

vtest["02a", Not[Mult[a$sym, b$sym + c$sym] === a$sym b$sym + a$sym c$sym]]
vtest["02b", Not[Mult[a$sym + b$sym, c$sym] === a$sym c$sym + b$sym c$sym]]

(*@
To get \VerbFcn{NonCommutativeMultiply[]} to distribute, we have to % 
call \VerbFcn{NCExpand[]}.   
@*)

vtest["03a", NCExpand[Mult[a$sym, b$sym + c$sym]] === a$sym b$sym + a$sym c$sym]
vtest["03b", NCExpand[Mult[a$sym + b$sym, c$sym]] === a$sym c$sym + b$sym c$sym]

vtest["04a", NCExpand[Mult[Ix$sym, Iy$sym + Iz$sym]] === Mult[Ix$sym, Iy$sym] + Mult[Ix$sym, Iz$sym]]
vtest["04b", NCExpand[Mult[Ix$sym + Iy$sym, Iz$sym]] === Mult[Ix$sym, Iz$sym] + Mult[Iy$sym, Iz$sym]]

(*@
Here is an example where we apparently do \emph{not} have to call \VerbFcn{NCExpand[]}.
@*)

vtest["05a", Mult[Ix$sym, Mult[Sx$sym, Sy$sym], Iz$sym] === Mult[Ix$sym, Sx$sym, Sy$sym, Iz$sym]]

(*@
Test that scalars get factored out properly.  Note that we do \emph{not} have to call % 
\VerbFcn{NCExpand[]} for \VerbFcn{NonCommutativeMultiply[]} to pull scalars out front. %
@*)

vtest["06a", Mult[Ix$sym, 2 I a$sym, Sx$sym] === a$sym 2 I Mult[Ix$sym,Sx$sym]]
vtest["06b", Mult[Ix$sym, Mult[a$sym, Iy$sym], Mult[b$sym, Sx$sym]] === a$sym b$sym Mult[Ix$sym, Iy$sym, Sx$sym]]

(*@
Test our sorting function:
@*)

vtest["07a", NCSort[{Sx$sym,Ix$sym, Iy$sym, Sz$sym}] === {Ix$sym, Iy$sym, Sx$sym, Sz$sym}]

(*@
The function \VerbFcn{MultSort[]} is used to order the operators in a standing %
\VerbFcn{NonCommutatativeMultiply[]} function. %
@*)

vtest["08a", SortedMult[Iy$sym, Sx$sym, Ix$sym] === Mult[Iy$sym, Ix$sym, Sx$sym]]
vtest["08b", MultSort[Mult[Iy$sym, Sx$sym, Ix$sym]] === Mult[Iy$sym, Ix$sym, Sx$sym]]

(*@
If we now define the operators to have a different natural order, then the above %
tests fail.  This confirms that the operators are being sorted according to our rules. %
@*)

CreateOperator[{{Sx$sym, Sy$sym, Sz$sym},{Ix$sym, Iy$sym, Iz$sym}}];
vtest["08b", Not[SortedMult[Iy$sym, Sx$sym, Ix$sym] === Mult[Iy$sym, Ix$sym, Sx$sym]]]
vtest["08d", Not[MultSort[Mult[Iy$sym, Sx$sym, Ix$sym]] === Mult[Iy$sym,Ix$sym,Sx$sym]]]

(*@
Check that \VerbFcn{MultSort[]} works as expected when scalars are peppered into % 
the list of operators being multiplied. %
@*)

vtest["09a", MultSort[Mult[a$sym Iy$sym, Sx$sym, Ix$sym]] === a$sym NonCommutativeMultiply[Sx$sym,Iy$sym, Ix$sym]]

(*@
This is a pretty fancy test showing that the \verb+NCAlgebra+ package can factor out an % 
operator.  For the following test to work, \VerbFcn{MultSort[]}, \VerbFcn{NCExpand[]}, %
and \VerbFcn{NCCollect[]} all have to work correctly. %
@*)

expr1 = Mult[Sx$sym, a$sym Sz$sym, Ix$sym] + Mult[Ix$sym, b$sym Sx$sym];
expr2 = NCCollect[MultSort[NCExpand[expr1]], Ix$sym];
expr3 = (a$sym Sx$sym**Sz$sym + b$sym**Sx$sym)**Ix$sym ;

vtest["10a", expr2 === expr3]

(*~ END ~*)

Clear[Ix$sym, Iy$sym, Iz$sym,Sx$sym, Sy$sym, Sz$sym];
Clear[a$sym, b$sym, c$sym, d$sym];



