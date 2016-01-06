(* ::Package:: *)

Off[Needs::nocont]
BeginPackage["UniDyn`",{"Global`","NC`","NCAlgebra`","OpCreate`","Mult`","Comm`","Spins`","Osc`","Evolve`"}]

$VerboseLoad::usage="True or False.  Whether to print out descriptions of key functions when loading the package."

If[ValueQ[$VerboseLoad] == False,
    $VerboseLoad = False];

EndPackage[]
On[Needs::nocont]








