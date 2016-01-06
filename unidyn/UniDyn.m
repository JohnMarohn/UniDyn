(* ::Package:: *)

BeginPackage["UniDyn`",{"Global`","OpCreate`","Mult`","Comm`"}]

$VerboseLoad::usage="True or False.  Whether to print out descriptions of key functions when loading the package."

If[ValueQ[$VerboseLoad] == False,
    $VerboseLoad = False];

EndPackage[]






