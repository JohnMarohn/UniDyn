(* ::Package:: *)

BeginPackage["UniDyn`",{"Global`","OpCreate`"}]

$VerboseLoad::usage="True or False.  Whether to print out descriptions of key functions when loading the package."

If[ValueQ[$VerboseLoad] == False,
    $VerboseLoad = False];

EndPackage[]



