(* ::Package:: *)

BeginPackage["MyPackage`"]
my$function::usage="f(a,b) returns a^2 + b"
Begin["Private`"]
my$function[a_,b_] := (c = a^2; Return[b+c])
End[]
EndPackage[]

s = my$function[2,3];
s (* <== returns 7 *)
c (* <== returns c *)




