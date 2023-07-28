(* see https://stackoverflow.com/questions/6367932/generate-a-list-in-mathematica-with-a-conditional-tested-for-each-element *)
Clear[tableGen];
tableGen[f_, iter : {i_Symbol, __}, addif : Except[_List] : (True &)] :=
 Module[{sowTag},   
  If[# === {}, #, First@#] &@
       Last@Reap[Do[If[addif[#], Sow[#,sowTag]] &[f[i]], iter],sowTag]];
(* tableGen[Prime, {i, 3, 10}, PrimeQ[# + 2]&] *)
ClearAll[tableGenAlt];
SetAttributes[tableGenAlt, HoldAll];
tableGenAlt[expr_, iter_List, addif : Except[_List] : (True &)] :=
 Module[{sowTag}, 
  If[# === {}, #, First@#] &@
    Last@Reap[Do[If[addif[#], Sow[#,sowTag]] &[expr], iter],sowTag]];

(* tableGenAlt[Prime[i], {i, 3,10}, PrimeQ[# + 2] &] *)
(* there are some problems *)
(* there are some problem in replacing the code *)
