(* set format *)

Format[tape[fn_,tmp_,output_],OutputForm]:="<tape fn:"<>ToString[fn]<>", nsteps:"<>ToString[tmp["Lookup","num"]]<>", output:"<>StringRiffle[output["Keys"],","]<>">\n";

Format[mop[ht_],OutputForm]:="<mop nterms:"<>ToString[ht["Length"]]<>">\n";


