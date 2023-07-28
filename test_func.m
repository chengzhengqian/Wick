(* play with various data structure *)
ds = CreateDataStructure["HashTable"];
Timing[Do[ds["Insert",i->i],{i,1,10^6}]];
map=<||>;
Timing[Do[map[i]=i,{i,1,10^6}]];
Clear[map,ds];

