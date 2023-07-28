(* benchmart the wick and wickHT, i.e with hashtable version *)

g0=Table[g/(i+j),{i,16},{j,16}];
ht=initExptHT[g0];
op1=mul[op["HOP",1,2],op["HOP",3,4],op["HOP",5,6],op["HOP",7,8],op["HOP",10,9],op["HOP",13,14]]
op1=mul[op["HOP",3,4],op["HOP",7,8]]
AbsoluteTiming[op1check=wick[op1,g0];]
AbsoluteTiming[op1checkht=wickHT[op1,ht];]
a=(op1check-op1checkht)//FullSimplify;

(*  *)



			 

