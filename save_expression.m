(* save expressions *)
(* results, {name:expression} *)
(* toString is defined in wick_v1 *)
(* mainly update for Subscription["a",i,j] *)
(* see /home/chengzhengqian/Documents/research/integer_time_solver/src/one_band_general_n3.m L63
 *)
(* fix bug, put all local variables in module list, like i,j,k *)
saveExpression[directory_,name_,args_,results_]:=Module[
	{s,i},
	s=OpenWrite[directory<>"/"<>name<>".jl"];
	argsStr=StringRiffle[args,","];
	WriteString[s,"function "<>name<>"("<>argsStr<>")\n"];
	Do[
		writeGeneral[s,results[[i]]]
	       ,{i,1,Length[results]}
	];
	WriteString[s,StringRiffle[Table[results[[i,1]],{i,1,Length[results]}],","]];
	WriteString[s,"\nend\n"];
	Close[s]];

(* name="test"; *)
(* args={"c","d"}; *)
(* results={{"a",c+d},{"b",c-d}}; *)
(* directory="/home/chengzhengqian/Documents/research/integer_time_solver/src/gene_code"; *)
(* CreateDirectory[directory]; *)

(* we also need to read the case where the values in results are matrix or vector *)
(* Dimensions[a]; *)
(* Dimensions[{a}]; *)
(* Dimensions[{{a}}]; *)
(* check "/home/chengzhengqian/Documents/research/vdat/hubbard_one_band_analytic/src/summarize_G_n3_utils.m"
 *)
(* result={"a",1}; *)
writeExpr[s_,result_]:=Module[
	{name=result[[1]],
	 value=result[[2]]},
	WriteString[s,ToString[name]<>"=("<>toString[value]<>")\n"]];

(* result={"a",{1,2}}; *)

writeVector[s_,result_]:=Module[
	{name=result[[1]],
	 value=result[[2]],w,widx},
	w=Length[value];
	WriteString[s,ToString[name]<>"="];
	WriteString[s,"["];
	(* write vector *)
	Do[WriteString[s,"("<>toString[value[[widx]]]<>"),  "],
	   {widx,1,w}];
	WriteString[s,"]\n"]];

(* matrix *)
writeMatrix[s_,result_]:=Module[
	{name=result[[1]],
	 value=result[[2]],h,w,widx,hidx},
	{h,w}=Dimensions[value];
	WriteString[s,ToString[name]<>"="];
	WriteString[s,"["];	
	(* write matrix *)
	Do[
		Do[
			WriteString[s,
				    "("<>toString[value[[hidx,widx]]]<>")  "
			],
			{widx,1,w}];
		WriteString[s,";\n"  ],
		{hidx,1,h}];
	WriteString[s,"]\n"]];
	


(* result={a,1} *)
(* combine these cases *)
writeGeneral[s_,result_]:=Switch[
	Length[Dimensions[result[[2]],AllowedHeads->List]],
	0,writeExpr[s,result],
	1,writeVector[s,result],
	2,writeMatrix[s,result]];



(* result=results[[2]] *)

(* Switch[1,0,Print[0],1,Print[1]]; *)


