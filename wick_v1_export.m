(* we rename export to exportXX for given backend *)
(* so we need to export these to julia file *)
(* we first need to overload CForm *)
(* Subscript["a",1]//CForm; *)
(* mainly, for Subscript["tmp",___] and "n", ..etc *)
Format[Subscript[x_String,idx__],CForm]:=x[idx];

toString[expr_]:=StringReplace[ToString[CForm[expr]],"\""->""];

(* add 0 for float number *)
Unprotect[Real];
Format[r_Real, CForm] := Format[
	If[StringEndsQ[ToString[N[r]],"."],ToString[N[r]]<>"0",ToString[N[r]]],
	OutputForm]
Protect[Real];		   


(* export to julia file *)
exportJulia[tp_tape,name_,args_,results_,outputDir_]:=Module[
	{file,tmp,num,output,i},
	file=OpenWrite[outputDir<>"/"<>name<>".jl"];
	WriteString[file,"function "<>name<>"("<>StringRiffle[args,","]<>")\n"];
	tmp=getTmp[tp];
	output=getOutput[tp];
	num=tmp["Lookup","num"];
	(* WriteString[file,"tmp=zeros("<>ToString[num]<>")\n"]; *)
	WriteString[file,
		    "\ttmp=Array{Float64,1}(undef,"<>ToString[num]<>")\n"];
	Do[
		WriteString[file,
			    "\ttmp["<>ToString[i]<>"]="
				  <>toString[tmp["Lookup",Subscript["tmp",i]]]<>"\n"]
	       ,{i,1,num}];
	Map[(WriteString[file,"\t"<>#<>"="<>toString[output["Lookup",#]]<>"\n"])&
	   ,results];
	WriteString[file,"\t["<>StringRiffle[results,","]<>"]\nend\n"];
	Close[file];];



(* we first just split functions *)
(* we have tried to using the parallel, but there is actually a overhead, so we first just split the task of computing the temp  array
 This should help to reduce the compile time for long functions*)
(* see
 /home/chengzhengqian/Documents/research/integer_time_solver/src/one_band_wick_v1_general_n5_G.m L86
for some script to general parallel code
 *)

exportJulia[tp_tape,name_,args_,results_,outputDir_,ninterval_]:=Module[
	{file,tmp,output,ntmp,npart,argswithtmp,i,j,splitlist},
	tmp=getTmp[tp];
	output=getOutput[tp];
	file=OpenWrite[outputDir<>"/"<>name<>".jl"];
	ntmp=tmp["Lookup","num"];
	npart=Ceiling[ntmp/ninterval];
	argswithtmp=Join[args,{"tmp"}];
	splitlist=Table[{ninterval*(i-1)+1,Min[ninterval*i,ntmp]},{i,1,npart}];
	(* write the main function *)
	WriteString[file,"function "<>name<>"("<>StringRiffle[args,","]<>")\n"];
	WriteString[file,
		    "tmp=Array{Float64,1}(undef,"<>ToString[ntmp]<>")\n"];
	Do[
		WriteString[file,"cal_"<>name<>"_n_"<>ToString[i]<>"("<>StringRiffle[argswithtmp,","]<>")\n"];
	       ,{i,1,npart}
	];
	Map[(WriteString[file,#<>"="<>toString[output["Lookup",#]]<>"\n"])&
	   ,results];
	WriteString[file,"["<>StringRiffle[results,","]<>"]\nend\n"];
	(* write the sub functions *)
	Do[
		WriteString[file,"function cal_"<>name<>"_n_"<>ToString[i]<>"("<>StringRiffle[argswithtmp,","]<>")\n"];
		Do[
			WriteString[file,
				    "tmp["<>ToString[j]<>"]="
					  <>toString[tmp["Lookup",Subscript["tmp",j]]]<>"\n"]
		       ,{j,splitlist[[i,1]],splitlist[[i,2]]}
		];
		WriteString[file,"end\n\n"];
	       ,{i,1,npart}
	];
	Close[file];];



(* put this part in v1 *)
(* export[tp,name,args,results,outputDir]; *)
Format[Subscript[x_String,idx__],FortranForm]:=x[idx];
toFString[expr_]:=StringReplace[ToString[FortranForm[expr]],"\""->""];

(* export to Fortran, we should name the function to indicate it is either julia for fortran*)
exportFortran[tp_tape,name_,argsinput_,argsdim_,results_,outputDir_]:=Module[
	{file,tmp,num,output,i,ffile,sofile,shfile,nresults,args,declaration,nargs,nargsinput},
	(* write .f90 *)
	nargsinput=Length[argsinput];
	args=Join[argsinput,{"tmp","results"}];
	nargs=Length[args];
	tmp=getTmp[tp];
	output=getOutput[tp];
	num=tmp["Lookup","num"];
	nresults=Length[results];
	declaration="implicit none\n"<>StringJoin[Table["double precision, intent(in) ::"<>argsinput[[i]]<>"("<>StringRiffle[argsdim[[i]],","]<>")\n",{i,1,nargsinput}]]<>"\ndouble precision, intent(out) :: results("<>ToString[nresults]<>")\ndouble precision, intent(inout) :: tmp("<>ToString[num]<>")
";
	ffile=outputDir<>"/"<>name<>".f90";
	sofile=outputDir<>"/"<>name<>".so";
	file=OpenWrite[ffile];
	WriteString[file,"subroutine "<>name<>"("<>StringRiffle[args,","]<>")\n"];
	WriteString[file,declaration];
	(* compute tmp *)
	Do[
		WriteString[file,
			    "tmp("<>ToString[i]<>")="
				  <>toFString[tmp["Lookup",Subscript["tmp",i]]]<>"\n"]
	       ,{i,1,num}];
	(* write to results *)
	Do[
		WriteString[file,
			    "results("<>ToString[i]<>")="
				  <>toFString[output["Lookup",results[[i]]]]<>"\n"]
		,{i,1,nresults}
	];
	WriteString[file,"end subroutine "<>name<>"\n"];
	Close[file];
	(* write compile script *)
	shfile=outputDir<>"/"<>name<>".sh";
	file=OpenWrite[shfile];
	(* ensure gfortran takes the whole line *)
	WriteString[file, "gfortran -ffree-line-length-0 -O3  -shared -fPIC -march=native "<>ffile<>" -o "<>sofile];
	Close[file];
	(* finally, we write the julia wrap *)
	file=OpenWrite[outputDir<>"/"<>name<>".jl"];
	WriteString[file,"function "<>name<>"("<>StringRiffle[argsinput,","]<>")\n"];
	WriteString[file,"results=zeros("<>ToString[nresults]<>")\ntmp=zeros("<>ToString[num]<>")\n"];
	WriteString[file,"\nccall((:"<>name<>"_,\""<>sofile<>"\"),Cvoid,("<>StringRiffle[Table["Ref{Float64}",{i,1,nargs}],","]<>"),"<>StringRiffle[args,","] <>")"];
	WriteString[file,"\nresults\nend\n"];
	Close[file];
	(* run the compile script *)
	Print["compile ...\n"];
	Run["sh "<>shfile];
	Print["complete\n"];
							      ];





(* export to fortran and split the code to evaluate the tmp array into multiple files *)
(* write the script  *)
(* num number of temps, type, type of matrix elements *)
declarationWithResults[num_,argsinput_,argsdim_,nresults_,type_]:=Module[
	{nargsinput},
	nargsinput=Length[argsinput];	
	("implicit none\n\n"
				      <>StringJoin[Table[type<>", intent(in) ::"<>argsinput[[i]]<>"("<>StringRiffle[argsdim[[i]],","]<>")\n",{i,1,nargsinput}]]<>
				      "\n"<>type<>", intent(out) :: results("
				      <>ToString[nresults]<>
				      ")\n"<>type<>", intent(inout) :: tmp("
				      <>ToString[num]<>")"
	)];

declarationWithNoResults[num_,argsinput_,argsdim_,type_]:=Module[
	{nargsinput},
	nargsinput=Length[argsinput];	
	("implicit none\n\n"
				      <>StringJoin[Table[type<>", intent(in) ::"<>argsinput[[i]]<>"("<>StringRiffle[argsdim[[i]],","]<>")\n",{i,1,nargsinput}]]<>
				      "\n"<>type<>", intent(inout) :: tmp("
				      <>ToString[num]<>")"
	)];



(* declarationWithResults[4,{"a","b"},{{2,2},{1}},4,"double precision"]; *)
(* declarationWithNoResults[4,{"a","b"},{{2,2},{1}},"double precision"]; *)
(* bodyFn takes the filename handle and append the body *)
writeFortranFunc[outputDir_,name_,args_,declaration_,bodyFn_]:=Module[
	{filename,file},
	filename=outputDir<>"/"<>name<>".f90";
	file=OpenWrite[filename];
	WriteString[file,"subroutine "<>name<>"("<>StringRiffle[args,","]<>")\n"];
	WriteString[file,declaration<>"\n"];
	(* write body *)
	bodyFn[file];
	WriteString[file,"\nend subroutine "<>name<>"\n"];
	Close[file]];

(* writeFortranFunc["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica","test",{"a","b","c"},declarationWithResults[4,{"a","b"},{{2,2},{1}},4,"double precision"],(WriteString[#,"\nthis is a body test\n"])&]; *)

(* compute tmp between tmpStart and tmpEnd (both included)*)
tmpcallName[name_,tmpStart_,tmpEnd_]:=name<>"_"<>ToString[tmpStart]<>"_"<>ToString[tmpEnd];

writeFortranTmp[tp_tape,name_,argsinput_,argsdim_,outputDir_,tmpStart_,tmpEnd_,type_]:=Module[
	{funcname,args,tmp,num,i},
	funcname=tmpcallName[name,tmpStart,tmpEnd];
	args=Join[argsinput,{"tmp"}];
	tmp=getTmp[tp];
	num=tmp["Lookup","num"];
	writeFortranFunc[outputDir, funcname,args,
			 declarationWithNoResults[num,argsinput,argsdim,type],
			 (
				 Do[
					 WriteString[#,
						     "tmp("<>ToString[i]<>")="
				  <>toFString[tmp["Lookup",Subscript["tmp",i]]]<>"\n"]
					,{i,tmpStart,tmpEnd}]
			 )&
	]];


(* now, we implement split function,  *)
splitList[ntmp_,ninterval_]:=Module[
	{npart},
	npart=Ceiling[ntmp/ninterval];
	Table[{ninterval*(i-1)+1,Min[ninterval*i,ntmp]},{i,1,npart}]];



(* splitList[111,20] *)
(* writefortrantmp[tp,"test",argsinput,argsdim,"/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica",2,5,"double precision"] *)

(* Do[Print[i],{i,1,2}] *)

(* we implement the export for Fortran with split *)
exportFortran[tp_tape,name_,argsinput_,argsdim_,results_,outputDir_,ninterval_]:=Module[
	{splitlist,tmp,num,type,i,argstmp,argsmain,declarationmain,namemain,nresults,argstmpstr,output},
	(* export this to parameter in the future *)
	type="double precision";
	tmp=getTmp[tp];
	(* number of tmp terms *)
	num=tmp["Lookup","num"];
	nresults=Length[results];
	splitlist=splitList[num,ninterval];
	output=getOutput[tp];
	(* first, generate the split calls *)
	Do[
		writeFortranTmp[tp,name,argsinput,argsdim,outputDir,splitlist[[i,1]],splitlist[[i,2]],type],
		{i,1,Length[splitlist]}];
	(* now, we need to generate the main call *)
	argsmain=Join[argsinput,{"tmp","results"}];
	declarationmain=declarationWithResults[num,argsinput,argsdim,nresults,type];
	namemain=name<>"_main";
	argstmp=Join[argsinput,{"tmp"}];
	argstmpstr=StringRiffle[argstmp,","];
	writeFortranFunc[outputDir,namemain,argsmain,declarationmain,
			 (
				 (* write the call to runs for split tmp array , and results*)
				 Do[
				 WriteString[#,"call "<>tmpcallName[name,splitlist[[i,1]],splitlist[[i,2]]]<>" ("<>argstmpstr<>")\n"],
				 {i,1,Length[splitlist]}];
				 Do[
					 WriteString[#,
						     "results("<>ToString[i]<>")="
							       <>toFString[output["Lookup",results[[i]]]]<>"\n"]
					,{i,1,nresults}
				 ]
			 )&
	];
										 ];
