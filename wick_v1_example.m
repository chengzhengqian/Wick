(* Document the v1 api*)
(* !! we change the export to exportXX and update this documents *)
Get["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick_v1.m"];
Get["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick_v1_tex.m"];
Get["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick_v1_format.m"];
Get["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick_v1_export.m"];


(* create operator *)
i=.;
j=.;
k=.;
l=.;
op1=op[{i},{j}];
op2=op[{k},{l}];
op3=op1.op2+op[-1].op2.op1;

(*commutator, as operator derivative for bosonic  case *)
cm[op[{},{i}],op[{i},{j}]];
(*anti-commutator, as operator derivative for  ferminc case *)
am[op[{},{i}],op[{j},{}]];

op[{i,j},{k,l}];
op[{j,i},{k,l}];
op[{i,i},{k,l}];
(* key function *)
Signature[{1,2}];		(*1*)
Signature[{2,1}]; 		(*-1*)
Signature[{2,2}]; 		(*0*)

(* now, we illustrate how to use the wick theorem *)
(* see /home/chengzhengqian/Documents/research/integer_time_solver/src/one_band_wick_v1_general_n3.m L1
 *)
(* /home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick_v1_example.m L32 *)

(* example for one band case *)

dm[i_,j_,spin_]:=op[{{spin,i}},{{spin,j}}];
u=Subscript["u",1];
proj[t_]:=op[1+u/4]+op[-u/2].(dm[t,t,up]+dm[t,t,dn])+op[u].dm[t,t,up].dm[t,t,dn];


p1=proj[1];
p2=proj[2];
p=p1.p2;
d3=dm[3,3,up].dm[3,3,dn];

(* now, evaluate using the Wick theorem *)
(* defining the input Green's function *)
ClearAll[diagg0];
diagg0[uop[{{spin_,i_}},{{spin_,j_}}]]:=Subscript["g",i,j];
diagg0[x_uop]:=0;

tp=tape[diagg0];
eval[p,"p",tp];
eval[p.d3,"d",tp];
gij[i_,j_]:="g"<>ToString[i]<>"_"<>ToString[j];
Table[eval[p.dm[i,j,up],gij[i,j],tp],{i,1,3},{j,1,3}];

(* explore tape structure*)
getGfn[tp];
getTmp[tp];
getOutput[tp];
getTmp[tp]["Lookup","num"];


(* export to julia *)
results=Join[{"p","d"},ArrayReshape[Table[gij[i,j],{j,1,3},{i,1,3}],9]];
(* outputDir="/home/chengzhengqian/Documents/research/integer_time_solver/src/gene_code"; *)
outputDir="/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/gene_code";
name="wick_v1_example_one_band_diag_n3";
args={"g","u"};
(* export[tp,name,args,results,outputDir]; *)
exportJulia[tp,name,args,results,outputDir];


(* export to julia, this just split code for computing tmp array, no parallelization, but this should help to reduce compile time *)
ninterval=10;
namesplit=name<>"_split_"<>ToString[ninterval];
exportJulia[tp,namesplit,args,results,outputDir,ninterval];

(* export to Fortran *)
argsinput={"g","u"};
argsdim={{3,3},{1}};
namefortran=ToLowerCase[name<>"_fortran"];
exportFortran[tp,namefortran,argsinput,argsdim,results,outputDir];

(* test the split *)
argsinput={"g","u"};
argsdim={{3,3},{1}};
namefortran=ToLowerCase[name<>"_fortran_split"];
ninterval=30;
exportFortran[tp,namefortran,argsinput,argsdim,results,outputDir,ninterval];



(* finally, export the final expression *)

Get["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/save_expression.m"]
outputDir="/home/chengzhengqian/Documents/research/integer_time_solver/src/gene_code";
name="wick_v1_example_one_band_diag_n3_p_save_expression";
args={"g","u"};
pval=eval["p",tp];
(* must in the following format *)
results={{"pval",pval}};
saveExpression[outputDir,name,args,results];











