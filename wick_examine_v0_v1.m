(* here, we examine both version, right now, they give differenent result, we try to fix this problem *)

Get["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick_v1.m"];
Get["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick_v1_tex.m"];


Get["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick_examine_v0_save.m"];

oldresults[[1]];
(* oldresults={proj1,proj2,p,d3,pval}; *)

dm[i_,j_,spin_]:=op[{{spin,i}},{{spin,j}}];
u=Subscript["u",1];
proj[t_]:=op[1+u/4]+op[-u/2].(dm[t,t,up]+dm[t,t,dn])+op[u].dm[t,t,up].dm[t,t,dn];


proj1=proj[1];
proj2=proj[2];
p=proj1.proj2;
d3=dm[3,3,up].dm[3,3,dn];
oldproj1=oldresults[[1]];
oldproj2=oldresults[[2]];
oldproj=oldresults[[3]];
oldd3=oldresults[[4]];
oldp=oldresults[[5]];
(* we need to convert old form to new form *)
ClearAll[convert];
convert[x_Integer]:=Which[EvenQ[x],{dn,x/2},
			  OddQ[x],{up,(x+1)/2}];

convert[fd[x_]]:=op[{convert[x]},{}];
convert[f[x_]]:=op[{},{convert[x]}];
convert[terms_List]:=Module[
	{result=op[1],nterm=Length[terms]},
	Do[result=result.convert[terms[[i]]],{i,1,nterm}];result];
convert[prod[c_,terms_]]:=op[c].convert[terms];
convert[sum[terms___]]:=Plus @@ Map[convert,List[terms]];

convert[oldproj1]+op[-1].proj1
convert[oldproj2]+op[-1].proj2
convert[oldproj]+op[-1].p
convert[oldd3]+op[-1].d3

c=.;
(* for comparison, we use c instead of "c" *)
diagg0[uop[{{spin_,i_}},{{spin_,j_}}]]:=Subscript[c,i,j];
diagg0[x_uop]:=0;

tp=tape[diagg0];
eval[p,"p",tp];


pnew=getOutput[tp]["Lookup","p"];
rules=Normal[Normal[getTmp[tp]]];
pnew=(pnew//.rules)//FullSimplify;

differencep=(oldp-pnew)//FullSimplify;

(* so there is a single term  *)








