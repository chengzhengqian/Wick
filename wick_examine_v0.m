Get["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick.m"]
Get["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wicktex.m"]


c=.;
g0s=c;
g0n3=Table[Subscript[g0s,i,j],{i,1,3},{j,1,3}];
g0n3full=geneGFromSpinUp[g0n3];

u=.;

proj[i_]:=add[op["C",1+u/4],
	      mul[op["C",-u/2],op["NT",i]],
	      mul[op["C",u],op["H",i]]];

dm[i_,j_]:=op["HOP",i,"UP",j,"UP"];

proj1=proj[1];
proj2=proj[2];
p=mul[proj1,proj2];
d3=op["H",3];

pval=wick[p,g0n3full]//FullSimplify;
oldresults={proj1,proj2,p,d3,pval};
Save["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick_examine_v0_save.m",oldresults];

(* so most part are correct, To narrow down the problem, we generate a matrix *)

projComponent[i_]:={op["C",1],
		    op["HOP",i,"UP",i,"UP"],
		    op["HOP",i,"DN",i,"DN"],
		    mul[op["HOP",i,"UP",i,"UP"],op["HOP",i,"DN",i,"DN"]]
		   };

proj1c=projComponent[1];
proj2c=projComponent[2];
(* pval component *)
i=1;j=1
pcval=Table[
	(wick[mul[proj1c[[i]],proj2c[[j]]],g0n3full]//FullSimplify),
	{i,1,4},
	{j,1,4}];
pcvalold=pcval;
Save["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick_examine_v0_save_pcval.m",pcvalold];





