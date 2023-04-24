Get["wick.m"];

g0s=\[ScriptCapitalG];

(* general form *)
(* non-interacting Green's function *)
g0n3=Table[Subscript[g0s,i,j],{i,1,3},{j,1,3}];
(* just for test *)
g0n3={{0.5,0.5,0.5},{-0.5,0.5,0.5},{-0.5,-0.5,0.5}};

g0n3full=geneGFromSpinUp[g0n3];

(* density matrix *)
dm[i_,j_]:=op["HOP",i,"UP",j,"UP"];
(* projector for given time step *)
u=.;
(* \mu=-u/2, using Eq 160 from https://journals.aps.org/prb/pdf/10.1103/PhysRevB.103.195138 *)
proj[i_]:=add[op["C",1+u/4],
    mul[op["C",-u/2],op["NT",i]],
    mul[op["C",u],op["H",i]]]

p=mul[proj[1],proj[2]];
pval=wick[p,g0n3full]//FullSimplify;
(* not normalized *)
gnval=Table[FullSimplify[wick[mul[p,dm[i,j]],g0n3full]],{i,1,3},{j,1,3}];
(* interacting Green's function *)
gval=gnval/pval//FullSimplify;

(* test for u=-4 *)
gvalueqminus4=gval/.{u->-4};
