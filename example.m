(* load wick.m, to apply the Wick theorem
make sure the working directory contains wick.m
 *)
Get["wick.m"];

(* optional !!
To use the Tex format, install MathematicaTeXUtilities
download https://github.com/jkuczm/MathematicaTeXUtilities/releases/download/v1.0.1/TeXUtilities.zip 
 *)
Get["wicktex.m"];

(* we first define the non-interacting single particle density matrix
  For simplicity, we consider a three sites problem
  1,3,5 are the indices for 1_up,2_up,3_up
  2,4,6 are the indices for 1_dn,2_dn,3_dn  
  g0loc is for spin up and spin down blocks
and g0locfull is the full single-particle density matrix
 *)
n=.;
g0loc=Table[Subscript[n,i,j],{i,1,3},{j,1,3}];
g0locfull=ConstantArray[0,{6,6}];
g0locfull[[{1,3,5},{1,3,5}]]=g0loc;
g0locfull[[{2,4,6},{2,4,6}]]=g0loc;


(* then we can use op[name,para...] to construct any operator in second quatization represntation,  examples *)
dm[i_,j_]:=op["HOP",i,"UP",j,"UP"];
dens[i_]:=dm[i,i];
holes[i_]:=add[op["C",1],mul[op["C",-1],dens[i]]];

(use add and mul to combine operators)
op1=mul[dens[1],dens[2],dens[3]];
(* If wicktex.m has been load, this generate a operator as 
  (1)\hat{f}^\dagger_{1} \hat{f}_{1} \hat{f}^\dagger_{3} \hat{f}_{3} \hat{f}^\dagger_{5} \hat{f}_{5}
using TeXForm[op1] 
 *)

(* then the expectation value can be evaluated as *)

op1expt=wick[op1,g0locfull];

