# Wick
Ensure the library is in the working directory, or use absolate path
```mathematica
Get["wick.m"];
```
To have a pretty latex represtation of the operator, load (ensure that MathematicaTeXUtilities has been installed. 
download https://github.com/jkuczm/MathematicaTeXUtilities/releases/download/v1.0.1/TeXUtilities.zip and see the install instruction)
```mathematica
Get["wicktex.m"];
```
To evaluate the expectation value, first define the non-interacting single particle density matrix.
For simplicity, we consider a three sites problem:
1,3,5 are the indices for 1_up,2_up,3_up
2,4,6 are the indices for 1_dn,2_dn,3_dn  
g0loc is for spin up and spin down blocks
g0locfull is the full single-particle density matrix
```mathematica
n=.;
g0loc=Table[Subscript[n,i,j],{i,1,3},{j,1,3}];
g0locfull=ConstantArray[0,{6,6}];
g0locfull[[{1,3,5},{1,3,5}]]=g0loc;
g0locfull[[{2,4,6},{2,4,6}]]=g0loc;
```
A general operator can be constructed using op[name,...], add[...] and mul[...].
Useful options for op[...] are "HOP" and "C". "HOP" is used to build a single particle operator, and "C" is used to build a number.
```mathematica
dmup[i_,j_]:=op["HOP",i,"UP",j,"UP"];
dmdn[i_,j_]:=op["HOP",i,"DN",j,"DN"];
dens[i_]:=dmup[i,i];
holes[i_]:=add[op["C",1],mul[op["C",-1],dens[i]]];
```
For example, we could use the above function to define
```mathematica
op1=mul[dens[1],dens[2],dens[3]];
```
If wicktex.m has been loaded, one could use TexForm[op1] to get a pretty latex representation of the operator
```latex

(1)\hat{f}^\dagger_{1} \hat{f}_{1} \hat{f}^\dagger_{3} \hat{f}_{3} \hat{f}^\dagger_{5} \hat{f}_{5}

```
Then we could evaluate the expectation values of op1 under g0locfull as
```mathematica
op1expt=wick[op1,g0locfull];
```
which gives
```latex

n_{1,3} \left(n_{2,1} n_{3,2}-n_{2,2} n_{3,1}\right)+n_{1,2} \left(n_{2,3} n_{3,1}-n_{2,1} n_{3,3}\right)+n_{1,1} \left(n_{2,2} n_{3,3}-n_{2,3} n_{3,2}\right)

```
