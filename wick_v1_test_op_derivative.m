Get["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick_v1.m"];
Get["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick_v1_tex.m"];

o=op[{l},{m},c];
fid=op[{i},{}];
fi=op[{},{i}];
cm[fid,o];
cm[fi,o];
(* a more complex example *)

o=op[Table[Subscript[l,i],{i,1,3}],Table[Subscript[m,i],{i,1,3}],c];
cm[fid,o];
cm[fi,o];

o=op[Table[Subscript[l,i],{i,1,2}],{m},c];
o=op[Table[Subscript[l,i],{i,1,2}],Table[Subscript[m,i],{i,1,3}],c];
am[fid,o];
am[fi,o];

t3=op[3]
t=op[{i},{j}]+op[{k},{l}]
m=t.t.t;

m=t.t;
n=m.t;


  
  
    
  
  


key1=m[[1]]["Values"][[1]]
key1//Simplify;
ClearAll[i,j,k,l,c];
FullForm[KroneckerDelta[i,j]^3]


power[KroneckerDelta[i,j],2]




nup[i_,j_]:=op[{{i,up}},{{j,up}}];
ndn[i_,j_]:=op[{{i,dn}},{{j,dn}}];

ClearAll[x,y,z]
KroneckerDelta[{x,y},{y,z}]

nup=(nup[1,1]+op[-1/2])
ndn=(ndn[1,1]+op[-1/2])
nup.ndn
number[uop[{1},{2,3}]]
?number
number[uop[{1,{1,2},{12,2}},{2}]];
?OddQ
fermionicQ[uop[]]
bosonicQ[uop[]]
constantQ[uop[{1},{1}]]


Get["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick_v1.m"];
Get["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick_v1_tex.m"];



