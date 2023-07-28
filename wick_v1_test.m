(* update to use normal order form  and store the intermediate results *)

ClearAll[uop,mop];

(* the definition of a uniop, i.e a operator within form a^dagger_i... a_j *)
(* the first list for creation operators and second for annilation part, order, always from , the third is coefficient  *)
(* t=uop[{1,2},{1,2}]; *)

(* Tex formatting *)
Needs["TeXUtilities`"];
Format[x_uop,TeXForm]:=TeXVerbatim[uopToTex[x]];
Format[x_mop,TeXForm]:=TeXVerbatim[mopToTex[x]];
(* the output are string for the following format function *)
uopToTex[uop[cr_,an_]]:=StringRiffle[Map[crToTeX,cr]]<>StringRiffle[Map[anToTeX,an]]
crToTeX[id_]:="\\hat{f}^\\dagger_{"<>ToString[id]<>"}";
anToTeX[id_]:="\\hat{f}_{"<>ToString[id]<>"}";
mopToTex[mop[uopToval___]]:=StringRiffle[Map["("<>ToString[TeXForm[#[[2]]]]<>")"<>uopToTex[#[[1]]]&,List[uopToval]],"+"];

Get["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick_v1_tex.m"];
uop1=uop[{i},{j}];
uop2=uop[{},{}];
uop3=uop[{2,1},{1,0}];
t1=op[1]
t2=op[2]
t3=op[3]
t+(uop[1]);
t=op[2];

Do[t[uop[{i},{i}]->Subscript[n,i]],{i,1,20}]
t2=op[2]
Do[t2[uop[{i},{i}]->Subscript[n,i+1]],{i,1,20}]
t[[1]]["Insert",uop3->a]

ht=t[[1]]
cr=uop3[[1]]
an=uop3[[2]]

(* create new multi-op with a constance*)
Clear[op];

op[c]:=Module[
	{ht=CreateDataStructure["HashTable"]},
	ht["Insert",uop[{},{}]->c];
	mop[ht]
];
mop1=op[];
mop1[uop1->-3]
ht=mop1[[1]]
mop1[uop2->-1]
simplify[uop3->2];



(* mopToTex[t]; *)
(* first, function to sort uop  *)
(* Signature[{1,3,"a"}] *)
(* Sort[{1,2,"a"}] *)
(*t=uop[{1,2},{2,1}]; *)
(* {t1,s}=sort[t]; *)
(* t *)
(* ?uop *)
(* ? StringRiffle *)

sort[uop[cr_,an_]]:={uop[Sort[cr],Sort[an]],Signature[cr]*Signature[an]};

mul[uop[cr1_,an1_],uop[{cr2_},{}]]:=Module[
	{nAn=Length[an1]},
	(* first, the term shift from left of an1 to the right *)
	op1={uop[Append[cr1,cr2],{}]->(-1)^(nAn)};
	(* second, the contracted terms resulted from the first step, similar to a derivative, there is no sign for the last element*)
	op2=Table[uop[cr1,Drop[an1,i]]->KroneckerDelta[an1[[i]],cr2]*(-1)^(nAn-i),{i,1,nAn}];
	mop@@Join[op1,op2]];

mul[uop[cr1_,an1_],uop[{},an2_]]:=mop[uop[cr1,Join[an1,an2]]->1];

(* ClearAll[mul]; *)
(* Assuming[k=m,mul[t4,t2]] *)
(* we have uop, and mop as some place for short term, and we use hashtable to store large operators *)





(* now,  we need some basic operation over uniop, to simplify the task, we implement multiop, using the hash map, but before that, we just use list*)
(* t=mop[uop[{1,2},{2,1}]->1,uop[{1,2},{1,3}]->2]; *)
ClearAll[mul];
(* test case *)
uop1=uop[{1,2},{1}];
uop2=uop[{1},{}];
mul[uop1,uop2]
(* we treat creation, with only one operator *)
(* we could first implement the brute force version, then optimize it *)
(* we first assume very generic operation, i.e symbolic index, so we can really do symbolic calculation *)
t1=uop[{i,j},{k,l}]
t4=uop[{i,j},{k}]
t5=uop[{},{k}]
t2=uop[{m},{}]
t3=mop[uop[{i,j},{l,m}]->c]
(* Append[{1,2},3] *)
cr1={1,2,3};
an1={2};
cr2=2;
t7=mul[t1,t5];
sort[t7[[1,1]]]



(* check twouopToc *)
(* key function  *)
ClearAll[i,jk,l,m,n,c];
addTo[t1,twouopToc[{i,j},{k},{l,m},{n},c]];
t
twouopToc[{},{},{},{},c]
t1
t2


op[1]
t=op[2]
uop1=t[[1]]["Keys"][[1]]
uop1[[1]]
t1=op[4]
t2=op[2]
mul[t1,t2]
t2=t1.t1;
t1=op[];
nij=op[{i},{j}];
nkl=op[{k},{l}];
nij.nkl+op[-1].nkl.nij;
Order[1,1]
Sort[m[[1]]["Keys"],order];

i1=Subscript[i,1];
i2=Subscript[i,2];
j1=Subscript[j,1];
j2=Subscript[j,2];
k1=Subscript[k,1];
k2=Subscript[k,2];
l1=Subscript[l,1];
l2=Subscript[l,2];
dij=op[{i1,i2},{j1,j2}];
dkl=op[{k1,k2},{l1,l2}];
dij.dkl+op[-1].dkl.dij;
number[uop[{{1},{2}},{2}]];

eval[uop[{1},{1}],tp]

eval[uop[{},{}],tp]
eval[uop[{1},{}],tp]
eval[uop[{1},{1}],tp]
eval[uop[{1,1},{}],tp]
eval[uop[{2,2},{1,1}],tp]
evalNgt2[uop[{1,2,3},{}],tp]
x=uop[{1,2,3},{}];

diagg0[uop[{idx1_},{idx2_}]]:=Subscript["n",idx1,idx2];
diagg0[uop[{idx1_,idx2_},{}]]:=0;
diagg0[uop[{},{idx1_,idx2_}]]:=0;

tp=tape[defaultg0];
eval[uop[{1,2,3},{4}],tp]
eval[uop[{},{1,2,3,4}],tp]
tp=tape[diagg0];
eval[uop[{1,2},{3,4}],tp]
eval[uop[{1,2,3,4},{}],tp]
eval[uop[{1,2},{4,5}],tp]
evalwick[uop[{1,2},{3,4}],tp];
evalwick[uop[{1,2,3,4},{}],tp];
getTmp[tp]["Lookup",uop[{1,2},{3,4}]]
a=1
  a+=1;

Delete[{1,2,3},2]

tp=tape[defaultg0];
evalwick[uop[{1,2},{3,4}],tp]
evalwick[uop[{1,5},{1,4}],tp]
     
cr1f=1;
cr1=Sequence[2]
an1={3,4};

Get["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick_v1.m"];
Get["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick_v1_tex.m"];

tp=tape[defaultg0];
eval[uop[{1,2,3},{4}],tp]
eval[uop[{},{1,2,3,4}],tp]
eval[uop[{1,2},{1,2,3,4}],tp]
number[uop[{},{1,2,3,4}]]
eval[uop[{1,2,3,4},{}],tp]
an1f=1;
an1=Sequence[2,3,4]
i=1;

tp=tape[defaultg0];
op1=op[{5,6},{3,4},2];
op2=op[{3,4},{5,6},2];
op1=op1+op2;
op3=op1^8
eval[op1,"op1",tp]

name="op1";
outputStr[output_,name_]:=name<>"="<>ToString[CForm[output["Lookup",name]]];

outputStr[output,name]
x=output["Lookup","op1"]
toString[x[[1]]]
     
tp

(* example *)

Get["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick_v1.m"];
Get["/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/wick_v1_tex.m"];

tp=tape[defaultg0];
op1=op[{5,6},{3,4},2];
op2=op[{3,4},{5,6},2];
eval[op1+op2,"op12",tp]

outputDir="/home/chengzhengqian/share_workspace/czq_julia_package/WickMathematica/gene_code";

name="op12";
args={"n","p","q"};
results={"op12"};
export[tp,name,args,results,outputDir];
