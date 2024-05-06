(* uop, single product, mop, sum of uops with coefficient, using hashtable
op, function to creat mop
 *)
ClearAll[uop,mop,op,cr,an,up,dn,mul,cm,am,order,number];
(* help to simplify the KroneckerDelta *)
Unprotect[Power];
Power[KroneckerDelta[x_,y_],p_Integer?Positive]:=KroneckerDelta[x,y]
Protect[Power];
(* only for this package *)
(* up, dn is reserved for spin up and spin dn *)
Unprotect[KroneckerDelta];
KroneckerDelta[{___,up,___},{___,dn,___}]:=0;
KroneckerDelta[{___,dn,___},{___,up,___}]:=0;
KroneckerDelta[up,dn]:=0;
KroneckerDelta[dn,up]:=0;
Protect[KroneckerDelta];
(* this does not work, one should add this to sort *)
(* Unprotect[Order]; *)
(* Order[up,dn]=1; *)
(* Order[dn,up]=-1; *)
(* Protect[Order]; *)


uop[]:=uop[{},{}];
op[]:=mop[CreateDataStructure["HashTable"]];
op[c_]:=addTo[op[],uop[]->c];
op[cr_List,an_List,c_]:=addTo[op[],uopToc[cr,an,c]];
op[cr_List,an_List]:=op[cr,an,1];
(* the mapping form assuming uop is sorted *)
signreverse[n_]:=(-1)^(n*(n-1)/2);
conj[x:uop[cr_,an_]->c_]:=uop[an,cr]->Conjugate[c]*signreverse[numcr[x]]*signreverse[numan[x]];

conj[mop[ht_]]:=Module[{result=op[],key},
		       Do[addTo[result,conj[key->ht["Lookup",key]]],{key,ht["Keys"]}];
		result];
(* get the constant part *)
const[mop[ht_]]:=ht["Lookup",uop[{},{}],0&];
(*matrix representation of a operator,   *)
repr[x_mop,basis_]:=Module[
	{conjbasis=Map[conj,basis],i,j},
	Table[const[i.x.j],{i,conjbasis},{j,basis}]];

cr[x_uop]:=x[[1]];
an[x_uop]:=x[[2]];

(* this will update the first mop , assuming uop is sorted*)
(* uop[cr_,an_]->c assume up are sorted, see uopToc to generate this form *)
addTo[mop[ht_],uop[cr_,an_]->c_]:=Module[
	{term=uop[cr,an],val},
	val=Simplify[ht["Lookup",term,0&]+c];
	If[SameQ[val,0],
	   If[ht["KeyExistsQ",term],ht["KeyDrop",term]],
	   ht["Insert",term->val]
	];
	mop[ht]];

addTo[x_mop,mop[ht2_]]:=((addTo[x,#->ht2["Lookup",#]]&) /@ ht2["Keys"];x);
addTo[x_mop]:=x;
addTo[x_mop,val__]:= (addTo[x,#] & /@List[val];x);

(* now, we need to implement multiplication *)
(* test the generating ideas *)
(* haha[a_,b__]=Sequence[haha[a],haha[b]]; *)
(* {haha[1,2,3,4,5]} *)
(* product of two normal ordered form  *)
shift[cr1_,an1_,cr2_,an2_,0]:=Sequence[];
shift[cr1_,an1_,{cr2f_,cr2___},an2_,c_]:=Sequence[
	shift[Append[cr1,cr2f],an1,{cr2},an2,c*(-1)^Length[an1]],
	Sequence @@ contract[cr1,an1,cr2f,{cr2},an2,c] ];
shift[cr1_,an1_,{},an2_,c_]:=uopToc[cr1,Join[an1,an2],c];

(* contracted terms, generated when move cr2f from right of an1 to its left *)
contract[cr1_,an1_,cr2f_,cr2_,an2_,c_]:=Module[
	{i,nan1=Length[an1]},
	Table[shift[cr1,Delete[an1,i],cr2,an2,
			c*KroneckerDelta[an1[[i]],cr2f]*(-1)^(nan1-i)],
	      {i,1,nan1}]];


(* uopToc assuming sorted *)
(* make the uniop sorted *)
(* convension, as long as in term(i.e uop[cr,an])->c pattern, term is sorted *)
uopToc[cr_,an_,0]:=Sequence[];
uopToc[cr_,an_,c_]:=uopTocSorted[Sort[cr],Sort[an],c*Signature[cr]*Signature[an]];
uopTocSorted[cr_,an_,0]:=Sequence[];
uopTocSorted[cr_,an_,c_]:=uop[cr,an]->c;
(* in a seqence expansion manner, notice this interesting trick! *)
mul[uop1_uop,uop2_uop,c_]:=shift[cr[uop1],an[uop1],cr[uop2],an[uop2],c];
(* now, we can implement product, first ,two two mops *)
mul[mop[ht1_],mop[ht2_]]:=Module[
	{result=op[],
	 key1=ht1["Keys"],key2=ht2["Keys"],
	 nk1=ht1["Length"],nk2=ht2["Length"],
	 i1,i2},
	Do[
		uop1=key1[[i1]];val1=ht1["Lookup",uop1];
		uop2=key2[[i2]];val2=ht2["Lookup",uop2];
		addTo[result,mul[uop1,uop2,val1*val2]];
	  ,{i1,1,nk1},{i2,1,nk2}];
	result ];

mul[x_mop,val__]:=Fold[mul,x,List[val]];

power[x_mop,val_]:=mul @@ ConstantArray[x,val];

(* number of operators in uop *)
(* ClearAll[number]; *)
(* function to check the properties of uop *)
numcr[uop[cr_,an_]]:=Length[cr];
numan[uop[cr_,an_]]:=Length[an];
number[uop[cr_,an_]]:=Length[cr]+Length[an];

fermionicQ[x_uop]:=OddQ[number[x]];
bosonicQ[x_uop]:=EvenQ[number[x]];
constantQ[x_uop]:=(number[x]==0);
(* just to display *)
order[x_uop,y_uop]:=Order[number[x],number[y]];

(* override operator + , . , mop and val are immultable*)
(* we focus to implement addTo and mul *)
(mop /: Plus[mop[ht_],val__]:=addTo[mop[ht["Copy"]],val]);
(mop /: Dot[x_mop,val__]:=mul[x,val]);
(mop /: Power[x_mop,val_]:=power[x,val]);

(* ClearAll[mop] *)

(* commutator *)
cm[a_,b_]:=a.b+op[-1].b.a;
(* anti-commutator *)
am[a_,b_]:=a.b+b.a;

(* now, we can start to implement Wick theorem *)
(* first, we evaluate the uop *)
(* we want to construct a general case, including the BCS case *)
(* ClearAll[defaultg0]; *)
(* default way to denote the single particle density matrix, n,p,q *)
(* it is better to use string, as user may accidently define n,p,q *)
(* after some consideration, I decide to use n,w,m, (as p may be used as 1-n) *)
(* Also, m can be viewed as a vertical flip from w, so like the conjugation *)
(* so, n, w,m *)
defaultg0[uop[{idx1_},{idx2_}]]:=Subscript["n",idx1,idx2];
defaultg0[uop[{idx1_,idx2_},{}]]:=Subscript["w",idx1,idx2];
defaultg0[uop[{},{idx1_,idx2_}]]:=Subscript["m",idx1,idx2];

(* fn, function to evalute single particle operator, see defaultg0, the second and third are two hashtable for intermediate values (uop->{val,idx}) *)
(* set default  name for intermediate calculation *)
(* for how we store the node in val *)
defaultTemp[idx_]:=Subscript[tmp,idx];
(* tape[fn,htUop,htOutput] *)
tape[fn_]:=Module[
	{tmp=CreateDataStructure["HashTable"],
	 output=CreateDataStructure["HashTable"]},
	tmp["Insert","num"->0];
	tape[fn,tmp,output]];

(* output entries *)
getGfn[tape[gfn_,tmp_,output_]]:=gfn;
getTmp[tape[gfn_,tmp_,output_]]:=tmp;
getOutput[tape[gfn_,tmp_,output_]]:=output;

(* tp=tape[defaultg0]; *)
 
eval[x_uop,tp_tape]:=Module[
	{n=number[x]},
	Which[
		n==0,1,
		n==2,getGfn[tp][x],
		EvenQ[n],getTmp[tp]["Lookup",x,evalwick[#,tp]&],
		OddQ[n],0
	]];

update[x_,tmp_,result_]:=Module[
	{num=tmp["Lookup","num"]},
	If[SameQ[result,0],
	   (tmp["Insert",x->0];0),
	   (tmp["Insert",Subscript["tmp",num+1]->result];
	    tmp["Insert",x->Subscript["tmp",num+1]];
	    tmp["Insert","num"->num+1];
	    Subscript["tmp",num+1])]];

evalwick[x:uop[{cr1f_,cr1___},an1_],tp_tape]:=Module[
	{
		tmp=getTmp[tp],
		(*we convert sequence to list*)
		cr1List=List[cr1],
		nan1=Length[an1],
		result=0,i,term,val,ncr1
	},
	ncr1=Length[cr1List];
	Do[
		term=cr1List[[i]];
		val=eval[uop[{cr1f,term},{}],tp];
		If[Not[SameQ[val,0]],
		   result+=(-1)^(i-1)*val*eval[uop[Delete[cr1List,i],an1],tp]]
	       ,{i,1,ncr1}];
	Do[
		term=an1[[i]];
		val=eval[uop[{cr1f},{term}],tp];
		If[Not[SameQ[val,0]],
		   result+=(-1)^(i-1+ncr1)*val*eval[uop[cr1List,Delete[an1,i]],tp]]
	       ,{i,1,nan1}];
	update[x,tmp,Simplify[result]]
	];

evalwick[x:uop[{},{an1f_,an1___}],tp_tape]:=Module[
	{
		tmp=getTmp[tp],
		an1List=List[an1],
		result=0,i,term,val,nan1
	},
	nan1=Length[an1List];
	Do[
		term=an1List[[i]];
		val=eval[uop[{},{an1f,term}],tp];
		If[Not[SameQ[val,0]],
		   result+=(-1)^(i-1)*val*eval[uop[{},Delete[an1List,i]],tp]]
	       ,{i,1,nan1}];
	update[x,tmp,Simplify[result]]
	];

(* now, we are ready to evaluate mop *)
(* we use a name to retrieve the output *)
eval[mop[ht_],name_String,tp_tape]:=Module[
	{result=0,
	 terms=ht["Keys"],
	 output=getOutput[tp],
	 term,i
	},
	Do[
		term=terms[[i]];
		result+=ht["Lookup",term]*eval[term,tp]
	       ,{i,1,Length[terms]}];
	output["Insert",name->result];
	result];

(* we add a function to get the full form of a given mop by its name *)
eval[name_String,tp_tape]:=tp[[3]]["Lookup",name]//.Normal[Normal[tp[[2]]]];

