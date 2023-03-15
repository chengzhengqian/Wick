(* implement the Wick theorem *)
(* f default symobl for fermion operator ,with spin*)
(* sum[prod...] *)
(* prod[coeff,{term...}] *)
(* term=f[index]|fd[index] *)
(* fd[index] annilation fd[index] creation *)

(* a vector in hte operator space, expressed as sum of prods *)

prod=.; sum=.;f=.;fd=.;

isSum[s_]:=(Head[s]===sum);
getProds[s_]:=Level[s,1];

(* a prod contains the coefficient and the operator list *)
isProd[p_]:=(Head[p]===prod);
getCoeff[p_]:=p[[1]];
getOps[p_]:=p[[2]];

(* for fermion operator *)
isCreation[op_]:=(Head[op]===fd)
isAnnihalation[op_]:=(Head[op]===f)
isFermion[op_]:=(isCreation[op]) || (isAnnihalation[op])
getIdx[op_]:=op[[1]]
(* time order, -1,0,1 *)
fermionOrder[op1_,op2_]:=Module[{idx1=getIdx[op1],idx2=getIdx[op2]}, Which[idx1<idx2,1,idx1==idx2,0,idx1>idx2,-1]];
isOpSame[op1_,op2_]:=(getIdx[op1]==getIdx[op2])&&(Head[op1]===Head[op2])
isOpConjugate[op1_,op2_]:=(getIdx[op1]==getIdx[op2])&&(!(Head[op1]===Head[op2]))

(* Now, we need to sort the ops with fermion statisits *)

swapList[oplist_,idx_]:=Module[{x1=oplist[[idx]],x2=oplist[[idx+1]],resultList=oplist},(resultList[[idx]]=x2;resultList[[idx+1]]=x1;{1,resultList})]

swapListWithOrder[oplist_,idx_]:=Module[{x1=oplist[[idx]],x2=oplist[[idx+1]]},If[fermionOrder[x1,x2]<0, swapList[oplist,idx],{0,oplist}]]

(* this performs a one loop swap from left tor right. In the end, the last element is garanteed to be largest *)

sortOnce[oplist_,until_]:=Module[{swaps=0,list=oplist,result,i}, (For[i=1,i<until,i++,(result=swapListWithOrder[list,i];list=result[[2]];swaps+=result[[1]])];{swaps,list})];

(* notice the local variable *)
sortAll[oplist_]:=Module[{length=Length[oplist],list=oplist,result,swaps=0,i}, (For[i=length,i>1,i--,(result=sortOnce[list,i];swaps+=result[[1]];list=result[[2]])];{swaps,list})]



(* now, we need to implement the simplication for a given idx ,check wither it can be simplified*)
(* this are following three categories a_d*a*a_d->a_d, a*a->0 *)
(* {nextIndexToCheck,list} *)
(* we need to keep the coefficient information *)
(* so we list the op list to prod  *)

simplifyProdAt[p_,idx_]:=Module[{list=getOps[p],c=getCoeff[p]},Which[ (idx>=3)&&isOpSame[list[[idx-2]],list[[idx]]]&&isOpConjugate[list[[idx-1]],list[[idx]]],{idx-1,prod[c,Drop[list,{idx-1,idx}]]},(idx>=2)&&isOpSame[list[[idx-1]],list[[idx]]],{1,prod[0,{}]},True,{idx+1,prod[c,list]}]]
(* now, we simplified the whole expression *)
simplifyProd[p_]:=Module[{n=1,pResult=p,result={1,p}},While[n<=Length[getOps[pResult]],(result=simplifyProdAt[pResult,n]);n=result[[1]];pResult=result[[2]]];result[[2]]]
(* there are name problems, c1, c2 list1.. shold not be used in the expression *)
mulProd[p1_,p2_]:=Module[{c1=getCoeff[p1],c2=getCoeff[p2],list1=getOps[p1],list2=getOps[p2],temp,c,list},temp=sortAll[Join[list1,list2]];list=temp[[2]];c=c1*c2*(-1)^(temp[[1]]);simplifyProd[prod[c,list]]]

(* now, we define the addition and multiplication of two generic operator *)
isTermInSum[s_,p_]:=Module[{sizeOfTerms=Length[s],i,isInSum=False,index=-1},For[i=1,(i<=sizeOfTerms && (isInSum==False)),i++,If[getOps[s[[i]]]===getOps[p],(isInSum=True;index=i)]];{isInSum,index}]

(*now we need to simplify sum*)
(* this is just append *)
addProdToSum[s_,p_]:=sum@@Join[getProds[s],{p}]
(* s is reduced, there is no duplicated terms *)
addProdToReducedSum[s_,p_]:=Module[{list=s,result=isTermInSum[s,p],location,isInSum},isInSum=result[[1]];location=result[[2]];If[isInSum,list[[location]]=prod[getCoeff[list[[location]]]+getCoeff[p],getOps[list[[location]]]],list=addProdToSum[list,p]];list]
(* this combines the smae terms *)
combineProdsInSum[s_]:=Module[{result=sum[],i,sizeOfTerms=Length[s]},For[i=1,i<=sizeOfTerms,i++,result=addProdToReducedSum[result,s[[i]]]];result]

dropZeroProdsInSum[s_]:=Select[s,!(getCoeff[#]===0)&]

simplifySum[s_]:=dropZeroProdsInSum[combineProdsInSum[s]]


(* now, we need to implement addition and multiplication *)
addSum[s1_,s2_]:=simplifySum[sum@@(Join@@{getProds[s1],getProds[s2]})]
mulSum[s1_,s2_]:=simplifySum[sum@@(Join@@Table[mulProd[s1[[i]],s2[[j]]],{i,1,Length[s1]},{j,1,Length[s2]}])]

(* now, we need to implement the add and sume that can takea arbitrary number of arguments *)
add[s___]:=Module[{list=List[s],l,result=sum[],i},l=Length[list];For[i=1,i<=l,i++,result=addSum[result,list[[i]]]];result]
mul[s___]:=Module[{list=List[s],l,result=sum[prod[1,{}]],i},l=Length[list];For[i=1,i<=l,i++,result=mulSum[result,list[[i]]]];result]

(* now, we need to implement the Wick theorem *)
(* compute the average of a single product, using Wick theorem *)
(* m is the matrix of <fd[idx1]f[idx2]> *)
(* we  treat the 0, 1, 2 case seperately*)
(* m is the normal matrix , fd[i]*f[j] normal order, which wick theorem use the time order, also, we need to handle one site case*)
(* assuming terms is ordered *)
wickProdZero[p_,m_]:=getCoeff[p];
wickProdOne[p_,m_]:=0;
wickProdTwo[p_,m_]:=Module[{c=getCoeff[p],terms=getOps[p],a,b,idx1,idx2},a=terms[[1]];b=terms[[2]];idx1=getIdx[a];idx2=getIdx[b];c*Which[isCreation[a]&&isAnnihalation[b],m[[idx1,idx2]],isCreation[b]&&isAnnihalation[a]&&(idx1==idx2),1-m[[idx1,idx1]],isCreation[b]&&isAnnihalation[a],-m[[idx2,idx1]],True,0]];
wickProdN[p_,m_]:=Module[{c=getCoeff[p],numTerms=Length[getOps[p]],terms=getOps[p],first,tail,i,result=0,temp,currentPair,remains},first=terms[[1]];tail=Drop[terms,{1}];For[i=1,i<=numTerms-1,i++,currentPair={first,tail[[i]]};remains=Drop[tail,{i}];temp=wickProdTwo[prod[c,currentPair],m];result+=wickProd[prod[temp*(-1)^(i-1),remains],m]];FullSimplify[result]]
wickProd[p_,m_]:=Module[{c=getCoeff[p],numTerms=Length[getOps[p]]},If[c===0,0,Which[numTerms==0,wickProdZero[p,m],numTerms==1,wickProdOne[p,m],numTerms==2,wickProdTwo[p,m],numTerms>2,wickProdN[p,m]]]];
wick[s_,m_]:=Module[{prods=getProds[s],l=Length[getProds[s]],result=0,i},For[i=1,i<=l,i++,result+=wickProd[prods[[i]],m]];FullSimplify[result]]

(*  now, we need some facillaties to create basic operators*)
(* this convension for spin is (1,2),(3,4) ->((1,up),(1,dn)) ((2,up),(2,dn))*)
(* one orbital first *)
op[name_,para___]:=Which[name=="CR",opCreation[para],name=="AN",opAnnihilation[para],name=="N",opDensity[para],name=="H",opHubbard[para],name=="NT",opDensityTotal[para],name=="C",opConstant[para],name=="HOP",opHopping[para]]
spinToNumber[spin_]:=Which[spin=="UP",0,spin=="DN",1];
orbitalSpinToNumber[orbital_,spin_]:=(2*orbital-1)+spinToNumber[spin]
opCreation[orbital_,spin_]:=sum[prod[1,{fd[orbitalSpinToNumber[orbital,spin]]}]]
opAnnihilation[orbital_,spin_]:=sum[prod[1,{f[orbitalSpinToNumber[orbital,spin]]}]]
opDensity[orbital_,spin_]:=Module[{idx=orbitalSpinToNumber[orbital,spin]},sum[prod[1,{fd[idx],f[idx]}]]]
opHubbard[orbital_]:=mul[opDensity[orbital,"UP"],opDensity[orbital,"DN"]]
opDensityTotal[orbital_]:=add[opDensity[orbital,"UP"],opDensity[orbital,"DN"]]
opConstant[c_]:=sum[prod[c,{}]];
opHopping[i_,spini_,j_,spinj_]:=mul[opCreation[i,spini],opAnnihilation[j,spinj]]

(* we need to generate the full Green function for spin up *)
geneGFromSpinUp[g_]:=Module[{dim=Length[g],result,i,j},result=ConstantArray[0,{2*dim,2*dim}];For[i=1,i<=dim,i++,For[j=1,j<=dim,j++,result[[orbitalSpinToNumber[i,"UP"],orbitalSpinToNumber[j,"UP"]]]=g[[i,j]];result[[orbitalSpinToNumber[i,"DN"],orbitalSpinToNumber[j,"DN"]]]=g[[i,j]];]];result]



(* it is weired that previous defintion does not work , the problem is that we should use use variable in the Block (we should use Module). Block still treas the variable as globle*)
(* mulSum[s1_,s2_]:=Moduel[{result=sum[],l1=Length[s1],l2=Length[s2],i,j},For[i=1,i<=l1,i++,For[j=1,j<=l2,j++,(result=addProdToReducedSum[result, mulProd[s1[[i]],s2[[j]]]])]];result] *)

(* now, we need to implement the wick theorem *)



(* we move the tex part to a different file *)

