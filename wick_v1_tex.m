(* for TeX formatting *)

Needs["TeXUtilities`"];
Format[x_uop,TeXForm]:=TeXVerbatim[uopToTex[x]];
(* the output are string for the following format function *)
uopToTex[uop[{},{}]]:="\\hat{1}";
uopToTex[uop[cr_,an_]]:=StringRiffle[Join[Map[crToTeX,cr],Map[anToTeX,an]]];
texString[x_]:=ToString[TeXForm[x]];
indexStringReplace={"\\text{up}"->"\\uparrow","\\text{dn}"->"\\downarrow","\\{"->"","\\}"->""};

indexTexString[x_]:=StringReplace[texString[x],indexStringReplace];
crToTeX[id_]:="\\hat{f}^\\dagger_{"<>indexTexString[id]<>"}";
anToTeX[id_]:="\\hat{f}_{"<>indexTexString[id]<>"}";

(* set this variable to control how many uops to print in mop *)
maxPrintTermNumber=10;

Format[x_mop,TeXForm]:=TeXVerbatim[mopToTex[x]];
mopToTex[mop[ht_]]:=Module[
	{
		keys=Sort[ht["Keys"],order],
		nkeys=ht["Length"],
		format=("("<>texString[ht["Lookup",#]]<>")"<>uopToTex[#]&),
		nprint,
		suffix
	},
	nprint=Min[maxPrintTermNumber,nkeys];
	suffix=If[nkeys>maxPrintTermNumber,"+\\cdots",""];
	If[nkeys==0,"0",
	   keys=keys[[Range[1,nprint]]];
	   StringRiffle[Map[format,keys],"+"]<>suffix]];







