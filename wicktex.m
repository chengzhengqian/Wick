(* we add some way to pretify the expression *)
(* this is not idea, as we force the output to string *)
(* Format[sum[arg___],TeXForm]:=StringRiffle[Map[prodToText,List[arg]],"+"]; *)
(* prodToText[prod_]:="("<>ToString[prod[[1]]]<>")"<>ToString[prod[[2]]] *)

(* we use texUtiles to make a better representation *)
(* download https://github.com/jkuczm/MathematicaTeXUtilities/releases/download/v1.0.1/TeXUtilities.zip *)
(* we can install the package by putting TexUtiles to the following directory *)
(* FileNameJoin[{$UserBaseDirectory,"Applications"}] *)
(* "/home/chengzhengqian/.WolframEngine/Applications" *)
Needs["TeXUtilities`"]

Format[sum[arg___],TeXForm]:=TeXVerbatim[StringRiffle[Map[prodToTeX,List[arg]],"+"]]
prodToTeX[prod_]:="("<>ToString[prod[[1]],TeXForm]<>")"<>termToTeX[prod[[2]]];
termToTeX[term_]:=StringRiffle[Map[opToTeX,term]," "]
opToTeX[fd[id_]]:="\\hat{f}^\\dagger_{"<>ToString[id]<>"}";
opToTeX[f[id_]]:="\\hat{f}_{"<>ToString[id]<>"}";


