oldresults = {sum[prod[1 + u/4, {}], prod[-u/2, {fd[1], f[1]}], 
      prod[-u/2, {fd[2], f[2]}], prod[u, {fd[1], f[1], fd[2], f[2]}]], 
     sum[prod[1 + u/4, {}], prod[-u/2, {fd[3], f[3]}], 
      prod[-u/2, {fd[4], f[4]}], prod[u, {fd[3], f[3], fd[4], f[4]}]], 
     sum[prod[(1 + u/4)^2, {}], prod[-((1 + u/4)*u)/2, {fd[3], f[3]}], 
      prod[-((1 + u/4)*u)/2, {fd[4], f[4]}], prod[(1 + u/4)*u, 
       {fd[3], f[3], fd[4], f[4]}], prod[-((1 + u/4)*u)/2, {fd[1], f[1]}], 
      prod[u^2/4, {fd[1], f[1], fd[3], f[3]}], 
      prod[u^2/4, {fd[1], f[1], fd[4], f[4]}], prod[-u^2/2, 
       {fd[1], f[1], fd[3], f[3], fd[4], f[4]}], prod[-((1 + u/4)*u)/2, 
       {fd[2], f[2]}], prod[u^2/4, {fd[2], f[2], fd[3], f[3]}], 
      prod[u^2/4, {fd[2], f[2], fd[4], f[4]}], prod[-u^2/2, 
       {fd[2], f[2], fd[3], f[3], fd[4], f[4]}], prod[(1 + u/4)*u, 
       {fd[1], f[1], fd[2], f[2]}], prod[-u^2/2, {fd[1], f[1], fd[2], f[2], 
        fd[3], f[3]}], prod[-u^2/2, {fd[1], f[1], fd[2], f[2], fd[4], f[4]}], 
      prod[u^2, {fd[1], f[1], fd[2], f[2], fd[3], f[3], fd[4], f[4]}]], 
     sum[prod[1, {fd[5], f[5], fd[6], f[6]}]], 
     ((4 + u)^2 + 4*u*(4*u*Subscript[c, 1, 2]^2*Subscript[c, 2, 1]^2 + 
         (4 + u)*(-1 + Subscript[c, 2, 2])*Subscript[c, 2, 2] + 
         2*u*Subscript[c, 1, 2]*Subscript[c, 2, 1]*
          (-1 + 2*Subscript[c, 2, 2]) + Subscript[c, 1, 1]^2*
          (4 + u + 4*u*(-1 + Subscript[c, 2, 2])*Subscript[c, 2, 2]) - 
         Subscript[c, 1, 1]*(4 + u + 4*u*(-1 + Subscript[c, 2, 2])*
            Subscript[c, 2, 2] + 4*u*Subscript[c, 1, 2]*Subscript[c, 2, 1]*
            (-1 + 2*Subscript[c, 2, 2]))))/16}
 
sum /: Format[sum[arg___], TeXForm] := TeXVerbatim[
      StringRiffle[prodToTeX /@ {arg}, "+"]]
 
prodToTeX[prod_] := StringJoin["(", ToString[prod[[1]], TeXForm], ")", 
     termToTeX[prod[[2]]]]
 
prod /: Format[p_prod, TeXForm] := TeXVerbatim[prodToTeX[p]]
 
p = sum[prod[(1 + u/4)^2, {}], prod[-((1 + u/4)*u)/2, {fd[3], f[3]}], 
     prod[-((1 + u/4)*u)/2, {fd[4], f[4]}], prod[(1 + u/4)*u, 
      {fd[3], f[3], fd[4], f[4]}], prod[-((1 + u/4)*u)/2, {fd[1], f[1]}], 
     prod[u^2/4, {fd[1], f[1], fd[3], f[3]}], 
     prod[u^2/4, {fd[1], f[1], fd[4], f[4]}], 
     prod[-u^2/2, {fd[1], f[1], fd[3], f[3], fd[4], f[4]}], 
     prod[-((1 + u/4)*u)/2, {fd[2], f[2]}], 
     prod[u^2/4, {fd[2], f[2], fd[3], f[3]}], 
     prod[u^2/4, {fd[2], f[2], fd[4], f[4]}], 
     prod[-u^2/2, {fd[2], f[2], fd[3], f[3], fd[4], f[4]}], 
     prod[(1 + u/4)*u, {fd[1], f[1], fd[2], f[2]}], 
     prod[-u^2/2, {fd[1], f[1], fd[2], f[2], fd[3], f[3]}], 
     prod[-u^2/2, {fd[1], f[1], fd[2], f[2], fd[4], f[4]}], 
     prod[u^2, {fd[1], f[1], fd[2], f[2], fd[3], f[3], fd[4], f[4]}]]
 
termToTeX[term_] := StringRiffle[opToTeX /@ term, " "]
 
opToTeX[fd[id_]] := StringJoin["\\hat{f}^\\dagger_{", ToString[id], "}"]
 
opToTeX[f[id_]] := StringJoin["\\hat{f}_{", ToString[id], "}"]
 
Attributes[Subscript] = {NHoldRest}
