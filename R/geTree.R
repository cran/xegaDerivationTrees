
#' Selects k-th production rule index from a vector of production rules.
#'
#' @description \code{chooseRulek()} selects the k-th production rule index 
#'      from the vector of production rule indices 
#'      in the \code{g$PT$LHS} for a non-terminal symbol.
#'
#' @param riv  Vector of production rule indices for 
#'                    a non-terminal symbol.
#' @param k    Integer. 
#'
#' @return The index of the production rule. 
#'
#' @family Choice
#'
#' @examples
#' chooseRulek(c(7, 8, 9), 9)
#' chooseRulek(as.vector(1), 9)
#' @export
chooseRulek<- function(riv, k) {return(riv[1+(k%%length(riv))])}

#' Transforms a non-terminal symbol into a 1-level derivation tree 
#' for a given k.
#'
#' @description \code{rndsubk()} expands a non-terminal by a derivation
#'              specified by k and returns a 1-level derivation tree.
#'
#' @param sym      Non-terminal symbol.
#' @param k        Codon (An integer). 
#' @param PT       Production table.
#'
#' @return 1-level derivation tree.
#'
#' @family Generate Derivation Tree
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' rndsubk(g$Start, 207, g$PT)
#'
#' @importFrom xegaBNF rules
#' @export
rndsubk<-function(sym, k, PT)
{substituteSymbol(chooseRulek(xegaBNF::rules(sym, PT$LHS), k),PT)}

#' Generates a derivation tree from an integer vector.
#'
#' @description \code{generateDerivationTree()} 
#'    generates a derivation tree from an integer vector.
#'    The derivation tree may be incomplete.
#'    (For grammatical evolution).
#'
#' @param sym          Non-terminal symbol. 
#' @param kvec         Integer vector.
#' @param complete     Boolean. FALSE for incomplete derivation trees.
#' @param G            Grammar. 
#' @param maxdepth     Integer. Maximal depth of the derivation tree.    
#'
#' @details \code{generateDerivationTree()} recursively expands 
#'         non-terminals and builds a derivation tree.
#'
#' @return A named list l$tree, l$kvec, l$complete. 
#'
#' @family Generate Derivation Tree
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' a<-sample(100, 100, replace=TRUE)
#' b<-generateDerivationTree(sym=g$Start, kvec=a, G=g, maxdepth=10)
#' decodeDT(b$tree, g$ST)
#'
#' @importFrom xegaBNF isTerminal
#' @export
generateDerivationTree<-function(sym, kvec, complete=TRUE, G, maxdepth=5)
{
   if (xegaBNF::isTerminal(sym, G$ST))
   { return(list(tree=sym, kvec=kvec, complete=complete)) }
   #
   if (length(kvec)==0)
   { return(list(tree=sym, kvec=kvec, complete=complete)) }
   #
   tmp<-rndsubk(sym, kvec[1], G$PT)
   if (length(kvec)==1) 
   {  # cat("integers used up.\n")
      return(list(tree=tmp, kvec=vector(), complete=FALSE)) }
   nvec<-kvec[2:length(kvec)]
   symbols<-tmp[[2]]
   l<-list()
   for (i in 1:length(symbols))
   { m<-generateDerivationTree(symbols[i], nvec, complete, G, maxdepth-1)
     h<-m$tree
     nvec<-m$kvec
     complete<-m$complete
     l[[i]]<-h }
   tmp[[2]]<-l
   return(list(tree=tmp, kvec=nvec, complete=complete))
}


#' Generates a complete derivation tree from an integer vector.
#'
#' @description \code{generateCDT()} 
#'    generates a derivation tree from an integer vector.
#'    The derivation tree may be incomplete.
#'    (For grammatical evolution).
#'
#' @param sym          Non-terminal symbol. 
#' @param kvec         Integer vector.
#' @param complete     Boolean. FALSE for incomplete derivation trees.
#' @param G            Grammar. 
#' @param maxdepth     Integer. Maximal depth of the derivation tree.    
#'
#' @details \code{generateDerivationTree()} recursively expands 
#'         non-terminals and builds a derivation tree.
#'
#' @return A named list l$tree, l$kvec, l$complete. 
#'
#' @family Generate Derivation Tree
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' a<-sample(100, 100, replace=TRUE)
#' b<-generateCDT(sym=g$Start, kvec=a, G=g, maxdepth=10)
#' decodeDT(b$tree, g$ST)
#'
#' @importFrom xegaBNF isTerminal
#' @export
generateCDT<-function(sym, kvec, complete=TRUE, G, maxdepth=5)
{
   if (xegaBNF::isTerminal(sym, G$ST))
   { return(list(tree=sym, kvec=kvec, complete=complete)) }
   #
   if (length(kvec)==0)
   { return(list(tree=sym, kvec=kvec, complete=complete)) }
   #
   tmp<-rndsubk(sym, kvec[1], G$PT)
   # cat("length(kvec):", length(kvec), "\n")
   if (length(kvec)==1) 
   { # cat("integers used up, use G$SPT.\n")
   tmp1<-rndsubk(sym, kvec[1], G$SPT)
   # print(decodeTree(tmp1, g$ST))
   return(list(tree=tmp1[[2]], kvec=sample(10*G$SPT$LHS,1), complete=TRUE)) }
   nvec<-kvec[2:length(kvec)]
   symbols<-tmp[[2]]
   l<-list()
   for (i in 1:length(symbols))
   { m<-generateCDT(symbols[i], nvec, complete, G, maxdepth-1)
     h<-m$tree
     nvec<-m$kvec
     complete<-m$complete
     l[[i]]<-h }
   tmp[[2]]<-l
   return(list(tree=tmp, kvec=nvec, complete=complete))
}

#' Decodes (and completes) a derivation tree into a working program.
#'
#' @description The program is guaranteed to work.
#'
#' @param tree     Derivation tree.
#' @param G        A Grammar object.
#' @param kvec     A random integer vector.
#'
#' @return A program
#'
#' @family Decoder
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' complete<-TRUE
#' while (complete) {
#' t1<-generateDerivationTree(sym=g$Start, kvec=sample(100, 10, replace=TRUE), G=g)
#' complete<-t1$complete}
#' decodeAndFixDT(t1$tree, G=g, kvec=sample(100, 10, replace=TRUE)) 
#'
#' @importFrom xegaBNF isTerminal
#' @export
decodeAndFixDT<-function(tree, G, kvec)
{ g1<-G; g1$PT<-G$SPT
nkvec<-kvec; expr<-""
c<-unlist(leavesIncompleteDT(tree, G$ST))
for (i in (1:length(c)))
{ if (xegaBNF::isTerminal(c[i], G$ST)) {expr<-paste(expr, G$ST$Symbols[c[i]], sep="")}
  else
    {d<-generateDerivationTree(sym=G$ST$SymbolId[c[i]], kvec=nkvec, G=g1)
     nkvec<-d$kvec; expr<-paste(expr,decodeDT(d$tree,G$ST), sep="") }
}
return(expr) }

#' Generate, decode, and show \code{times} derivation trees from random 
#' integer vectors for grammar BNF on the console.
#' 
#' @param times     Number of derivation trees which should be generated.
#' @param G         A grammar object G.
#' @param generateDT  Function for generating a derivation tree.
#' @param verbose   Boolean. If TRUE (default) , print decoded derivation tree on console.  
#'
#' @return Number of complete derivation trees generated.
#' 
#' @family Tests
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' testGenerateDerivationTree(5, g)
#' @export 
testGenerateDerivationTree<-function(times, G,
                            generateDT=generateDerivationTree, 
                            verbose=TRUE)
{
cDT<-0
for (i in 1:times) 
{ a<-sample(100, 100, replace=TRUE)
b<-generateDT(sym=G$Start, kvec= a, complete=TRUE, G=G, maxdepth=10)
if (b$complete) {cDT<-cDT+1}
if (verbose)
   { cat("Derivation Tree", i, "Complete:", b$complete, "\n")
     cat(decodeDT(b$tree, G$ST), "\n")
     if (!b$complete)
     { cat("Fixed Derivation Tree", i, "\n")
       cat(decodeAndFixDT(tree=b$tree, G=G, kvec=a), "\n\n") }}
}
return(cDT)
}

