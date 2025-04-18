#
# Derivation Tree Package
# (c) 2020 A. Geyer-Schulz
# Package derivationTrees
#

#
# Generating a random derivation tree
#

#' A constant function which returns the BNF (Backus-Naur Form) 
#' of a context-free grammar for the XOR problem.
#'
#' @details Imported from package xegaBNF for use in examples.
#'
#' @return A named list with elements \code{$filename} and  \code{$BNF} 
#'         representing the grammar of a boolean grammar with two variables and
#'         the boolean functions \code{AND}, \code{OR}, and \code{NOT}.
#'
#' @family Grammar 
#'
#' @examples
#' booleanGrammar()
#' @importFrom xegaBNF booleanGrammar
#' @export
booleanGrammar<-xegaBNF::booleanGrammar

#' Compile a  BNF (Backus-Naur Form) of a context-free grammar.
#' 
#' @description \code{compileBNF()} produces a context-free grammar  
#'               from its specification in Backus-Naur form (BNF).   
#'               Warning: No error checking is implemented.
#'
#' @details A grammar consists of the symbol table \code{ST}, the production
#'          table \code{PT}, the start symbol \code{Start}, 
#'          and the short production
#'          table \code{SPT}. 
#'
#' The function performs the following steps:
#'  \enumerate{
#'  \item Make the symbol table. 
#'  \item Make the production table. 
#'  \item Extract the start symbol. 
#'  \item Compile a short production table.
#'  \item Return the grammar.}
#' 
#' @param g        A character string with a BNF. 
#' @param verbose  Boolean. TRUE: Show progress. Default: FALSE. 
#' 
#' @return A grammar object (list) with the attributes 
#'         \itemize{
#'         \item \code{name}: Filename of the grammar.
#'         \item \code{ST}: Symbol table. 
#'         \item \code{PT}: Production table. 
#'         \item \code{Start}: Start symbol of the grammar.
#'         \item \code{SPT}: Short production table.
#'         }
#'
#' @family Grammar
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' g$ST
#' g$PT
#' g$Start
#' g$SPT
#' @importFrom xegaBNF compileBNF
#' @export
compileBNF<-xegaBNF::compileBNF

#' Selects a production rule index at random from a vector of production rules.
#'
#' @description \code{chooseRule()} selects a production rule index 
#'      from the vector of production rule indices 
#'      in the \code{g$PT$LHS} for a non-terminal symbol.
#'
#' @param riv  Vector of production rules indices for 
#'                    a non-terminal symbol.
#'
#' @return Integer. Index of the production rule. 
#'
#' @family Random Choice
#'
#' @examples
#' chooseRule(c(7, 8, 9))
#' chooseRule(as.vector(1))
#' @export
chooseRule<- function(riv) {return(riv[sample(length(riv),1)])}

#' Codes the substitution of a non-terminal symbol by the symbols 
#' derived by a production rule as a nested list. 
#' 
#' @description \code{substituteSymbol()} 
#' generates a nested list with the non-terminal symbol as the root 
#' (first list element) and the derived symbols as the second list element.
#'
#' @param rindex   Rule index.
#' @param PT       Production table.
#'
#' @return 2-element list.
#'
#' @family Generate Derivation Tree
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' substituteSymbol(3, g$PT)
#'
#' @importFrom xegaBNF derive
#' @export
substituteSymbol<- function(rindex, PT)
   { a<-xegaBNF::derive(rindex, PT$RHS)
     b<-list()
     b[[1]]<-PT$LHS[rindex]
     b[[2]]<-a
     return(b)}

#' Transforms a non-terminal symbol into a random 1-level derivation tree.
#'
#' @description \code{rndsub()} expands a non-terminal by a random derivation
#'              and returns a 1-level derivation tree.
#'
#' @param sym      Non-terminal symbol.
#' @param PT       Production table.
#'
#' @return Derivation tree with 1-level.
#'
#' @family Generate Derivation Tree
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' rndsub(g$Start, g$PT)
#'
#' @importFrom xegaBNF rules
#' @export
rndsub<-function(sym, PT){substituteSymbol(chooseRule(xegaBNF::rules(sym, PT$LHS)),PT)}

#' Generates a random derivation tree.
#'
#' @description \code{randomDerivationTree()} 
#'    generates a random derivation tree.
#'
#' @details \code{RandomDerivationTree()} recursively expands 
#'         non-terminals and builds a depth-bounded derivation tree.
#'
#' @param sym          Non-terminal symbol. 
#' @param G            Grammar. 
#' @param maxdepth     Integer. Maximal depth of the derivation tree.    
#' @param CompleteDT   Boolean. Generate a complete derivation tree? 
#'                     Default: TRUE.
#'
#' @return Derivation tree (a nested list).
#'
#' @family Generate Derivation Tree
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' a<-randomDerivationTree(g$Start, g)
#' b<-randomDerivationTree(g$Start, g, maxdepth=10)
#' c<-randomDerivationTree(g$Start, g, 2, FALSE)
#'
#' @importFrom xegaBNF isTerminal
#' @export
randomDerivationTree<-function(sym, G, maxdepth=5, CompleteDT=TRUE)
{
   if (xegaBNF::isTerminal(sym, G$ST))
   { return(sym) }

   # if ((maxdepth<0) && (!CompleteDT)) {return(1)}
   if ((maxdepth<0) && (!CompleteDT)) {return(sym)}
   else
   { if (maxdepth<0) {PT<-G$SPT} else {PT<-G$PT} }

   tmp<-rndsub(sym, PT)
   symbols<-tmp[[2]]
   l<-list()
   for (i in 1:length(symbols))
   { h<-randomDerivationTree(symbols[i], G, maxdepth-1, CompleteDT)
	   l[[i]]<-h }
   tmp[[2]]<-l
   return(tmp)   
}

#' Randomly partitions n in k parts.
#'
#' @description Sampling a partition is a two-step process:
#'   \enumerate{
#'   \item The k parts of the partion are sampled in the loop.
#'         This implies that the first partition p is a random number 
#'         between 1 and 1+n-k. The next partition is sampled from 
#'         1 to 1+n-k-p. 
#'   \item We permute the partitions.      
#'   }
#' 
#' @param n   The integer to divide.
#' @param k   Number of parts.
#'
#' @return The integer partition of n in k parts.    
#'
#' @family Unused
#'
#' @examples
#'  rndPartition(10, 4)
#'@export
rndPartition<-function(n, k)
{
if (k==1) {return(n)}
r<-rep(0,k)
nn<-1+n-k
for (i in (1:(k-1)))
{ r[i]<-sample(1:nn, 1)
nn<-1+nn-r[i] }
r[k]<-n-sum(r)
p<-sample(k, k, replace=FALSE)
return(r[p])
}

#
# Measures of tree attributes
#

#' Measures the depth of a (nested) list.
#'
#' @description \code{treeListDepth()} returns the depth of a nested list.  
#'              For a derivation tree, this is approximately twice
#'              the derivation depth.
#'
#' @param t       List.
#' @param tDepth  Integer. List depth. Default: 0.
#'
#' @return Depth of a nested list.
#'
#' @family Measures of Tree Attributes
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' a<-randomDerivationTree(g$Start, g)
#' treeListDepth(a) 
#'
#' @export
treeListDepth <- function(t,tDepth=0){
  if(is.list(t) && length(t) == 0){return(0)}
  if(!is.list(t)){
    return(tDepth) }else{
    return(max(unlist(lapply(t,treeListDepth,tDepth=tDepth+1))))    
  }
}

#' Measures the number of symbols in a derivation tree.
#'
#' @description \code{treeSize()} returns the number of symbols in a 
#'              derivation tree.
#'
#' @param tree    Derivation tree.
#'
#' @return Integer. Number of symbols in a derivation tree.
#'
#' @family Measures of Tree Attributes
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' a<-randomDerivationTree(g$Start, g)
#' treeSize(a) 
#'
#' @export
treeSize<-function(tree)
{ return(length(unlist(tree)))}

#' Measures the number of inner nodes in a derivation tree.
#'
#' @description \code{treeNodes()} returns 
#'              the number of non-terminal symbols in a 
#'              derivation tree.
#'
#' @param tree   Derivation tree.
#' @param ST     Symbol table.
#'
#' @return Integer. Number of non-terminal symbols in a derivation tree.
#'
#' @family Measures of Tree Attributes
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' a<-randomDerivationTree(g$Start, g)
#' treeNodes(a, g$ST) 
#'
#' @importFrom xegaBNF isNonTerminal
#' @export
treeNodes<-function(tree, ST)
{ return(sum(unlist(lapply(unlist(tree),FUN=xegaBNF::isNonTerminal, ST=ST))))}

#' Measures the number of leaves of a complete derivation tree.
#'
#' @description \code{treeLeaves()} returns 
#'              the number of terminal symbols in a 
#'              complete derivation tree.
#'
#' @param tree    Derivation tree.
#' @param ST      Symbol table.
#'
#' @return Integer. Number of terminal symbols in a complete derivation tree.
#'
#' @family Measures of Tree Attributes
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' a<-randomDerivationTree(g$Start, g)
#' treeLeaves(a, g$ST) 
#' ((treeLeaves(a, g$ST)+treeNodes(a, g$ST)) == treeSize(a))
#'
#' @importFrom xegaBNF isTerminal
#' @export
treeLeaves<-function(tree, ST)
{ return(sum(unlist(lapply(unlist(tree),FUN=xegaBNF::isTerminal, ST=ST))))}

#' The (path) probability of generating \code{tree} by grammar \code{G}.
#'
#' @description \code{treeProbability()} returns the path
#'              probability of generating the derivation tree \code{tree} 
#'              by the context-free grammar \code{G}. 
#'              The probability is exact, if the grammar is not ambiguous and
#'              if the grammar does not contain redundant rules.
#'             
#' @param tree    Derivation tree.
#' @param G       A context-free grammar. 
#'
#' @return Real. The probability of generating \code{tree} 
#'               by grammar \code{G}.
#'
#' @family Measures of Tree Attributes
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' a<-randomDerivationTree(g$Start, g)
#' decodeTree(a, g$ST) 
#' treeProbability(tree=a, G=g)
#'
#' @importFrom xegaBNF isNonTerminal
#' @importFrom xegaBNF rules 
#' @export
treeProbability<-function(tree, G)
{Prob<-function(symbol, G)
{if(xegaBNF::isNonTerminal(symbol, G$ST))
     {return(1/length(xegaBNF::rules(symbol, G$PT$LHS)))}
else {return(1.0)}}
return(prod(unlist(lapply(unlist(tree),FUN=Prob, G=G))))}

#
# tree Helpers:
#

#' Returns the root of a derivation tree.
#'
#' @description \code{treeRoot()} returns the root of a derivation tree.
#'
#' @param tree  Derivation tree.
#'
#' @return Root of a derivation tree.
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' a<-randomDerivationTree(g$Start, g)
#' treeRoot(a) 
#'
#' @family Access Tree Parts
#'
#' @export
treeRoot<-function(tree)
{ return(tree[[1]][1])}

#' Returns the children of a derivation tree.
#'
#' @description \code{treeChildren()} returns the children of a derivation tree
#'          represented as a list of derivation trees.
#'
#' @param tree     Derivation tree.
#'
#' @return The children of a derivation tree (a list of derivation trees).
#'
#' @family Access Tree Parts
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' a<-randomDerivationTree(g$Start, g)
#' treeChildren(a) 
#'
#' @export
treeChildren<-function(tree)
{ return(tree[[2]])}

#
# treeANL: Attributed Node List: 
# Node$ID, Node$NT, Node$Pos, Node$Depth, Node$RDepth, Node$subtreedepth
# Node$Index,

#' Builds an Attributed Node List (ANL) of a derivation tree.
#'
#' @description \code{treeANL()} recursively traverses a derivation tree
#'     and collects information about the derivation tree in an attributed
#'     node list (ANL).
#' 
#' @details     An attributed \code{node} has the following elements:
#'     \itemize{
#'         \item \code{$ID}:  Id in the symbol table \code{ST}.
#'         \item \code{$NonTerminal}:  Is the symbol a non-terminal?
#'         \item \code{$Pos}: Position in the trail.
#'         \item \code{$Depth}:  Depth of node.
#'         \item \code{$Rdepth}: Residual depth for expansion.
#'         \item \code{$subtreedepth}: Depth of subtree starting here.
#'         \item \code{$Index}:  R index of the node in the derivation tree.
#'                            Allows fast tree extraction and insertion.
#'        }
#'    These elements can be used e.g. 
#'    \itemize{
#'        \item for inserting and extracting subtrees 
#'        (\code{Pos} or  \code{node$Index}), 
#'        \item for checking
#'              the feasibility of subtree substitution (\code{ID}),
#'        \item for checking depth bounds 
#'              (\code{Depth}, \code{RDepth}, and \code{subtreedepth}),
#'              \dots
#'        }
#'
#' @param tree     A derivation tree.
#' @param ST       A symbol table.
#' @param maxdepth Limit on the depth of a derivation tree.
#' @param ANL      Attributed node list (empty on invocation). 
#' @param IL       Index function list  (empty on invocation).
#' @param count    Trail count (1 on invocation).
#' @param depth    Derivation tree depth (1 on invocation).
#'
#' @return A list with three elements:
#'         \enumerate{
#'         \item \code{$count}: The trail length (not needed).
#'         \item \code{$subtreedepth}: The derivation tree depth (not needed).
#'         \item \code{$ANL}:   The attributed node list is a list of nodes.
#'           Each node is represented as a list of the following attributes:
#'         \itemize{
#'         \item \code{Node$ID}:  Id in the symbol table ST.
#'         \item \code{Node$NonTerminal}:  Is the symbol a non-terminal?
#'         \item \code{Node$Pos}: Position in the trail.
#'         \item \code{Node$Depth}:  Depth of node.
#'         \item \code{Node$Rdepth}: Residual depth for expansion.
#'         \item \code{Node$subtreedepth}: Depth of subtree starting here.
#'         \item \code{Node$Index}:  R index of the node in the derivation tree.
#'                            Allows fast tree extraction and insertion.
#'         } 
#'                        
#'         }
#'
#' @family Access Tree Parts
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' a<-randomDerivationTree(g$Start, g)
#' b<-treeANL(a, g$ST)
#' c<-treeANL(a, g$ST, 10)
#' d<-treeANL(a, g$ST, maxdepth=10)
#' 
#' @importFrom xegaBNF isTerminal
#' @importFrom xegaBNF isNonTerminal
#' @export
treeANL<-function(tree, ST, maxdepth=5, ANL=list(), IL=list(),  
		  count=1, depth=1)
{ root<-treeRoot(tree)
    thiscount<-count
    if (xegaBNF::isTerminal(root, ST)) {
            r<-list()
            r$count<-count
            r$subtreedepth<-1
            r$ANL<list()
            return(r)}
    kids<-treeChildren(tree)
    subtreedepth<-0
    anl<-list()
    inl<-list()
    for (i in 1:length(kids))
    { 
#### Control of the depth of the insertion point? TBD
       inl<-append(IL, paste("[[2]][[", as.character(i), "]]", sep=""))
       r<-treeANL(kids[[i]], ST, maxdepth, ANL=list(), inl, count+1, depth+1)
       count<-r$count
       subtreedepth<-max(subtreedepth, r$subtreedepth)
       anl<-append(anl,r$ANL) }
    Node<-list()
    Node$ID<-root
    Node$NonTerminal<-xegaBNF::isNonTerminal(root,ST)
    Node$Pos<-thiscount
    Node$Depth<-depth
    Node$Rdepth<-maxdepth-depth
    Node$subtreedepth<-subtreedepth
    Node$Index<-paste(unlist(IL), sep="", collapse="")
    ANL<-append(ANL, list(Node))
    ANL<-append(ANL, anl)
    r<-list()
    r$count<-count
    r$subtreedepth<-subtreedepth+1
    r$ANL<-ANL
    return(r)
}

#' Filter an Attributed Node List (ANL) of a derivation tree by depth.
#'
#' @description \code{filterANL()} deletes all nodes whose depth 
#'              \code{node$Depth}  is 
#'              less than \code{minb} and larger than \code{maxb}
#'              from the ANL. 
#'              However, if the resulting list is empty, the original
#'              ANL is returned.
#'              
#' 
#' @details     An attributed \code{node} has the following elements:
#'     \itemize{
#'         \item \code{$ID}:  Id in the symbol table \code{ST}.
#'         \item \code{$NonTerminal}:  Is the symbol a non-terminal?
#'         \item \code{$Pos}: Position in the trail.
#'         \item \code{$Depth}:  Depth of node.
#'         \item \code{$Rdepth}: Residual depth for expansion.
#'         \item \code{$subtreedepth}: Depth of subtree starting here.
#'         \item \code{$Index}:  R index of the node in the derivation tree.
#'                            Allows fast tree extraction and insertion.
#'        }
#'
#' @param ANL     Attributed node list.
#' @param minb    Integer. 
#' @param maxb    Integer.  
#'
#' @return An attributed node list with nodes whose depths are in 
#'         \code{minb:maxb}. 
#'           Each node is represented as a list of the following attributes:
#'         \itemize{
#'         \item \code{Node$ID}:  Id in the symbol table ST.
#'         \item \code{Node$NonTerminal}:  Is the symbol a non-terminal?
#'         \item \code{Node$Pos}: Position in the trail.
#'         \item \code{Node$Depth}:  Depth of node.
#'         \item \code{Node$Rdepth}: Residual depth for expansion.
#'         \item \code{Node$subtreedepth}: Depth of subtree starting here.
#'         \item \code{Node$Index}:  R index of the node in the derivation tree.
#'                            Allows fast tree extraction and insertion.
#'         } 
#'
#' @family Access Tree Parts
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' set.seed(111)
#' a<-randomDerivationTree(g$Start, g, maxdepth=10)
#' b<-treeANL(a, g$ST)
#' c<-filterANL(b, minb=1, maxb=3)
#' d<-filterANL(b, minb=3, maxb=5)
#' e<-filterANL(b, minb=14, maxb=15)
#' f<-filterANL(b, minb=13, maxb=15)
#' 
#' @importFrom xegaBNF isTerminal
#' @importFrom xegaBNF isNonTerminal
#' @export
filterANL<-function(ANL, minb=1, maxb=3)
{ nodelist<-ANL$ANL
  nlist<-list()
  for (i in (1:length(nodelist)))
  {  if (nodelist[[i]]$Depth %in% (minb:maxb))
  {nlist<-c(nlist, nodelist[i])}
  } 
  if (length(nlist)==0) {nlist<-ANL$ANL}
  return(list(count=ANL$count, 
               subtreedepth=ANL$subtreedepth, 
               ANL=nlist))
}

#' Filter an Attributed Node List (ANL) of a derivation tree by a symbol identifier.
#'
#' @description \code{filterANLid()} deletes all nodes whose \code{node$ID} does not match 
#'              \code{node$ID}.
#'              If the resulting list is empty, a list of length 0 is returned.
#' 
#' @details     An attributed \code{node} has the following elements:
#'     \itemize{
#'         \item \code{$ID}:  Id in the symbol table \code{ST}.
#'         \item \code{$NonTerminal}:  Is the symbol a non-terminal?
#'         \item \code{$Pos}: Position in the trail.
#'         \item \code{$Depth}:  Depth of node.
#'         \item \code{$Rdepth}: Residual depth for expansion.
#'         \item \code{$subtreedepth}: Depth of subtree starting here.
#'         \item \code{$Index}:  R index of the node in the derivation tree.
#'                            Allows fast tree extraction and insertion.
#'        }
#'
#' For the implementation of crossover and mutation, we expect a non-terminal symbol identifier.  
#'
#' @param ANL       Attributed node list.
#' @param nodeID    Integer. The identifier of a symbol. 
#'
#' @return An attributed node list with nodes whose depths are in 
#'         \code{minb:maxb}. 
#'           Each node is represented as a list of the following attributes:
#'         \itemize{
#'         \item \code{Node$ID}:  Id in the symbol table ST.
#'         \item \code{Node$NonTerminal}:  Is the symbol a non-terminal?
#'         \item \code{Node$Pos}: Position in the trail.
#'         \item \code{Node$Depth}:  Depth of node.
#'         \item \code{Node$Rdepth}: Residual depth for expansion.
#'         \item \code{Node$subtreedepth}: Depth of subtree starting here.
#'         \item \code{Node$Index}:  R index of the node in the derivation tree.
#'                            Allows fast tree extraction and insertion.
#'         } 
#'
#' @family Access Tree Parts
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' set.seed(111)
#' a<-randomDerivationTree(g$Start, g, maxdepth=10)
#' b<-treeANL(a, g$ST)
#' c<-filterANLid(b, nodeID=5)
#' d<-filterANLid(b, nodeID=6)
#' e<-filterANLid(b, nodeID=7)
#' f<-filterANLid(b, nodeID=8)
#' 
#' @importFrom xegaBNF isTerminal
#' @importFrom xegaBNF isNonTerminal
#' @export
filterANLid<-function(ANL, nodeID=1)
{ nodelist<-ANL$ANL
  nlist<-list()
  for (i in (1:length(nodelist)))
  {  if (nodelist[[i]]$ID == nodeID)
  {nlist<-c(nlist, nodelist[i])}
  } 
  return(list(count=ANL$count, 
               subtreedepth=ANL$subtreedepth, 
               ANL=nlist))
}

#
# Random choice in node list.
#

#' Randomly selects an attributed node in an attributed node list.
#'
#' @description \code{chooseNode()} returns  a random attributed node 
#'              from an attributed node list.
#  
#' @details     An attributed \code{node} has the following elements:
#'     \itemize{
#'        \item \code{ID}
#'        \item \code{NonTerminal}
#'        \item \code{Pos}
#'        \item \code{Depth}
#'        \item \code{Rdepth}
#'        \item \code{subtreedepth}
#'        \item \code{Index}
#'        }
#'    These elements can be used e.g. 
#'    \itemize{
#'        \item for inserting and extracting subtrees 
#'        (\code{Pos} or  \code{node$Index}), 
#'        \item for checking
#'              the feasibility of subtree substitution (\code{ID}),
#'        \item for checking depth bounds 
#'              (\code{Depth}, \code{RDepth}, and \code{subtreedepth}),
#'              \dots
#'        }
#'
#' @param ANL     Attributed node list.
#'
#' @return An attributed node.  
#'
#' @family Random Choice
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' a<-randomDerivationTree(g$Start, g)
#' b<-treeANL(a, g$ST)
#' c<-chooseNode(b$ANL)
#'
#' @export
chooseNode<-function(ANL)
{
	return(ANL[[sample(length(ANL), 1)]])
}

#
# Test of compatibility of subtrees: 
# 1. same root symbol
# Depth bounds:
# 2.1 depth(node1) + depth(subtree2) \leq maxdepth+2
# 2.2 depth(node2) + depth(subtree1) \leq maxdepth+2
#  
# TODO: Replace 3 by Max derivations needed in SPT.

#' Test the compatibility of subtrees.
#'
#' @description \code{compatibleSubtrees()} tests the compatibility of two 
#'        subtrees. 
#'     
#' @details \code{compatibleSubtrees()} tests the compatibility of two 
#'        subtrees:
#'        \enumerate{
#'      \item The root symbol of the two subtrees must be identical:
#'            \code{(n1$ID==n2$ID)}.
#'      \item The depth restrictions must hold:
#'          \enumerate{
#'              \item \code{depth(n1) + depth(subtree2) <= maxdepth+maxSPT}
#'              \item \code{depth(n2) + depth(subtree1) <= maxdepth+maxSPT} 
#'                 } 
#'               maxSPT is the maximal number of derivations needed 
#'                to generate a complete derivation tree.}
#'
#' @param n1      Attributed node of the root of subtree 1.    
#' @param n2      Attributed node of the root of subtree 2. 
#' @param maxdepth Integer. Maximal derivation depth.
#' @param DepthBounded \itemize{
#'                  \item \code{TRUE}: Only subtrees 
#'                         with the same root symbol that respect 
#'                        the depth restrictions are compatible. 
#'                  \item \code{FALSE}: The depth restrictions are not 
#'                                      checked.}
#'                       
#' @family Tree Operations 
#'
#' @return \code{TRUE} or \code{FALSE}
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' t1<-randomDerivationTree(g$Start, g)
#' t1anl<-treeANL(t1, g$ST)
#' t2<-randomDerivationTree(g$Start, g)
#' t2anl<-treeANL(t2, g$ST)
#' n1<-chooseNode(t1anl$ANL)
#' n2<-chooseNode(t2anl$ANL)
#' compatibleSubtrees(n1, n2)
#' compatibleSubtrees(n1, n2, maxdepth=1)
#' compatibleSubtrees(n1, n2, DepthBounded=FALSE)
#'
#' @export
compatibleSubtrees<-function(n1, n2, maxdepth=5, DepthBounded=TRUE)
{
	if (!identical(n1$ID, n2$ID)) {return(FALSE)}
	if (identical(DepthBounded, FALSE)) {return(TRUE)}
	if (((n1$Depth+n2$subtreedepth)<(maxdepth+3)) &&
	((n2$Depth+n1$subtreedepth)<(maxdepth+3)))
	{ return(TRUE)} else {return(FALSE)}
}

#
# Extracting a subtree of a derivation tree 
#

#' Extracts the subtree at position \code{pos} in a derivation tree.
#'
#' @description \code{treeExtract()} returns 
#'              the subtree at position \code{pos} in a derivation tree.
#'
#' @details     An attributed \code{node} is a list 
#'              whose element \code{node$Index} contains 
#'              an access function to the node. 
#'              The access function is represented as a string 
#'              with an executable R index expression.
#'              All what remains to be done, is 
#'              \itemize{
#'              \item  to complete 
#'              the access statement and  
#'              \item to return 
#'              the result of parsing and evaluating the string.
#'              }
#'
#' @param tree     Derivation tree.
#' @param node     Attributed node.
#'
#' @return Derivation tree.
#'
#' @family Tree Operations 
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' t1<-randomDerivationTree(g$Start, g)
#' t1anl<-treeANL(t1, g$ST)
#' n1<-chooseNode(t1anl$ANL)
#' st1<-treeExtract(t1, n1)
#' decodeCDT(st1, g$ST)
#' st2<-treeExtract(t1, chooseNode(t1anl$ANLa))
#' decodeCDT(st2, g$ST)
#'
#' @export
treeExtract<-function(tree, node)
{      
	a<-paste("tree",node$Index, sep="")
	return(eval(parse(text=a)))
}

#
# Inserting a subtree of a derivation tree
#

#' Inserts a subtree into a derivation tree at a \code{node}.
#'
#' @description \code{treeInsert()} inserts a \code{subtree} into 
#'              a \code{tree} at a \code{node}.
#'              
#' @details     An attributed \code{node} is a list 
#'              whose element \code{node$Index} contains 
#'              an access function to the node. 
#'              The access function is represented as a string 
#'              which contains an executable R index expression.
#'              All what remains to be done, is 
#'              \itemize{
#'              \item to complete 
#'              the assignment statement and 
#'              \item to parse and evaluate the string.
#'              }
#'
#' @param tree     Derivation tree.
#' @param subtree  Subtree.
#' @param node     Attributed node.
#'
#' @return A derivation tree.
#'
#' @family Tree Operations 
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' t1<-randomDerivationTree(g$Start, g)
#' t2<-randomDerivationTree(g$Start, g)
#' t1anl<-treeANL(t1, g$ST)
#' n1<-chooseNode(t1anl$ANL)
#' t2<-randomDerivationTree(n1$ID, g)
#' tI1<-treeInsert(t1, t2, n1)
#' decodeCDT(tI1, g$ST)
#'
#' @export
treeInsert<-function(tree, subtree, node)
{ 
	a<-paste("tree",node$Index,"<-subtree",  sep="")
        eval(parse(text=a))
    return(tree) }

#
# 4. decode a random derivation tree
#

#' Returns a list of all symbols of a derivation tree  
#' in depth-first left-to-right order.
#'
#' @description \code{decodeTree()} returns a
#'              list of all symbols of a derivation tree 
#'              in depth-first left-to-right order
#'              (coded as R Factor with the symbol identifiers as levels).
#'
#' @param tree     Derivation tree.
#' @param ST       Symbol table.
#'
#' @return List of all symbols in depth-first left-to-right order.
#'
#' @family Decoder
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' a<-randomDerivationTree(g$Start, g)
#' decodeTree(a, g$ST) 
#'
#' @export
decodeTree<-function(tree, ST)
{
	ST[unlist(tree),1]
}

#' Converts a complete derivation tree into a program.
#'
#' @description \code{decodeCDT()} returns a program
#'              (a text string with the terminal symbol string).
#'              If the derivation tree still has non-terminal leaves,
#'              the non-terminal leaves are omitted.
#'              The program produces a syntax error.
#'              The program can not be repaired.
#'
#' @param tree     Derivation tree.
#' @param ST       Symbol table.
#'
#' @return A program.
#'
#' @family Decoder
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' a<-randomDerivationTree(g$Start, g)
#' decodeCDT(a, g$ST) 
#'
#' @importFrom xegaBNF isTerminal
#' @export
decodeCDT<-function(tree, ST)
{
	a<-unlist(tree)
	c<-a[as.logical(unlist(lapply(a,FUN=xegaBNF::isTerminal, ST=ST)))]
	b<-unlist(lapply(ST[c,1],as.character))
        d<-Reduce(b, f=paste0)
	return(d)
}

#' Returns the list of symbol identifiers
#'              of the leaves of a derivation tree.
#'
#' @description For incomplete derivation trees, non-terminal symbols
#'              are leaves.
#'
#' @details Must perform a depth-first left-to-right tree traversal to collect 
#'          all leave symbols (terminal and non-terminal symbols). 
#'
#' @param tree            Derivation tree.
#' @param ST              Symbol table.
#' @param leavesList      List of symbol identifiers. 
#'
#' @return List of symbol identifiers.
#'
#' @family Decoder
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' a<-randomDerivationTree(g$Start, g)
#' leavesIncompleteDT(a, g$ST) 
#'
#' @importFrom xegaBNF isTerminal
#' @export
leavesIncompleteDT<-function(tree, ST, leavesList=list())
{
        root<-treeRoot(tree)
        if (xegaBNF::isTerminal(root, ST))
           { leavesList<-append(leavesList, root); 
             return(leavesList)}
        if ((xegaBNF::isNonTerminal(root, ST)) & (length(tree)==1))
           { leavesList<-append(leavesList, root); 
             return(leavesList)}
        kids<-treeChildren(tree)
        lL<-list()
        for (i in 1:length(kids))
        {
          r<-leavesIncompleteDT(kids[[i]], ST, leavesList)
          lL<-append(lL, r)
        }  
	return(lL)
}

#' Decodes a derivation tree into a list of the leaf symbols
#' of the derivation tree.
#'
#' @param tree     Derivation tree.
#' @param ST       Symbol table.
#'
#' @return List of the leaf symbols of the derivation tree.
#'
#' @family Decoder
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' t1<-generateDerivationTree(sym=g$Start,sample(100, 10, replace=TRUE), G=g)
#' decodeDTsym(t1$tree, g$ST) 
#'
#' @importFrom xegaBNF isTerminal
#' @export
decodeDTsym<-function(tree, ST)
{
        a<-unlist(leavesIncompleteDT(tree, ST))
	return(ST[a,1])
}

#' Decodes a derivation tree into a program.
#'
#' @description The program may contain non-terminal symbols
#'              and its evaluation may fail.
#'
#' @param tree     Derivation tree.
#' @param ST       Symbol table.
#'
#' @return A program
#'
#' @family Decoder
#'
#' @examples
#' g<-compileBNF(booleanGrammar())
#' t1<-generateDerivationTree(sym=g$Start,sample(100, 10, replace=TRUE), G=g)
#' decodeDT(t1$tree, g$ST) 
#'
#' @importFrom xegaBNF isTerminal
#' @export
decodeDT<-function(tree, ST)
{
        a<-unlist(leavesIncompleteDT(tree, ST))
	return(Reduce(ST[a,1], f=paste0))
}

# end of file

