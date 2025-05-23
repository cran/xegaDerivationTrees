% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derivationTrees.R
\name{treeListDepth}
\alias{treeListDepth}
\title{Measures the depth of a (nested) list.}
\usage{
treeListDepth(t, tDepth = 0)
}
\arguments{
\item{t}{List.}

\item{tDepth}{Integer. List depth. Default: 0.}
}
\value{
Depth of a nested list.
}
\description{
\code{treeListDepth()} returns the depth of a nested list.  
             For a derivation tree, this is approximately twice
             the derivation depth.
}
\examples{
g<-compileBNF(booleanGrammar())
a<-randomDerivationTree(g$Start, g)
treeListDepth(a) 

}
\seealso{
Other Measures of Tree Attributes: 
\code{\link{treeLeaves}()},
\code{\link{treeNodes}()},
\code{\link{treeProbability}()},
\code{\link{treeSize}()}
}
\concept{Measures of Tree Attributes}
