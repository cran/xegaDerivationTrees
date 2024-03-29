% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derivationTrees.R
\name{compileBNF}
\alias{compileBNF}
\title{Compile a  BNF (Backus-Naur Form) of a context-free grammar.}
\usage{
compileBNF(g, verbose = FALSE)
}
\arguments{
\item{g}{A character string with a BNF.}

\item{verbose}{Boolean. TRUE: Show progress. Default: FALSE.}
}
\value{
A grammar object (list) with the attributes 
        \itemize{
        \item \code{name}: Filename of the grammar.
        \item \code{ST}: Symbol table. 
        \item \code{PT}: Production table. 
        \item \code{Start}: Start symbol of the grammar.
        \item \code{SPT}: Short production table.
        }
}
\description{
\code{compileBNF()} produces a context-free grammar  
              from its specification in Backus-Naur form (BNF).   
              Warning: No error checking is implemented.
}
\details{
A grammar consists of the symbol table \code{ST}, the production
         table \code{PT}, the start symbol \code{Start}, 
         and the short production
         table \code{SPT}. 

The function performs the following steps:
 \enumerate{
 \item Make the symbol table. 
 \item Make the production table. 
 \item Extract the start symbol. 
 \item Compile a short production table.
 \item Return the grammar.}
}
\examples{
g<-compileBNF(booleanGrammar())
g$ST
g$PT
g$Start
g$SPT
}
\seealso{
Other Grammar: 
\code{\link{booleanGrammar}()}
}
\concept{Grammar}
