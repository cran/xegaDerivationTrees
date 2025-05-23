% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derivationTrees.R
\name{chooseRule}
\alias{chooseRule}
\title{Selects a production rule index at random from a vector of production rules.}
\usage{
chooseRule(riv)
}
\arguments{
\item{riv}{Vector of production rules indices for 
a non-terminal symbol.}
}
\value{
Integer. Index of the production rule.
}
\description{
\code{chooseRule()} selects a production rule index 
     from the vector of production rule indices 
     in the \code{g$PT$LHS} for a non-terminal symbol.
}
\examples{
chooseRule(c(7, 8, 9))
chooseRule(as.vector(1))
}
\seealso{
Other Random Choice: 
\code{\link{chooseNode}()}
}
\concept{Random Choice}
