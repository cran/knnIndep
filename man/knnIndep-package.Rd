\name{knnIndep-package}
\alias{knnIndep-package}
\alias{knnIndep}
\docType{package}
\title{
A package giving the formulas of an exact distribution of ith nearest neighbours
and two associated tests for independence 
}
\description{
This package provides the formulas to calculate the probability of observing the ith 
nearest neighbour given the (i-1)th nearest neighbour. Additionally this formulas is used in 
independence testing and this package provides implementations for two tests of independence 
\code{\link{novelTest.chisq}} and \code{\link{novelTest.extreme}}.

This package also provides a mean to benchmark test for independence on many 
different type of functional dependences and a new type of non-functional dependence.
}
\details{
\tabular{ll}{
Package: \tab knnIndep\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-03-06\cr
License: \tab GPL>=3.0\cr
}
For benchmarking purposes refer to \code{\link{run.tests}} and \code{\link{generate.benchmark.data}}.
The formula is given by \code{\link{P_ceq},\link{P_cge_ale}} and \code{\link{Pc_givena}}.
The two tests of independence are \code{\link{novelTest.chisq}} and \code{\link{novelTest.extreme}}.
}
\author{
Sebastian Dümcke \email{duemcke@mpipz.mpg.de}

The author is also the maintainer.
}
%\references{
%~~ Literature or other references for background information ~~
%}

\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
%\examples{
%~~ simple examples of the most important functions ~~
%}
