\name{benchmark.patchwork.copula}
\alias{benchmark.patchwork.copula}
\title{
Benchmark function for a new type of non-functional dependence
}
\description{
This function is used to benchmark test for independence on a new type of 
non-function dependence called patchwork copula
}
\usage{
benchmark.patchwork.copula(fun, args, cvals, n = 320, nsim = 500, bins = 20)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{
	function or character naming a function. A function should have two vectors of coordinates as first two arguments
}
  \item{args}{
	list of additional arguments to the functions \code{fun}. If a function does not need any arguments use an empty list.	
}
  \item{cvals}{
	target mutual information values
	vector of concentration factors, these represent mutual information values (see \code{\link{generate.patchwork.copula}})
}
  \item{n}{
	numeric, size of the data sets to generate (default 320 points)
}
  \item{nsim}{
  	numeric, how many replicate simulations to run under the null model and H1, default 500
}
  \item{bins}{
	decimal, number of bins of the bins*bins grid, (see \code{\link{generate.patchwork.copula}})
}
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
This function returns a list data structure that can be further processed with the functions of this package, \code{\link{calculate.power},\link{generate.roc}}
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{
Sebastian Dümcke \email{duemcke@mpipz.mpg.de}
}
%\note{
%%%  ~~further notes~~
%}

\seealso{
\code{\link{calculate.power},\link{generate.roc}}
}
\examples{
mycor = function(...) cor(...)^2
copula.vals = benchmark.patchwork.copula(mycor,list(),c(.3,1,10))
drop(calculate.power(copula.vals,.95))
roc.plot(generate.roc(copula.vals))
}
