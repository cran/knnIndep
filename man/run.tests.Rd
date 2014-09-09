\name{run.tests}
\alias{run.tests}
\title{
Run several tests of independence on a benchmark of different functional relationships
}
\description{
This function runs a set of independence tests on a benchmark consisting of different functional dependence types (see \code{\link{generate.benchmark.data}})
}
\usage{
run.tests(fun, args, types, noises, size = 320, nsim = 500, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{
	function or character naming a function. A function should have two vectors of coordinates as first two arguments
}
  \item{args}{
 	list of additional arguments to the functions \code{fun}. If a function does not need any arguments use an empty list.
}
  \item{types}{
	numeric, which type of dependence to benchmark (see \code{\link{generate.benchmark.data}})
}
  \item{noises}{
	matrix of noise to add to each dependence. It should have \code{types} number of columns 
}
  \item{size}{
	numeric, size of the data sets to generate (default 320 points)
}
  \item{nsim}{
  	numeric, how many replicate simulations to run under the null model and H1, default 500
}
  \item{\dots}{
	additional arguments to pass on to function \code{\link{generate.benchmark.data}}
}
}
\details{
	This function makes use of \code{mclapply} so \code{MC_CORES} should be set to a number greater than 1 for parallelization
}
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
\code{\link{calculate.power},\link{generate.roc}}, \code{\link{generate.benchmark.data}}
}
\examples{
noises = cbind(lin=c(.1,.5,.8),circ=c(.2,.4,.6))
mycor = function(...) cor(...)^2
results.cor=run.tests(mycor,args=list(),types=c(1,7),noises=noises,nsim=50,size=100)
results = run.tests("novelTest.extreme",args=list(maxi=10),types=c(1,7),noises=noises,nsim=25,
	size=100)
\dontrun{
x11()
par(mfrow=c(1,ncol(noises)))
roc.plot(generate.roc(results,pval=FALSE),legend=noises)
}
power = t(drop(calculate.power(results,.95,`>`)))
power.cor = t(drop(calculate.power(results.cor,.95,`>`)))
#cor is excellent at linear relationships, not so much for circular relationships:
#(increasing power is an artifact of low number of simulation, increase nsim in run.tests)
power.plot(list(cor=power.cor, novelTest=power),noises,show.legend="topright",mains=c("Linear", 
	"Circle")) 
}
