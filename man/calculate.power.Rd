\name{calculate.power}
\alias{calculate.power}
\title{
Calculate power at a given significance level
}
\description{
Function to calculate power at a given significance level. Uses the data structure returned by \code{\link{run.tests}} 
}
\usage{
calculate.power(vals, alpha = 0.95, comp = `>`)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vals}{
	list, values as returned by \code{\link{run.tests}} 
}
  \item{alpha}{
	significance level at which to return power
}
  \item{comp}{
	comparison function, for alpha < .5, it should probably be set to `<`
}
}
\details{
power is calculated as the fraction of tests that are higher or lower than (according to \code{comp}) than the significance level. The significance level is fixed on data generated under the null hypothesis. 
}
\value{
returns the power for applicable data from the structure vals, usually for each test it returns the power for all types of dependence and all noise levels. 
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{run.tests}} 
}
\examples{
mycor = function(...) cor(...)^2
vals = run.tests(mycor,list(),1:2,cbind(c(.3,.4,6),c(.3,.5,4)),100)
drop(calculate.power(vals))
}
