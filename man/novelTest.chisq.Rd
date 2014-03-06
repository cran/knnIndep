\name{novelTest.chisq}
\alias{novelTest.chisq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A novel test of  independence
}
\description{
This function implements a novel test of independence of bivariate data. It is based on the formula of the exact distribution of the ith 
nearest neighbour given the previous nearest neighbour (see \code{\link{Pc_givena}}). 
}
\usage{
novelTest.chisq(xdata, ydata, maxi = length(xdata) - 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xdata}{
	first dimension of data
}
  \item{ydata}{
	second dimension of data
}
  \item{maxi}{
	up to which ith nearest neighbour to consider 
}
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
	This function returns an object of class \code{htest} with:
		\item{statistic }{The value of the statistic}
		\item{p.value}{p-value of the test}
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
%\seealso{
%
%}
\examples{
set.seed(10)
xylist = generate.benchmark.data(7,.3,100)
x = runif(100)
novelTest.chisq(x,xylist$y,maxi=20)
novelTest.chisq(xylist$x,xylist$y,maxi=20)
}

