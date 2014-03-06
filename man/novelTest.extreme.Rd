\name{novelTest.extreme}
\alias{novelTest.extreme}
\title{
A novel test of independence
}
\description{
This function implements a novel test of independence of bivariate data. It is based on the formula of the exact distribution of the ith 
nearest neighbour given the previous nearest neighbour (see \code{\link{Pc_givena}}). 
}
\usage{
novelTest.extreme(xdata, ydata, maxi = length(xdata) - 1)
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
This function returns the aggregated test statistic
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
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
set.seed(10)
xylist = generate.benchmark.data(7,.3,50)
x = runif(50)
novelTest.extreme(x,xylist$y,maxi=20)
novelTest.extreme(xylist$x,xylist$y,maxi=20)
}

