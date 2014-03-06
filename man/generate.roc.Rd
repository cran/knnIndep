\name{generate.roc}
\alias{generate.roc}
\title{
Generate ROC curve data
}
\description{
Generate data suitable for ROC curve plotting from the results of \code{\link{run.tests}}
}
\usage{
generate.roc(vals, pval = TRUE)
}
\arguments{
  \item{vals}{
	list, data structure as returned by \code{\link{run.tests}} 
}
  \item{pval}{
	boolean, whether the values in \code{vals} represent pvalues
}
}
\details{
calculates the power via \code{\link{calculate.power}} for all significance levels from 0 to 1.
}
\value{
array of values suitable for plotting via \code{\link{roc.plot}}
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
\code{\link{run.tests}} and \code{\link{roc.plot}} for plotting
}
\examples{
noises <- cbind(lin=c(.1,.5,.8),circ=c(.2,.4,.6))
mycor <- function(...) cor(...)^2
results.cor <- run.tests(mycor,args=list(),types=c(1,7),noises=noises,nsim=100,size=50)
roc.data <- generate.roc(results.cor,pval=FALSE)
roc.plot(roc.data,legend=noises)
}
