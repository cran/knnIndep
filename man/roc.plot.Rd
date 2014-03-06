\name{roc.plot}
\alias{roc.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a ROC
}
\description{
This functions uses the results of \code{\link{generate.roc}} to plot a ROC plot
}
\usage{
roc.plot(pows, legend = NULL, cols = colorRampPalette(c("blue", "gray"))(dim(pows)[3]), 
	mains = c("Linear", "Quadratic", "Cubic", "Sine:period 1/2", 
		"Sine: period 1/8", "X^(1/4)", "Circle", "Step function", "Torus"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pows}{
	array, as returned by \code{\link{generate.roc}}
}
  \item{legend}{
	\code{NULL} (default) to disable legend or a matrix with noise levels as used in \code{\link{run.tests}}
}
  \item{cols}{
	colours to use for the plots
}
  \item{mains}{
 	main title for each plot
}
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
This function is used solely for its side effect of plotting
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
\code{\link{run.tests}}
}
\examples{
mycor = function(...) cor(...)^2
noises = cbind(lin=c(.1,.5,.8),circ=c(.2,.4,.6))
results.cor= run.tests(mycor,args=list(),types=c(1,7),noises=noises,nsim=100,size=50)
roc.plot(generate.roc(results.cor,pval=FALSE),legend=noises)
}
