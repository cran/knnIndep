\name{generate.benchmark.data}
\alias{generate.benchmark.data}
\title{
Generating functional dependencies
}
\description{
Generate functional dependencies for benchmarking tests of independence. This function can generate 8 types of functional dependence: linear, quadratic, cubic, 
two sine functions, x^(1/4), step function and a circular dependence. 
}
\usage{
generate.benchmark.data(typ, noises, n, project = FALSE, windx = 1, windy = 1)
}
\arguments{
  \item{typ}{
     decimal, which type of dependence to generate. 
     1: linear
     2: quadratic
     3: cubic 
     4: sine period pi/4
     5: sine period pi/16
     6: x^(1/4)
     7: circle
     8: step function
}
  \item{noises}{
  	vector of noise values to apply to the generated dependence. The noise is normally distributed. 
}
  \item{n}{
  	decimal, size of sample to return. 
}
  \item{project}{
  	boolean (default FALSE), wether to project the generated dependence onto a torus
}
  \item{windx}{
  	decimal, how many times the dependence should wind around the torus in x-direction. Only used if \code{project} is \code{TRUE}
}
  \item{windy}{
	decimal, how many times the dependence should wind around the torus in y-direction. Only used if \code{project} is \code{TRUE}
}
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
	list with two elements
		\item{x}{matrix of x-coordinates, each column corresponds to a noise level from \code{noises}}
		\item{y}{matrix of y-coordinates, each column corresponds to a noise level from \code{noises}}
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
\code{\link{generate.patchwork.copula}} for generating non-functional dependence and \code{\link{run.tests}} for benchmarking tests of independence
}
\examples{
#generate a quadratic dependence of 10 points with two noise levels 0.3 and 0.6
generate.benchmark.data(2,c(.3,.6),10)
plot(generate.benchmark.data(4,.2,1000))
}
