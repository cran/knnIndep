\name{generate.patchwork.copula}
\alias{generate.patchwork.copula}
\title{
Generate data from a non-functional dependence
}
\description{
Generate data from a non-functional dependence called 'patchwork copula'. Like a copula the data is uniform in x and y but it has a dependence between x and yy that has a block like structure
}
\usage{
generate.patchwork.copula(p = matrix(rbeta(bins * bins, alpha, beta), ncol = bins),
	 alpha = 0.01, beta = 1, c = 1, npoints = 320, bins = 20, returnmi = FALSE, 
	 plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
	matrix, starting mass distribution on the grid
}
  \item{alpha}{
	decimal, parameter of beta distribution used for \code{p} (if \code{p} left as per default)
}
  \item{beta}{
	decimal, parameter of beta distribution used for \code{p} (if \code{p} left as per default)
}
  \item{c}{
	decimal, concentration factor (default \code{1}), used to stabilize mutual information estimation  
}
  \item{npoints}{
	decimal, sample size
}
  \item{bins}{
	decimal, number of bins of the bins*bins grid
}
  \item{returnmi}{
	boolean, whether to return the mutual information
}
  \item{plot}{
	boolean, whether to plot the dependence 
}
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
	list with the following elements 
			\item{x}{matrix of x-coordinates, each column corresponds to a noise level from \code{noises}}
			\item{y}{matrix of y-coordinates, each column corresponds to a noise level from \code{noises}}
			\item{mi}{mutual information of the dependence, only return if \code{returnmi} is set to \code{TRUE}}
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
generate.patchwork.copula(bins=20,plot=TRUE)
}
