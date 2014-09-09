\name{optimise.copula.mi}
\alias{optimise.copula.mi}
\title{
optimize the parameter \code{c} of \code{\link{generate.patchwork.copula}}
}
\description{
Find the correct \code{c} parameter for the patchwork copula (\code{\link{generate.patchwork.copula}}) to reach a certain mutual information value
}
\usage{
optimise.copula.mi(mis, distribution, interval = c(-10, 5), npoints)
}
\arguments{
  \item{mis}{
    traget mutual information values
}
  \item{distribution}{
    matrix. Choices of alpha and beta parameter of \code{\link{generate.patchwork.copula}} e.g. \code{matrix(rbeta(bins*bins,.01,1),ncol=bins)} 
}
  \item{interval}{
    search interval for solution
}
  \item{npoints}{
    sample size
}
}
\value{
    vector of values to be used as concentration facor \code{c} in \code{\link{generate.patchwork.copula}} to achive the input MI value
}
\author{
Sebastian Dümcke \email{duemcke@mpipz.mpg.de}
}
\seealso{
\code{\link{generate.patchwork.copula}}
}
\examples{
bins=10
knnIndep:::optimise.copula.mi(c(0.001,.01,.5,2),matrix(rbeta(bins*bins,.01,1),ncol=bins),npoints=10)
}
