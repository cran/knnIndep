\name{Pc_givena}
\alias{Pc_givena}
\title{
Probability of observing the ith nearest neighbour at distance greater or equal c given the (i-1)th nearest neighbour at distance a
}
\description{
This function gives the probability of observing the ith nearest neighbour at distance c given the (i-1)th nearest neighbour at distance a, $P(d_i <= x | d_(i-1) = a)$
}
\usage{
Pc_givena(i, c, a, N)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i}{
	numeric, which nearest neighbour to consider
}
  \item{c}{
	vector, the distance at which the ith NN was observed
}
  \item{a}{
	vector, the distance at which the (i-1)th NN was observed, a <= c
}
  \item{N}{
	numeric, size of the dataset
}
}
\details{
The probability is calculated by ranking the data and assuming that the data lie on a torus. For details see Dümcke et al. "A novel test for independence derived from an exact distribution of ith nearest neighbours" (manuscript in preparation)
}
\value{
Probability vector
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
\code{\link{P_cge_ale}}, \code{\link{P_ceq}}
}
\examples{
Pc_givena(10,2:7,1:6,20)
}
