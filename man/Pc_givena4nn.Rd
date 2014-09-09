\name{Pc_givena4nn}
\alias{Pc_givena4nn}
\title{
Probability of observing the ith nearest neighbour at distance greater or equal
c given the 4 previous nearest neighbours
}
\description{
This function gives the probability of observing the ith nearest neighbour at
distance c given the previous 4 nearest neighbour distances, $P(d_i >= x | d_(i-1), d_(i-2), d_(i-3), d_(i-4))$
}
\usage{
Pc_givena4nn(i, c, a, k1, k2, N)
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
  \item{k1}{
    vector, number of previous neighbhour at distance d_i	
}
  \item{k2}{
	vector, number of preivous neighbhours at distance d_(i-1)
}
  \item{N}{
	numeric, size of the dataset
}
}
\details{
The probability is calculated by ranking the data and assuming that the data lie on a torus. For details see Dümcke et al. "A novel test for independence derived from an exact distribution of ith nearest neighbours" (PLoS ONE 2014)
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
Pc_givena4nn(10,2:7,1:6,rep(0,6),rep(1,6),20)
}
