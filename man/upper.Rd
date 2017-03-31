\name{upper}
\alias{upper}
\title{
upper
}
\description{
Returns the (d*(d-1)/2) x 2 matrix of the (i,j) indexes to the upper triangle of a d x d matrix. The result can then directly be used as an index, see example.
}
\usage{
upper(d)
}
\arguments{
  \item{d}{
	dimension of the square matrix which we intend to index.
}}
\value{
Index values
}
\author{
Pierrick Bruneau
}
\examples{
inds <- upper(5)
vals <- matrix(runif(25), nrow=5)
selvals <- vals[inds] # vector containing the values of the upper triangle
}

