\name{getPurity}
\alias{getPurity}
\title{
getPurity
}
\description{
Gets the purity of a label vector wrt a ground truth (in the context of a clustering algorithm).
}
\usage{
getPurity(truthLabels, inferLabels)
}
\arguments{
  \item{truthLabels}{
	ground truth labels, to which inferred labels are compared to compute the purity value
  }
  \item{inferLabels}{
    vector of inferred labels, which should have the same length as truthLabels
}}
\value{
purity value in [0,1]
}
\author{
Pierrick Bruneau
}
\examples{
temp <- getPurity(c(1,1,1,2,1,5,3,4,5,3), c(2,2,2,3,1,1,3,4,2,3))
}

