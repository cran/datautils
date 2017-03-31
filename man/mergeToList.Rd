\name{mergeToList}
\alias{mergeToList}
\title{
mergeToList
}
\description{
Merges k objects (lists or vectors), all being of length L, into a list object of length L, with each list cell being a list of the k elements in L-th position in their respective object.
}
\usage{
mergeToList(...)
}
\arguments{
  \item{...}{
	k objects to be merged. Their lengths are checked, and should be all equal.
}}
\value{
Merged list as specified above.
}
\author{
Pierrick Bruneau
}
\examples{
temp <- mergeToList(c(1,2), list(3,4), c(5,6))
}

