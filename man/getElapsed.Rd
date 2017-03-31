\name{getElapsed}
\alias{getElapsed}
\title{
getElapsed
}
\description{
return the decimal number of seconds elapsed since a reference timestamp.
}
\usage{
getElapsed(stamp)
}
\arguments{
  \item{stamp}{
timestamp of a reference.
}
}
\value{
decimal number of seconds since reference timestamp.
}
\author{
Pierrick Bruneau
}
\seealso{
getTimestamp
}
\examples{
stamp <- getTimestamp()
Sys.sleep(1)
stamp2 <- getElapsed(stamp)
}

