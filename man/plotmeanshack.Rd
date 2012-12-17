\name{plotmeanshack}
\alias{plotmeanshack}
\title{
plotmeanshack
}
\description{
Hack of the plotmeans function (gplots package), to allow native scale on the x axis, if the associated grouping variable is numeric.
}
\usage{
plotmeanshack(formula, data = NULL, subset, na.action, bars = TRUE, 
    p = 0.95, minsd = 0, minbar = NULL, maxbar = NULL, xlab = names(mf)[2], 
    ylab = names(mf)[1], mean.labels = FALSE, ci.label = FALSE, 
    n.label = TRUE, digits = getOption("digits"), col = "black", 
    barwidth = 1, barcol = "blue", connect = TRUE, ccol = col, 
    legends = names(means), xaxt, use.t = TRUE, nummeans=TRUE, ...)
}
\arguments{
  \item{formula}{
	see documentation of gplots::plotmeans
  }
  \item{data}{
	see documentation of gplots::plotmeans
  }
  \item{subset}{
	see documentation of gplots::plotmeans
  }
  \item{na.action}{
	see documentation of gplots::plotmeans
  }
  \item{bars}{
	see documentation of gplots::plotmeans
  }
  \item{p}{
	see documentation of gplots::plotmeans
  }
  \item{minsd}{
	see documentation of gplots::plotmeans
  }
  \item{minbar}{
	see documentation of gplots::plotmeans
  }
  \item{maxbar}{
	see documentation of gplots::plotmeans
  }
  \item{xlab}{
	see documentation of gplots::plotmeans
  }
  \item{ylab}{
	see documentation of gplots::plotmeans
  }
  \item{mean.labels}{
	see documentation of gplots::plotmeans
  }
  \item{ci.label}{
	see documentation of gplots::plotmeans
  }
  \item{n.label}{
	see documentation of gplots::plotmeans
  }
  \item{digits}{
	see documentation of gplots::plotmeans
  }
  \item{col}{
	see documentation of gplots::plotmeans
  }
  \item{barwidth}{
	see documentation of gplots::plotmeans
  }
  \item{barcol}{
	see documentation of gplots::plotmeans
  }
  \item{connect}{
	see documentation of gplots::plotmeans
  }
  \item{ccol}{
	see documentation of gplots::plotmeans
  }
  \item{legends}{
	see documentation of gplots::plotmeans
  }
  \item{xaxt}{
	see documentation of gplots::plotmeans
  }
  \item{use.t}{
	see documentation of gplots::plotmeans
  }
  \item{nummeans}{
	if TRUE, the independent variable (r.h.s.) in formula should be numeric
  }
  \item{...}{
	see documentation of gplots::plotmeans
  }
}
\author{
Pierrick Bruneau
}
\examples{
data(state)
plotmeanshack(state.area ~ state.region, nummeans=FALSE) # non-numeric independent variable
data(iris)
irisdat <- cbind(iris[,1:4], c(rep(1,50), rep(2,50), rep(5,50)))
names(irisdat)[5] <- "iris.class"
plotmeanshack(Sepal.Width ~ iris.class, data=irisdat, nummeans=TRUE) # (artificial) numeric variable
}



