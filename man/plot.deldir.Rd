\name{.plot.deldir.datautils}
\alias{plot.deldir}
\title{
.plot.deldir.datautils
}
\description{
overrides the original plot.deldir function, to support background colors when printing the tesselation.
}
\usage{
\method{plot}{deldir}(x, add = FALSE, wlines = c("both", "triang", "tess"), 
    wpoints = c("both", "real", "dummy", "none"), number = FALSE, 
    cex = 1, nex = 1, col = NULL, lty = NULL, pch = NULL, xlim = NULL, 
    ylim = NULL, xlab = "x", ylab = "y", showrect = FALSE, fill = NULL, ...)
}
\arguments{
  \item{x}{
	see documentation of deldir::plot.deldir
  }
  \item{add}{
	see documentation of deldir::plot.deldir
  }
  \item{wlines}{
	see documentation of deldir::plot.deldir
  }
  \item{wpoints}{
	see documentation of deldir::plot.deldir
  }
  \item{number}{
	see documentation of deldir::plot.deldir
  }
  \item{cex}{
	see documentation of deldir::plot.deldir
  }
  \item{nex}{
	see documentation of deldir::plot.deldir
  }
  \item{col}{
	see documentation of deldir::plot.deldir
  }
  \item{lty}{
	see documentation of deldir::plot.deldir
  }
  \item{pch}{
	see documentation of deldir::plot.deldir
  }
  \item{xlim}{
	see documentation of deldir::plot.deldir
  }
  \item{ylim}{
	see documentation of deldir::plot.deldir
  }
  \item{xlab}{
	see documentation of deldir::plot.deldir
  }
  \item{ylab}{
	see documentation of deldir::plot.deldir
  }
  \item{showrect}{
	see documentation of deldir::plot.deldir
  }
  \item{fill}{
	vector of colors (in any valid R color format). Each color in the vector is used for the background of the Voronoi cell of the associated element in x.
  }
  \item{...}{
	see documentation of deldir::plot.deldir
  }
}
\author{
Pierrick Bruneau
}
\examples{
xvals <- rnorm(50)
yvals <- rnorm(50)
res <- deldir(xvals, yvals)

rvalues <- runif(50)
gvalues <- runif(50)
bvalues <- runif(50)
plot(res, wlines="tess", fill=rgb(rvalues, gvalues, bvalues))
}

