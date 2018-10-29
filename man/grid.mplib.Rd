\name{grid.mplib}
\alias{grid.mplib}
\alias{mplibGrob}
\alias{mplibGrob.mppath}
\alias{mplibGrob.mpcontrols}
\alias{mplibGrob.mpcontrolList}
\title{
  Draw a MetaPost curve.
}
\description{
  Draw a MetaPost curve in \pkg{grid} graphics.
}
\usage{
\method{mplibGrob}{mppath}(x, gp = gpar(), name = NULL)
\method{mplibGrob}{mpcontrols}(x, gp = gpar(), name = NULL)
\method{mplibGrob}{mpcontrolList}(x, gp = gpar(), name = NULL)
grid.mplib(...)
}
\arguments{
  \item{x}{
    A MetaPost path, either unsolved (a description generated using
    \code{\link{knot}} etc), or solved (as produced by
    \code{\link{mpsolve}}).
  }
  \item{gp}{
    Graphical parameters (from a call to \code{gpar}).
  }
  \item{name}{
    A name for the grob that is created.
  }
  \item{\dots}{
    Arguments passed to \code{mplibGrob}.
  }
}
\value{
  \code{mplibGrob} creates a \code{"mplibgrob"} object.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{knot}},
  \code{\link{mpsolve}},
  \code{\link{grid.metapost}}.
}
\examples{
p <- knot(0, 0) + dir(0) + dir(0) + knot(1, 1) 
grid.mplib(p)
}
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")
