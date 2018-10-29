\name{mplib-package}
\alias{mplib-package}
\alias{mplib}
\docType{package}
\title{
  Interface to the mplib Library
}
\description{
  Provides a limited interface to the METAPOST library mplib.
  This interface can be used to describe a METAPOST path and then
  obtain the knots that METAPOST calculates to satisfy the path.
}
\details{
  Generate a MetaPost path with functions like \code{\link{knot}}.

  Solve the path (generate Bezier control points from the path)
  with \code{\link{mpsolve}}.

  Draw a solved path with \code{\link{grid.mplib}}.

  It is also possible to pass \code{\link{grid.mplib}} the original
  path (and it will perform the solve step itself).
}
\author{
Paul Murrell <paul@stat.auckland.ac.nz>
}
\references{
  Hobby, J. D. and the MetaPost development team (2018). METAPOST a
  user's manual. \url{https://www.tug.org/docs/metapost/mpman.pdf}
}
\keyword{ package }
\seealso{
  \code{\link{knot}}
  \code{\link{mpsolve}}
  \code{\link{grid.mplib}}
}
\examples{
options(metapath.units="in")
p <- knot(0, 0) + dir(0) + dir(0) + knot(1, 1)
grid.mplib(p)
}
