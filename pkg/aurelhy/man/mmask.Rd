\name{mmask}
\alias{mmask}
\non_function{}
\title{ A geomask object masking the Morocco terrain model }
\usage{data(mmask)}
\description{
The \code{mmask} dataset is a 'geomask' object (georeferenced mask). It
indicates which ones of the points in the \code{morocco} terrain model do belong
to the Morocco territory (\code{TRUE}), or are land outside (\code{FALSE}).
Points in the sea are flagged as \code{NA}.
}
\format{
  This 'geomask' object contains a grid of 1986 x 1866 booleans masking the
  \code{morocco} terrain model.
}

\seealso{ \code{\link{geomask}}, \code{\link{morocco}} , \code{\link{mbord}}}

\keyword{datasets}
