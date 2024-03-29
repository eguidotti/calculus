% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mxtr.R
\name{mxtr}
\alias{mxtr}
\title{Numerical and Symbolic Matrix Trace}
\usage{
mxtr(x)
}
\arguments{
\item{x}{\code{numeric} or \code{character} matrix.}
}
\value{
\code{numeric} or \code{character}.
}
\description{
Computes the trace of a \code{numeric} or \code{character} matrix.
}
\examples{
### numeric matrix
x <- matrix(1:4, nrow = 2)
mxtr(x)

### character matrix
x <- matrix(letters[1:4], nrow = 2)
mxtr(x)

}
\references{
Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
}
\seealso{
Other matrix algebra: 
\code{\link{mxdet}()},
\code{\link{mxinv}()},
\code{\link{mx}()}
}
\concept{matrix algebra}
