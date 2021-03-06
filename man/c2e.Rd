% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{c2e}
\alias{c2e}
\title{Characters to Expressions}
\usage{
c2e(x)
}
\arguments{
\item{x}{\code{character}.}
}
\value{
\code{expression}.
}
\description{
Converts \code{characters} to \code{expressions}.
}
\examples{
### convert characters
c2e("a")

### convert array of characters
c2e(array("a", dim = c(2,2)))

}
\references{
Guidotti, E. (2020). "calculus: High dimensional numerical and symbolic calculus in R". \url{https://arxiv.org/abs/2101.00086}
}
\seealso{
Other utilities: 
\code{\link{e2c}()},
\code{\link{evaluate}()},
\code{\link{wrap}()}
}
\concept{utilities}
