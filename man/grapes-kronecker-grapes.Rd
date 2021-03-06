% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/operators.R
\name{\%kronecker\%}
\alias{\%kronecker\%}
\title{Numerical and Symbolic Kronecker Product}
\usage{
x \%kronecker\% y
}
\arguments{
\item{x}{\code{numeric} or \code{character} array.}

\item{y}{\code{numeric} or \code{character} array.}
}
\value{
\code{array}.
}
\description{
Computes the generalised Kronecker product of two \code{numeric} or \code{character} arrays.
}
\examples{
### numeric Kronecker product 
c(1,2) \%kronecker\% c(2,3)

### symbolic Kronecker product 
array(1:4, dim = c(2,2)) \%kronecker\% c("a","b")

}
\references{
Guidotti, E. (2020). "calculus: High dimensional numerical and symbolic calculus in R". \url{https://arxiv.org/abs/2101.00086}
}
\seealso{
Other basic arithmetic: 
\code{\link{\%diff\%}()},
\code{\link{\%div\%}()},
\code{\link{\%dot\%}()},
\code{\link{\%inner\%}()},
\code{\link{\%outer\%}()},
\code{\link{\%prod\%}()},
\code{\link{\%sum\%}()}
}
\concept{basic arithmetic}
