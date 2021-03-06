% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{diagonal}
\alias{diagonal}
\alias{diagonal<-}
\title{Tensor Diagonals}
\usage{
diagonal(x = 1, dim = rep(2, 2))

diagonal(x) <- value
}
\arguments{
\item{x}{an \code{array} from which to extract the diagonals, or a vector giving the diagonal values to construct the \code{array}.}

\item{dim}{the dimensions of the (square) \code{array} to construct when \code{x} is a vector.}

\item{value}{vector giving the values of the diagonal entries.}
}
\value{
Vector of the diagonal entries of \code{x} if \code{x} is an \code{array}. 
If \code{x} is a vector, returns the diagonal \code{array} with the 
entries given by \code{x}.
}
\description{
Functions to extract or replace the diagonals of an \code{array}, or construct a diagonal \code{array}.
}
\section{Functions}{
\itemize{
\item \code{diagonal<-}: set diagonals.
}}

\examples{
### 3x3 matrix
diagonal(x = 1, dim = c(3,3))

### 2x2x2 array
diagonal(x = 1:2, dim = c(2,2,2))

### extract diagonals 
x <- diagonal(1:5, dim = c(5,5,5))
diagonal(x)

### set diagonals
x <- array(0, dim = c(2,2,2))
diagonal(x) <- 1:2
x

}
\references{
Guidotti, E. (2020). "calculus: High dimensional numerical and symbolic calculus in R". \url{https://arxiv.org/abs/2101.00086}
}
\seealso{
Other tensor algebra: 
\code{\link{contraction}()},
\code{\link{delta}()},
\code{\link{einstein}()},
\code{\link{epsilon}()},
\code{\link{index}()}
}
\concept{tensor algebra}
