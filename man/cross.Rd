% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/operators.R
\name{cross}
\alias{cross}
\alias{\%cross\%}
\title{Numerical and Symbolic Cross Product}
\usage{
cross(...)

x \%cross\% y
}
\arguments{
\item{...}{N-1 vectors of length N.}

\item{x}{3-d vector}

\item{y}{3-d vector}
}
\value{
N-dimensional vector orthogonal to the N-1 vectors.
}
\description{
Computes the generic cross product of N-1 vectors of length N.
}
\section{Functions}{
\itemize{
\item \code{cross}: N-d cross product

\item \code{\%cross\%}: 3-d cross product
}}

\examples{
# canonical basis 3-d
c(1,0,0) \%cross\% c(0,1,0)

# canonical basis 4-d
cross(c(1,0,0,0), c(0,1,0,0), c(0,0,0,1))

}