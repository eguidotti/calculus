#' Numerical and Symbolic Determinant
#' 
#' Computes the determinant of a \code{numeric} or \code{character} matrix.
#' 
#' @param x \code{numeric} or \code{character} matrix.
#' 
#' @return \code{numeric} or \code{character}.
#' 
#' @examples 
#' ### numeric matrix
#' x <- matrix(1:4, nrow = 2)
#' mxdet(x)
#' 
#' ### symbolic matrix
#' x <- matrix(letters[1:4], nrow = 2)
#' mxdet(x)
#' 
#' @family matrix algebra
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
mxdet <- function(x) {

  if(is.numeric(x))
    return(base::det(x))
  
  x.dim   <- dim(x)
  x.n.dim <- length(x.dim)

  if(x.n.dim!=2)
    stop("not a matrix")
  
  n <- unique(x.dim)
  if(length(n)!=1)
    stop("not a square matrix")
  
  if(getOption('calculus.auto.wrap', default = TRUE))
    x <- wrap(x)
  
  det <- cpp_det(x, n)
  
  if(det=="")
    return("0")
  
  return(det)
  
}
