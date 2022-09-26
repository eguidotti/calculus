#' Numerical and Symbolic Matrix Trace
#' 
#' Computes the trace of a \code{numeric} or \code{character} matrix.
#' 
#' @param x \code{numeric} or \code{character} matrix.
#' 
#' @return \code{numeric} or \code{character}.
#' 
#' @examples 
#' ### numeric matrix
#' x <- matrix(1:4, nrow = 2)
#' mxtr(x)
#' 
#' ### character matrix
#' x <- matrix(letters[1:4], nrow = 2)
#' mxtr(x)
#' 
#' @family matrix algebra
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
mxtr <- function(x){
  
  if(!is.matrix(x))
    stop("not a matrix")
  
  if(is.numeric(x))
    return(sum(diag(x)))
  
  cpp_collapse(diag(x), sep = " + ")
  
}
