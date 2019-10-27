#' Numerical and Symbolic Determinant
#' 
#' Calculates the determinant of a matrix.
#' 
#' @param x numeric or character matrix. 
#' 
#' @return numeric or character determinant.
#' 
#' @examples 
#' # numeric matrix
#' x <- matrix(1:4, nrow = 2)
#' det(x)
#' 
#' # symbolic matrix
#' x <- matrix(letters[1:4], nrow = 2)
#' det(x)
#' 
#' @export
#' 
det <- function(x) {

  x <- as.matrix(x)
  if(is.numeric(x))
    return(base::det(x))
  
  x       <- as.matrix(x)
  x.dim   <- dim(x)
  x.n.dim <- length(x.dim)

  if(x.n.dim!=2)
    stop("not a matrix")
  
  n <- unique(x.dim)
  if(length(n)!=1)
    stop("not a square matrix")
  
  if(getOption('auto.wrap', default = TRUE))
    x <- wrap(x)
  
  det <- cpp_det(x, n)
  
  if(det=="")
    return("0")
  
  return(det)
  
}