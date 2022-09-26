#' Numerical and Symbolic Matrix Inverse
#' 
#' Computes the inverse of a \code{numeric} or \code{character} matrix.
#' 
#' @param x \code{numeric} or \code{character} matrix.
#' 
#' @return \code{numeric} or \code{character} matrix. 
#' 
#' @examples 
#' ### numeric matrix
#' x <- matrix(1:4, nrow = 2, byrow = TRUE)
#' mxinv(x)
#' 
#' ### symbolic matrix
#' x <- matrix(letters[1:4], nrow = 2, byrow = TRUE)
#' mxinv(x)
#' 
#' @family matrix algebra
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
mxinv <- function(x) {

  if(is.numeric(x))
    return(solve(x))
  
  x.dim   <- dim(x)
  x.n.dim <- length(x.dim)
  
  if(x.n.dim!=2)
    stop("not a matrix")
  
  n <- unique(x.dim)
  if(length(n)!=1)
    stop("not a square matrix")
  
  det <- mxdet(x)
  if(det=="0")
    stop("det = 0. The matrix is not invertible")
  if(n==1)
    return(array(sprintf("1/%s", det), dim = c(1,1)))
  
  ij <- 1:n
  ij <- expand.grid(list(ij, ij))
    
  adj <- apply(ij, 1, function(ij){
    mxdet(x[-ij[2], -ij[1], drop=FALSE])
  })
  
  odd <- (rowSums(ij) %% 2 == 1) & (adj != "0")
  adj[odd] <- paste0('-', wrap(adj[odd]))
  adj[!odd] <- wrap(adj[!odd])
  
  return(array(cpp_paste(adj, wrap(det), sep = " / "), dim = rep(n,2)))

}
