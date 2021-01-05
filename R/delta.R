#' Generalized Kronecker Delta
#' 
#' Computes the Generalized Kronecker Delta.
#' 
#' @param n number of elements for each dimension.
#' @param p order of the generalized Kronecker delta, \code{p=1} for the standard Kronecker delta.
#' 
#' @return \code{array} representing the generalized Kronecker delta tensor.
#' 
#' @examples
#' ### Kronecker delta 3x3
#' delta(3)
#' 
#' ### generalized Kronecker delta 3x3 of order 2
#' delta(3, p = 2)  
#' 
#' @family tensor algebra
#' 
#' @references 
#' Guidotti, E. (2020). "calculus: High dimensional numerical and symbolic calculus in R". \url{https://arxiv.org/abs/2101.00086}
#' 
#' @export
#'
delta <- function(n, p = 1){
  
  mu <- as.matrix(expand.grid(lapply(1:p, function(i) 1:n)))
  
  delta <- cpp_parity(mu, mu)
  
  return(array(delta, dim = rep(n,2*p)))
  
}
