#' Levi-Civita Symbol
#' 
#' Computes the Levi-Civita totally antisymmetric tensor.
#' 
#' @param n number of dimensions.
#' 
#' @return \code{array} representing the Levi-Civita symbol.
#' 
#' @examples 
#' ### Levi-Civita symbol in 2 dimensions
#' epsilon(2)
#' 
#' ### Levi-Civita symbol in 3 dimensions
#' epsilon(3)
#' 
#' @family tensor algebra
#' 
#' @references 
#' Guidotti, E. (2020). "calculus: High dimensional numerical and symbolic calculus in R". \url{https://arxiv.org/abs/2101.00086}
#' 
#' @export
#'
epsilon <- function(n){
  
  mu <- as.matrix(expand.grid(lapply(1:n, function(i) 1:n)))
  
  eps <- cpp_parity(x = mu, y = matrix(1:n, nrow = 1))
  
  return(array(eps, dim = rep(n,n)))
  
}
