#' Levi-Civita Symbol
#' 
#' Computes the Levi-Civita totally antisymmetric tensor.
#' 
#' @param n dimension
#' 
#' @return array representing the Levi-Civita tensor.
#' 
#' @examples 
#' # Levi-Civita tensor in 2-d
#' levicivita(2)
#' 
#' # Levi-Civita tensor in 3-d
#' levicivita(3)
#' 
#' @export
#'
levicivita <- function(n){
  
  mu <- as.matrix(expand.grid(lapply(1:n, function(i) 1:n)))
  
  eps <- cpp_parity(x = mu, y = matrix(1:n, nrow = 1))
  
  return(array(eps, dim = rep(n,n)))
  
}
