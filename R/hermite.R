#' Hermite Polynomials
#' 
#' Computes univariate and multivariate Hermite polynomials.
#' 
#' @param order integer. The order of the Hermite polynomial.
#' @param sigma the covariance matrix of the Gaussian kernel.
#' @param var character. The variables of the polynomial.
#' 
#' @details 
#' Hermite polynomials are obtained by successive differentiation of the Gaussian kernel
#' \deqn{H_{\nu}(x,\Sigma) = exp \Bigl( \frac{1}{2} x^\dagger \Sigma x \Bigl) (- \partial_x )^\nu exp \Bigl( -\frac{1}{2} x^\dagger \Sigma x \Bigl)}
#' where \eqn{\Sigma} is a d-dimensional square matrix and \eqn{\nu=(\nu_1, ..., \nu_d)} is the vector representing the order of differentiation for each variable.
#' 
#' @return 
#' list of Hermite polynomials with components
#' \describe{
#'  \item{f}{the Hermite polynomial.}
#'  \item{order}{the order of the Hermite polynomial.}
#'  \item{terms}{data.frame containing the variables, coefficients and degrees of each term in the Hermite polynomial.}
#' }
#' 
#' @examples
#' # univariate Hermite polynomials up to order 3
#' hermite(3)
#' 
#' # univariate Hermite polynomials with variable z
#' hermite(3, var = 'z')
#' 
#' # multivariate Hermite polynomials up to order 2
#' hermite(order = 2, 
#'         sigma = matrix(c(1,0,0,1), nrow = 2), 
#'         var = c('z1', 'z2'))
#' 
#' @export
#' 
hermite <- function(order, sigma = 1, var = 'x'){
  
  sigma <- as.matrix(sigma)
  
  if(length(var)==1 && var=='x' && ncol(sigma)>1) 
      var <- paste0('x', 1:ncol(sigma))
  
  if(any(dim(sigma)!=length(var)))
    stop("sigma must be a square matrix with ncol and nrow equal to the length of var")
  
  # cache
  H <- list()
  
  # kernel
  kernel <- sprintf('exp(%s)', (-0.5 * sigma) %inner% (var %outer% var))
  
  nu <- partitions(n = order, length = length(var), fill = TRUE, perm = TRUE, equal = FALSE)
  for(i in 1:ncol(nu)){
    v <- nu[,i]
    
    prev <- 1
    expr <- kernel
    if(length(H)>0){
      prev <- which(apply(v - nu, 2, function(x) sum(x)==1 & all(x>=0)))[[1]]
      expr <- H[[prev]]$f
      expr <- paste0(wrap(expr), " * " , kernel)
    }
    
    expr <- parse(text = expr)
    h <- derivative(f = expr, var = var, order = v - nu[,prev], deparse = FALSE)
    h <- paste0((-1)^sum(v - nu[,prev]), " * ", wrap(e2c(h)))
    h <- gsub(x = h, pattern = kernel, replacement = '1', fixed = T)  
    
    H[[paste(v, collapse = ',')]] <- taylor(h, var = var, order = sum(v))
    
  }
  
  return(H)
}
