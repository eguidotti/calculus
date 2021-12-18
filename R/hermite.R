#' Hermite Polynomials
#' 
#' Computes univariate and multivariate Hermite polynomials.
#' 
#' @param order the order of the Hermite polynomial.
#' @param sigma the covariance \code{matrix} of the Gaussian kernel.
#' @param var \code{character} vector giving the variables of the polynomial.
#' @param transform \code{character} vector representing a change of variables. See details.
#' 
#' @details Hermite polynomials are obtained by differentiation of the Gaussian kernel:
#' 
#' \deqn{H_{\nu}(x,\Sigma) = exp \Bigl( \frac{1}{2} x_i \Sigma_{ij} x_j \Bigl) (- \partial_x )^\nu exp \Bigl( -\frac{1}{2} x_i \Sigma_{ij} x_j \Bigl)}
#' 
#' where \eqn{\Sigma} is a \eqn{d}-dimensional square matrix and 
#' \eqn{\nu=(\nu_1 \dots \nu_d)} is the vector representing the order of 
#' differentiation for each variable \eqn{x = (x_1\dots x_d)}. 
#' In the case where \eqn{\Sigma=1} and \eqn{x=x_1} the formula reduces to the 
#' standard univariate Hermite polynomials:
#' 
#' \deqn{H_{\nu}(x) = e^{\frac{x^2}{2}}(-1)^\nu \frac{d^\nu}{dx^\nu}e^{-\frac{x^2}{2}}}
#' 
#' If \code{transform} is not \code{NULL}, the variables \code{var} \eqn{x} are replaced with
#' \code{transform} \eqn{f(x)} to compute the polynomials \eqn{H_{\nu}(f(x),\Sigma)}
#' 
#' @return 
#' \code{list} of Hermite polynomials with components:
#' \describe{
#'  \item{f}{the Hermite polynomial.}
#'  \item{order}{the order of the Hermite polynomial.}
#'  \item{terms}{\code{data.frame} containing the variables, coefficients and degrees of each term in the Hermite polynomial.}
#' }
#' 
#' @examples
#' ### univariate Hermite polynomials up to order 3
#' hermite(3)
#'
#' ### multivariate Hermite polynomials up to order 2
#' hermite(order = 2, 
#'         sigma = matrix(c(1,0,0,1), nrow = 2), 
#'         var = c('z1', 'z2'))
#'         
#' ### multivariate Hermite polynomials with transformation of variables
#' hermite(order = 2, 
#'         sigma = matrix(c(1,0,0,1), nrow = 2), 
#'         var = c('z1', 'z2'),
#'         transform = c('z1+z2','z1-z2'))
#'         
#' @family polynomials
#' 
#' @references 
#' Guidotti, E. (2020). "calculus: High dimensional numerical and symbolic calculus in R". \url{https://arxiv.org/abs/2101.00086}
#' 
#' @export
#' 
hermite <- function(order, sigma = 1, var = 'x', transform = NULL){
  
  sigma <- as.matrix(sigma)
  
  if(length(var)==1 && var=='x' && ncol(sigma)>1) 
    var <- paste0('x', 1:ncol(sigma))
  
  if(any(dim(sigma)!=length(var)))
    stop("sigma must be a square matrix with ncol and nrow equal to the length of var")
  
  if(!is.null(transform)) if(any(dim(var)!=dim(transform)))
    stop("parameters transform and var must be of the same length")
  
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
  
  if(!is.null(transform)){
    
    d <- length(var)
    var.reg <- paste0("\\b",var,"\\b")
    var.tmp <- paste0("#",var,"#")
    var.new <- wrap(transform)
    
    H <- lapply(H, function(h){
      
      for(i in 1:d) h$f <- gsub(x = h$f, pattern = var.reg[i], replacement = var.tmp[i])
      for(i in 1:d) h$f <- gsub(x = h$f, pattern = var.tmp[i], replacement = var.new[i], fixed = TRUE)
      
      taylor(h$f, var = var, order = h$order)
      
    })
    
  }
  
  return(H)
}
