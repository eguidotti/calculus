#' Taylor Series Expansion
#' 
#' Computes the Taylor series of \code{functions} or \code{characters}.
#' 
#' @param f \code{character}, or \code{function} returning a \code{numeric} scalar value.
#' @param var vector giving the variable names with respect to which the derivatives are to be computed and/or the point where the derivatives are to be evaluated (the center of the Taylor series). See \code{\link{derivative}}.
#' @param params \code{list} of additional parameters passed to \code{f}.
#' @param order the order of the Taylor approximation.
#' @param accuracy degree of accuracy for numerical derivatives.
#' @param stepsize finite differences stepsize for numerical derivatives. It is based on the precision of the machine by default.
#' @param zero tolerance used for deciding which derivatives are zero. Absolute values less than this number are set to zero.
#' 
#' @return \code{list} with components:
#' \describe{
#'  \item{f}{the Taylor series.}
#'  \item{order}{the approximation order.}
#'  \item{terms}{\code{data.frame} containing the variables, coefficients and degrees of each term in the Taylor series.}
#' }
#' 
#' @examples 
#' ### univariate taylor series (in x=0)
#' taylor("exp(x)", var = "x", order = 2)
#' 
#' ### univariate taylor series of user-defined functions (in x=0)
#' f <- function(x) exp(x)
#' taylor(f = f, var = c(x=0), order = 2)
#' 
#' ### multivariate taylor series (in x=0 and y=1)
#' taylor("x*(y-1)", var = c(x=0, y=1), order = 4)
#' 
#' ### multivariate taylor series of user-defined functions (in x=0 and y=1)
#' f <- function(x,y) x*(y-1)
#' taylor(f, var = c(x=0, y=1), order = 4)
#' 
#' ### vectorized interface
#' f <- function(x) prod(x)
#' taylor(f, var = c(0,0,0), order = 3)
#' 
#' @family polynomials
#' @family derivatives
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#'    
#' @export
#' 
taylor <- function(f, var, params = list(), order = 1, accuracy = 4, stepsize = NULL, zero = 1e-7){
  
  # init
  cache <- list()
  n.v <- length(var)
  is.fun <- is.function(f)
  
  # parse 
  if(is.character(f))
    f <- parse(text = f)
  
  # prepare envir to extract coefficients
  is.vectorized <- FALSE
  if(is.null(names(var))){
    if(is.character(var)){
      x0 <- rep(0, n.v)
      names(x0) <- var  
      x0 <- as.list(x0)
    }
    else {
      x0 <- var
      var <- paste0("x", 1:n.v)  
      names(x0) <- var
      x0 <- as.list(x0)
      is.vectorized <- TRUE
    }
  }
  else {
    x0  <- as.list(var)
    var <- names(var)
  }
  
  # coef indexes
  nu <- partitions(n = order, length = n.v, fill = TRUE, perm = TRUE, equal = FALSE)
  
  # taylor
  for(n in 1:ncol(nu)){
    v <- nu[,n]
    c <- list(degree = sum(v))
    
    if(!is.fun){
    
      prev <- 1
      expr <- f
      if(length(cache)>0){
        prev <- which(apply(v - nu, 2, function(x) sum(x)==1 & all(x>=0)))[[1]]
        expr <- cache[[prev]]$expr
      }
      
      c$expr <- derivative(f = expr, var = var, params = params, order = v - nu[,prev], deparse = FALSE)
      c$coef <- eval(c$expr, envir = c(x0, params))/prod(factorial(v))
    
    }
    else {
      
      x <- unlist(x0)
      if(is.vectorized)
        names(x) <- NULL
      
      c$coef <- D.num(f = f, x0 = x, params = params, order = v, accuracy = accuracy, stepsize = stepsize, zero = zero, drop = TRUE)
      c$coef <- c$coef/prod(factorial(v))
      
    }
    
    idx <- which(v>0)
    if(length(idx)==0) {
      c$var <- 1
    }
    else {
      vv <- var[idx]
      cc <- x0[vv]!=0
      if(any(cc)) vv[cc] <- sprintf("(%s-%s)", vv[cc], x0[vv][cc]) 
      c$var <- paste0(vv, "^", v[idx], collapse = "*")
    }
    
    cache[[paste(v, collapse = ',')]] <- c
  }
  
  # terms
  terms <- data.frame(var = sapply(cache, function(x) x$var),
                      coef = sapply(cache, function(x) x$coef),
                      degree = sapply(cache, function(x) x$degree), 
                      row.names = names(cache), stringsAsFactors = FALSE)
  
  # taylor series
  f <- cpp_collapse(cpp_paste(as.character(wrap(terms$coef)), as.character(terms$var), sep = " * "), sep = " + ")
  
  # return
  return(list(f = f, order = order, terms = terms))
  
}
