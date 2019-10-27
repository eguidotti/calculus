#' Taylor Series
#' 
#' Computes the Taylor series for functions, expressions or characters.
#' 
#' @param f function, expression or character
#' @param var character. The variables of \code{f}.
#' @param order the order of the Taylor approximation.
#' @param accuracy accuracy degree for numerical derivatives.
#' @param stepsize finite differences stepsize for numerical derivatives. Auto-optimized by default.
#' 
#' @return list with components
#' \describe{
#'  \item{f}{the Taylor series.}
#'  \item{order}{the approximation order.}
#'  \item{terms}{data.frame containing the variables, coefficients and degrees of each term in the Taylor series.}
#' }
#' 
#' @examples 
#' # univariate taylor series 
#' taylor('exp(x)', var = 'x', order = 3)
#' 
#' # univariate taylor series of arbitrary functions
#' taylor(function(x) exp(x), var = 'x', order = 3)
#' 
#' # multivariate taylor series 
#' taylor('sin(x*y)', var = c('x','y'), order = 6)
#' 
#' # multivariate taylor series of arbitrary functions
#' taylor(function(x,y) sin(x*y), var = c('x','y'), order = 6)
#' 
#' @export
#' 
taylor <- function(f, var = 'x', order = 1, accuracy = 2, stepsize = NULL){
  
  cache <- list()
  n.v <- length(var)
  is.fun <- is.fun(f)
  
  if(length(f)!=1)
    stop("f must be of length 1")
  
  # parse 
  if(is.character(f))
    f <- parse(text = f)
  
  # prepare envir to extract coefficients
  if(is.null(names(var))){
    x0 <- rep(0, n.v)
    names(x0) <- var 
    x0 <- as.list(x0)
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
      
      c$expr <- derivative(f = expr, var = var, order = v - nu[,prev], deparse = FALSE)
      c$coef <- evaluate(c$expr[[1]], envir = x0)/prod(factorial(v))
    
    }
    else {
      
      c$coef <- DD.n(f = f, x0 = unlist(x0), order = v, accuracy = accuracy, stepsize = stepsize)
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
