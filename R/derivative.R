#' Numerical and Symbolic Derivatives
#' 
#' Computes symbolic derivatives based on the \code{\link[stats]{D}} function, or accurate and reliable numerical derivatives based on finite differences.
#' 
#' @param f function, expression or character array.
#' @param var character vector, giving the variable names with respect to which derivatives will be computed. If a named vector is provided, derivatives will be computed at that point. See examples.
#' @param order integer vector, giving the differentiation order for each variable. See details.
#' @param accuracy accuracy degree for numerical derivatives.
#' @param stepsize finite differences stepsize for numerical derivatives. Auto-optimized by default.
#' @param deparse logical. Return character instead of expression or call?
#' 
#' @details 
#' The function behaves differently depending on the length of the \code{order} argument.
#' 
#' If \code{order} is of length 1, then the n-th order derivative is computed for each function with respect to each 
#' variable.
#' \deqn{D = \partial^{(n)} \otimes F \rightarrow D_{i,.,j,k,.,l} = \partial^{(n)}_{k,.,l} F_{i,.,j}}
#' where \eqn{F} is the tensor of functions and \eqn{\partial} is the tensor of variable names with respect to which 
#' the \eqn{n}-th order derivatives will be computed.
#' 
#' If \code{order} matches the length of \code{var}, then it is assumed that the differentiation order is provided
#' for each variable. In this case, each function will be derived \eqn{n_i} times with respect to the \eqn{i}-th variable, 
#' for each of the \eqn{j} variables.
#' \deqn{D = \partial^{(n_1)}_1\partial^{(...)}_{...}\partial^{(n_i)}_i\partial^{(...)}_{...}\partial^{(n_j)}_j F}
#' where \eqn{F} is the tensor of functions to differentiate. 
#' 
#' If \code{var} is a named vector, e.g. \code{c(x = 0, y = 0)}, derivatives will be computed at that point. 
#' Note that if \code{f} is a function, then \code{var} must be a named vector giving the point at which the numerical derivatives will be computed.
#' 
#' @return array of derivatives.
#' 
#' @examples 
#' # derive f with respect to x
#' derivative(f = "sin(x)", var = "x")
#' 
#' # derive f with respect to x and evaluate in x = 0
#' derivative(f = "sin(x)", var = c("x" = 0))
#' 
#' # derive f twice with respect to x
#' derivative(f = "sin(x)", var = "x", order = 2)
#' 
#' # derive f once with respect to x, and twice with respect to y
#' derivative(f = "y^2*sin(x)", var = c("x","y"), order = c(1,2))
#' 
#' # compute the gradient of f with respect to (x,y)
#' derivative(f = "y*sin(x)", var = c("x","y"))
#' 
#' # compute the Jacobian of f with respect to (x,y)
#' f <- c("y*sin(x)", "x*cos(y)")
#' derivative(f = f, var = c("x","y"))
#' 
#' # compute the Hessian of f with respect to (x,y)
#' g <- derivative(f = "y^2*sin(x)", var = c("x","y"))
#' derivative(f = g, var = c("x","y"))
#' 
#' # compute the Jacobian of f with respect to (x,y) and evaluate in (0,0)
#' f1 <- function(x, y) y*sin(x)
#' f2 <- function(x, y) x*cos(y)
#' derivative(f = c(f1, f2), var = c("x"=0,"y"=0))
#' 
#' @export
#' 
derivative <- function(f, var = 'x', order = 1, accuracy = 2, stepsize = NULL, deparse = TRUE){
  
  n.v <- length(var)
  n.o <- length(order)
  
  is.fun <- is.fun(f)
  
  deparse <- deparse & !is.fun
  
  if(n.o!=n.v && n.o!=1 && !is.fun)
    stop('var and order must be the same length')
  
  if(is.character(f)) 
    f <- c2e(f)
  
  if(is.fun && !is.array(f))
    f <- array(c(f))
  
  x0 <- names(var)
  if(!is.null(x0)){
    x0  <- var
    var <- names(var)
  }
  
  if(is.fun && is.null(x0))
    stop("don't know where to compute the numeric derivative")
  
  if(n.o>1) {
    
    f <- outer(f, "1", FUN = function(x,y){
      
      if(is.fun){
          
        z <- sapply(x, FUN = function(f) {
          return(DD.n(f = f, x0 = x0, order = order, accuracy = accuracy, stepsize = stepsize))
        })
        
      }
      else {
       
        z <- sapply(x, FUN = function(f) {
          for(i in 1:n.v) 
            f <- DD(f = f, var = var[i], order = order[i])
          return(f)
        })
        
        if(deparse)
          z <- e2c(z)
         
      }
      
      return(z)
      
    })
    
  } else {
    
    if(is.fun){
      
      f <- outer(f, 1:n.v, FUN = function(x,y){
        
        z <- sapply(1:length(y), FUN = function(i) {
          o <- rep(0, n.v)
          o[y[i]] <- order
          return(DD.n(f = x[[i]], x0 = x0, order = o, accuracy = accuracy, stepsize = stepsize))
        })
        
        return(z)
        
      })
      
    }
    else {
      
      f <- outer(f, var, FUN = function(x,y){
        
        z <- sapply(1:length(y), FUN = function(i) {
          return(DD(f = x[[i]], var = y[i], order = order))
        })
        
        if(deparse)
          z <- e2c(z)
        
        return(z)
        
      })
      
    }
    
  }
  
  if(!is.fun & !is.null(x0))
    f <- evaluate(f, envir = as.list(x0))
  
  return(f)
}



DD <- function(f, var, order = 1) {
  
  if(order == 0) 
    return(f)
  
  else if(order == 1) 
    return(stats::D(f, var))
  
  else 
    return(DD(stats::D(f, var), var, order - 1))
  
}



DD.n <- function(f, x0, order, accuracy, stepsize = NULL){

  p <- accuracy
  if(all(p==0))
    return(NA)
  
  d <- order
  n <- length(x0)
  
  if(length(d)!=n)
    stop("order and x0 must be the same length")
  
  if(is.null(names(x0)))
    stop("don't know where to compute the numeric derivative")
  
  p <- rep(p, length.out = n)

  for(i in 1:n) 
    if((d[i]+p[i]) %% 2 == 0)
      p[i] <- p[i]+1
  
  C <- function(i, d){
    
    n.i <- length(i)
    
    k <- rep(0, n.i)
    k[d+1] <- 1
    
    return(solve(t(i^(matrix(data = rep(0:n.i, each = n.i), nrow = n.i, ncol = n.i)))) %*% k)
    
  }
  
  h <- stepsize
  if(is.null(h))
    h <- .Machine$double.eps^(0.25/sum(d))
  
  i <- lapply(1:n, function(i) (-(d[i]+p[i]-1)/2):((d[i]+p[i]-1)/2))
  
  F.i <- apply(expand.grid(i), 1, function(i){
    args <- x0 + i*h
    do.call(f, as.list(args))
  })
  
  F.i <- array(F.i, dim = sapply(i, length))
  index(F.i) <- 1:n
  
  for(k in 1:n){
    
    tmp <- array(C(i[[k]], d[k]))
    index(tmp) <- k
    
    assign(x = paste0("C",k), value = tmp)
    
  }
  
  expr <- sprintf('einstein(F.i,%s)', paste0("C",1:n, collapse = ',')) 
    
  return(prod(factorial(d)/h^d) * eval(parse(text = expr)))
    
}











