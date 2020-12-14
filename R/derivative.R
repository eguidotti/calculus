#' Numerical and Symbolic Derivatives
#' 
#' Computes symbolic derivatives based on the \code{\link[stats]{D}} function, or accurate and reliable numerical derivatives based on finite differences.
#' 
#' @param f \code{function} returning a vector, matrix, or array; \code{array} of \code{character}; or \code{array} of \code{expression}.
#' @param var character vector, giving the variable names with respect to which the derivatives are computed. If a named vector is provided, the derivatives will be computed at that point. See examples.
#' @param order integer vector, giving the differentiation order for each variable. See details.
#' @param accuracy degree of accuracy for numerical derivatives.
#' @param stepsize finite differences stepsize for numerical derivatives. Auto-optimized by default.
#' @param deparse logical. Return \code{character} instead of \code{expression}? Default \code{TRUE}.
#' @param drop drop dimensions when...
#' @param ... additinal arguments passed to \code{f}, when \code{f} is a \code{function}.
#' 
#' @details 
#' The function behaves differently depending on the arguents \code{order} and \code{var}.
#' 
#' If \code{order} is an integer and more than one \code{var} are provided, then the n-th order derivative is computed for each element with respect to each 
#' variable.
#' \deqn{D = \partial^{(n)} \otimes F \rightarrow D_{i,.,j,k,.,l} = \partial^{(n)}_{k,.,l} F_{i,.,j}}
#' where \eqn{F} is a tensor of functions and \eqn{\partial} is the tensor of variable names with respect to which 
#' the \eqn{n}-th order derivatives are computed.
#' 
#' If \code{order} is a named vector, or it matches the length of \code{var}, then it is assumed that the differentiation order is provided
#' for each variable. In this case, each element is derived \eqn{n_i} times with respect to the \eqn{i}-th variable, 
#' for each of the \eqn{j} variables.
#' \deqn{D = \partial^{(n_1)}_1\partial^{(...)}_{...}\partial^{(n_i)}_i\partial^{(...)}_{...}\partial^{(n_j)}_j F}
#' where \eqn{F} is the tensor of functions to differentiate. 
#' 
#' If \code{var} is a named vector, e.g. \code{c(x = 0, y = 0)}, derivatives will be computed at that point. 
#' Note that if \code{f} is a function, then \code{var} must be a named vector giving the point in which the numerical derivatives have to be computed.
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
#' # derive a generica function f with respect to x, in x = 0
#' f <- function(x) sin(x)
#' derivative(f = f, var = c(x = 0))
#' 
#' # derive f twice with respect to x
#' derivative(f = "sin(x)", var = "x", order = 2)
#' 
#' # derive f once with respect to x, and twice with respect to y
#' derivative(f = "y^2*sin(x)", var = c("x","y"), order = c(1, 2))
#' # or equivalently
#' derivative(f = "y^2*sin(x)", var = c("x","y"), order = c(x = 1, y = 2))
#' 
#' # derive f twice with respect to y only
#' derivative(f = "y^2*sin(x)", var = "y", order = 2)
#' # or equivalently
#' derivative(f = "y^2*sin(x)", var = c("x","y"), order = c(0, 2))
#' # or equivalently
#' derivative(f = "y^2*sin(x)", var = c("x","y"), order = c(y = 2))
#' 
#' # compute the gradient of f with respect to (x,y)
#' derivative(f = "y*sin(x)", var = c("x","y"))
#' 
#' # compute the Jacobian of f with respect to (x,y)
#' f <- c("y*sin(x)", "x*cos(y)")
#' derivative(f = f, var = c("x","y"))
#' 
#' # compute the Jacobian of a generic function f with respect to (x,y) in (0,0) 
#' f <- function(x, y) c(y*sin(x), x*cos(y))
#' derivative(f = f, var = c(x = 0, y = 0))
#' 
#' @export
#' 
derivative <- function(f, var = 'x', order = 1, accuracy = 2, stepsize = NULL, drop = TRUE, deparse = TRUE, ...){
  
  is.fun <- is.function(f)
  deparse <- deparse & !is.fun
  
  if(is.character(f)) 
    f <- c2e(f)
  
  x0 <- names(var)
  if(!is.null(x0)){
    x0  <- var
    var <- names(var)
  } 
  else if(is.numeric(var)){
    if(is.fun)
      x0 <- var
    else
      stop("the variable(s) must be a character string or a named vector")
  }
    
  if(!is.null(names(order))){
    order <- sapply(var, function(v){
      o <- order[v]
      if(is.na(o)) o <- 0
      return(o)
    })
  }
  
  n.v <- length(var)
  n.o <- length(order)
  
  if(n.o==n.v & (n.o>1 | drop)) {
    
    if(is.fun){
      
      f <- D.num(f = f, x0 = x0, order = order, accuracy = accuracy, stepsize = stepsize, drop = drop, ...)
      
    } else{
      
      for(i in 1:n.v)
        f[] <- sapply(f, FUN = function(f) D.sym(f = f, var = var[i], order = order[i]))
      
      if(deparse)
        f <- e2c(f)
        
    }
    
  } else if(n.o==1) {
    
    if(is.fun){
      
      f <- D.num(f = f, x0 = x0, order = order, accuracy = accuracy, stepsize = stepsize, drop = drop, ...)
      
    } else {
      
      f <- outer(f, var, FUN = function(x,y){
        sapply(1:length(y), FUN = function(i) {
          D.sym(f = x[[i]], var = y[i], order = order)
        })
      })
      
      if(deparse)
        f <- e2c(f)
      
    }
    
  } else {
    
    stop("the order of the derivative is ambiguous. See the help ?derivative for details")
    
  }
  
  if(!is.fun & !is.null(x0))
    f <- evaluate(f, envir = as.list(x0))
  
  return(as.array(f))
  
}

# Symbolic derivatives
D.sym <- function(f, var, order = 1) {
  
  if(order == 0) 
    return(f)
  
  else if(order == 1) 
    return(stats::D(f, var))
  
  else 
    return(D.sym(stats::D(f, var), var, order - 1))
  
}

# Numerical derivatives
D.num <- function(f, x0, order, accuracy, stepsize, drop, cross = FALSE, ...){

  f.dim <- f.dim(f, x0, ...)

  if(cross)
    order <- rep.int(order, 2)
  
  d <- order
  p <- accuracy
  
  w <- which(d>0)
  if(length(w)==0)
    w <- 1
  
  n <- length(w)
  n.o <- length(order)
  n.x <- length(x0)
  
  if(is.null(stepsize))
    stepsize <- .Machine$double.eps^(1/3)
  h <- stepsize^(1/sum(d))
  h <- abs(h*x0) + h * (abs(x0) < .Machine$double.eps^(2/3))

  i <- lapply(w, function(i) {i.max <- as.integer((d[i]+p-1)/2); -i.max:i.max})
  i.dim <- sapply(i, length)
  i.grid <- as.matrix(expand.grid(i))
  
  is.named <- !is.null(names(x0))
  
  if(cross){
    
    F.i <- array(dim = c(prod(f.dim), prod(i.dim), n.x*(n.x-1)/2))
    
    k <- 0
    for(w.j in 1:(n.x-1)) for(w.i in (w.j+1):n.x) {
      k <- k+1
      w.ij <- c(w.i,w.j)
      for(idx in 1:nrow(i.grid)){
        x <- x0
        x[w.ij] <- x[w.ij] + i.grid[idx,] * h[w.ij]
        if(!is.named)
          F.i[,idx,k] <- f(x, ...)/prod(h[w.ij]^d)
        else 
          F.i[,idx,k] <- do.call(f, c(as.list(x), list(...)))/prod(h[w.ij]^d) 
      }
    }
    
    dim(F.i) <- c(f.dim, i.dim, n.x*(n.x-1)/2)
    index(F.i) <- c(rep.int(NA, length(f.dim)), w, NA)
    
  }
  else if(n.o==n.x & (n.o>1 | drop)){
    
    F.i <- array(dim = c(prod(f.dim), prod(i.dim)))
    
    for(idx in 1:nrow(i.grid)){
      x <- x0
      x[w] <- x[w] + i.grid[idx,] * h[w]
      if(!is.named)
        F.i[,idx] <- f(x, ...)
      else 
        F.i[,idx] <- do.call(f, c(as.list(x), list(...)))
    }

    dim(F.i) <- c(f.dim, i.dim)
    index(F.i) <- c(rep.int(NA, length(f.dim)), w)
    
  }
  else if(n.o==1){
    
    F.i <- array(dim = c(prod(f.dim), prod(i.dim), n.x))
    
    for(w.i in 1:n.x) for(idx in 1:nrow(i.grid)){
      x <- x0
      x[w.i] <- x[w.i] + i.grid[idx,] * h[w.i]
      if(!is.named)
        F.i[,idx,w.i] <- f(x, ...)
      else 
        F.i[,idx,w.i] <- do.call(f, c(as.list(x), list(...)))
    }
    
    dim(F.i) <- c(f.dim, i.dim, n.x)
    index(F.i) <- c(rep.int(NA, length(f.dim)), w, NA)
    
  }
  else {
    
    stop("the order of the derivative is ambiguous.")
    
  }
  
  C.i <- function(i, d){
    n.i <- length(i)
    k <- rep.int(0, n.i)
    k[d+1] <- 1
    m <- t(i^(matrix(data = rep(0:n.i, each = n.i), nrow = n.i, ncol = n.i)))
    return(solve(m) %*% k)
  }
  
  for(k in 1:n){
    w.k <- w[k]
    tmp <- array(C.i(i[[k]], d[w.k]))
    index(tmp) <- w.k
    assign(x = paste0("C.", w.k), value = tmp)
  }
  
  if(cross)
    s <- prod(factorial(d))
  else if(n.o==1)
    s <- rep(factorial(d)/h^d, each = prod(f.dim))
  else 
    s <- prod(factorial(d)/h^d)
    
  x <- s * eval(parse(text = sprintf('einstein(F.i,%s)', paste0("C.", w, collapse = ','))))
  index(x) <- NULL
  
  return(x)
    
}
