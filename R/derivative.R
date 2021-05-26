#' Numerical and Symbolic Derivatives
#' 
#' Computes symbolic derivatives based on the \code{\link[stats]{D}} function, or numerical derivatives based on finite differences.
#' 
#' @param f array of \code{characters} or a \code{function} returning a \code{numeric} array.
#' @param var vector giving the variable names with respect to which the derivatives are to be computed and/or the point where the derivatives are to be evaluated. See details.
#' @param params \code{list} of additional parameters passed to \code{f}.
#' @param order integer vector, giving the differentiation order for each variable. See details.
#' @param accuracy degree of accuracy for numerical derivatives.
#' @param stepsize finite differences stepsize for numerical derivatives. It is based on the precision of the machine by default.
#' @param deparse if \code{TRUE}, return \code{character} instead of \code{expression}.
#' @param drop if \code{TRUE}, return the array of derivatives without adding a dummy dimension when \code{order} is of length 1.
#' 
#' @details The function behaves differently depending on the arguents \code{order}, 
#' the order of differentiation, and \code{var}, the variable names with respect to 
#' which the derivatives are computed.
#' 
#' When multiple variables are provided and \code{order} is a single integer \eqn{n}, 
#' then the \eqn{n}-th order derivative is computed for each element of \code{f} 
#' with respect to each variable:
#' 
#' \deqn{D = \partial^{(n)} \otimes F}
#' 
#' that is:
#' 
#' \deqn{D_{i,\dots,j,k} = \partial^{(n)}_{k} F_{i,\dots,j}}
#' 
#' where \eqn{F} is the array of functions and \eqn{\partial_k^{(n)}} denotes the 
#' \eqn{n}-th order partial derivative with respect to the \eqn{k}-th variable.
#'     
#' When \code{order} matches the length of \code{var}, it is assumed that the 
#' differentiation order is provided for each variable. In this case, each element 
#' is derived \eqn{n_k} times with respect to the \eqn{k}-th variable, for each 
#' of the \eqn{m} variables.
#'     
#' \deqn{D_{i,\dots,j} = \partial^{(n_1)}_1\cdots\partial^{(n_m)}_m F_{i,\dots,j}}
#'     
#' The same applies when \code{order} is a named vector giving the differentiation 
#' order for each variable. For example, \code{order = c(x=1, y=2)} differentiates 
#' once with respect to \eqn{x} and twice with respect to \eqn{y}. A call with 
#' \code{order = c(x=1, y=0)} is equivalent to \code{order = c(x=1)}. 
#'     
#' To compute numerical derivatives or to evaluate symbolic derivatives at a point, 
#' the function accepts a named vector for the argument \code{var}; e.g. 
#' \code{var = c(x=1, y=2)} evaluates the derivatives in \eqn{x=1} and \eqn{y=2}. 
#' For \code{functions} where the first argument is used as a parameter vector, 
#' \code{var} should be a \code{numeric} vector indicating the point at which the 
#' derivatives are to be calculated.
#' 
#' @return \code{array}.
#' 
#' @examples 
#' ### symbolic derivatives
#' derivative(f = "sin(x)", var = "x")
#' 
#' ### numerical derivatives
#' f <- function(x) sin(x)
#' derivative(f = f, var = c(x=0))
#'
#' ### higher order derivatives
#' f <- function(x) sin(x)
#' derivative(f = f, var = c(x=0), order = 3)
#' 
#' ### multivariate functions 
#' ##  - derive once with respect to x
#' ##  - derive twice with respect to y
#' ##  - evaluate in x=0 and y=0
#' f <- function(x, y) y^2*sin(x)
#' derivative(f = f, var = c(x=0, y=0), order = c(1,2))
#' 
#' ### vector-valued functions
#' ##  - derive each element twice with respect to each variable
#' ##  - evaluate in x=0 and y=0
#' f <- function(x, y) c(x^2, y^2)
#' derivative(f, var = c(x=0, y=0), order = 2)
#' 
#' ### vectorized interface
#' f <- function(x) c(sum(x), prod(x))
#' derivative(f, var = c(0,0,0), order = 1)
#' 
#' @family derivatives
#' @family differential operators
#' 
#' @references 
#' Guidotti, E. (2020). "calculus: High dimensional numerical and symbolic calculus in R". \url{https://arxiv.org/abs/2101.00086}
#' 
#' @export
#' 
derivative <- function(f, var, params = list(), order = 1, accuracy = 4, stepsize = NULL, drop = TRUE, deparse = TRUE){
  
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
      
      f <- D.num(f = f, x0 = x0, order = order, accuracy = accuracy, stepsize = stepsize, params = params, drop = drop)
      
    } else{
      
      for(i in 1:n.v)
        f[] <- sapply(f, FUN = function(f) D.sym(f = f, var = var[i], order = order[i]))
      
      if(deparse)
        f <- e2c(f)
        
    }
    
  } else if(n.o==1) {
    
    if(is.fun){
      
      f <- D.num(f = f, x0 = x0, order = order, accuracy = accuracy, stepsize = stepsize, params = params, drop = drop)
      
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
    f <- evaluate(f, x0, params)
  
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
D.num <- function(f, x0, order, accuracy, stepsize, params, drop, zero = NULL, cross = FALSE){

  f.dim <- f.eval(f, x0, params, dim = TRUE)

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

  if(is.null(stepsize)){
    stepsize <- .Machine$double.eps^(1/3)
    h <- stepsize^(1/sum(d))
    h <- abs(h*x0) + h * (abs(x0) < .Machine$double.eps^(2/3))
  }
  else{
    h <- rep(stepsize, length.out = n.x)
  }
  
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
          F.i[,idx,k] <- do.call(f, c(list(x), params))/prod(h[w.ij]^d) 
        else 
          F.i[,idx,k] <- do.call(f, c(as.list(x), params))/prod(h[w.ij]^d) 
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
        F.i[,idx] <- do.call(f, c(list(x), params))
      else 
        F.i[,idx] <- do.call(f, c(as.list(x), params))
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
        F.i[,idx,w.i] <- do.call(f, c(list(x), params))
      else 
        F.i[,idx,w.i] <- do.call(f, c(as.list(x), params))
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
    m <- t(i^(matrix(data = rep(0:(n.i-1), each = n.i), nrow = n.i, ncol = n.i)))
    return(solve(m, k))
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
  
  if(!is.null(zero))
    x[which(abs(x)<=zero)] <- 0
    
  return(x)
    
}
