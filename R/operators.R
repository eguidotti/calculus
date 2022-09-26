#' Numerical and Symbolic Sum
#' 
#' Elementwise sum of \code{numeric} or \code{character} arrays.
#' 
#' @param x \code{numeric} or \code{character} array.
#' @param y \code{numeric} or \code{character} array.
#' 
#' @return \code{array}.
#' 
#' @examples 
#' ### vector 
#' x <- c("a+1","b+2")
#' x %sum% x
#' 
#' ### matrix 
#' x <- matrix(letters[1:4], ncol = 2)
#' x %sum% x
#' 
#' ### array
#' x <- array(letters[1:12], dim = c(2,2,3))
#' y <- array(1:12, dim = c(2,2,3))
#' x %sum% y
#' 
#' @family basic arithmetic
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
"%sum%" <- function(x,y) {
  
  x <- as.array(x)
  y <- as.array(y)
  
  if(length(x)==1) 
    x <- array(x, dim = dim(y))
  else if(length(y)==1) 
    y <- array(y, dim = dim(x))
  
  if(any(dim(x)!=dim(y)))
    stop('non-conformable arrays')
  
  if(is.numeric(x) && is.numeric(y))
    return(x+y)
  
  return(array(cpp_paste(as.character(x), as.character(y), sep = " + "), dim = dim(x)))
  
}

#' Numerical and Symbolic Difference
#' 
#' Elementwise difference of \code{numeric} or \code{character} arrays.
#' 
#' @param x \code{numeric} or \code{character} array.
#' @param y \code{numeric} or \code{character} array.
#' 
#' @return \code{array}.
#' 
#' @examples 
#' ### vector 
#' x <- c("a+1","b+2")
#' x %diff% x
#' 
#' ### matrix 
#' x <- matrix(letters[1:4], ncol = 2)
#' x %diff% x
#' 
#' ### array
#' x <- array(letters[1:12], dim = c(2,2,3))
#' y <- array(1:12, dim = c(2,2,3))
#' x %diff% y
#' 
#' @family basic arithmetic
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
"%diff%" <- function(x,y) {
  
  x <- as.array(x)
  y <- as.array(y)
  
  if(length(x)==1) 
    x <- array(x, dim = dim(y))
  else if(length(y)==1) 
    y <- array(y, dim = dim(x))
  
  if(any(dim(x)!=dim(y)))
    stop('non-conformable arrays')
  
  x_num <- is.numeric(x)
  y_num <- is.numeric(y)
  if(x_num && y_num)
    return(x-y)
  
  if(getOption('calculus.auto.wrap', default = TRUE)){
    if(!x_num) x <- wrap(x)
    if(!y_num) y <- wrap(y)
  }
  
  return(array(cpp_paste(as.character(x), as.character(y), sep = " - "), dim = dim(x)))
  
}

#' Numerical and Symbolic Product
#' 
#' Elementwise product of \code{numeric} or \code{character} arrays.
#' 
#' @param x \code{numeric} or \code{character} array.
#' @param y \code{numeric} or \code{character} array.
#' 
#' @return \code{array}.
#' 
#' @examples 
#' ### vector 
#' x <- c("a+1","b+2")
#' x %prod% x
#' 
#' ### matrix 
#' x <- matrix(letters[1:4], ncol = 2)
#' x %prod% x
#' 
#' ### array
#' x <- array(letters[1:12], dim = c(2,2,3))
#' y <- array(1:12, dim = c(2,2,3))
#' x %prod% y
#' 
#' @family basic arithmetic
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
"%prod%" <- function(x,y){
  
  x <- as.array(x)
  y <- as.array(y)
  
  if(length(x)==1) 
    x <- array(x, dim = dim(y))
  else if(length(y)==1) 
    y <- array(y, dim = dim(x))
  
  if(any(dim(x)!=dim(y)))
    stop('non-conformable arrays')
  
  x_num <- is.numeric(x)
  y_num <- is.numeric(y)
  if(x_num && y_num)
    return(x*y)
    
  if(getOption('calculus.auto.wrap', default = TRUE)){
    if(!x_num) x <- wrap(x)
    if(!y_num) y <- wrap(y)
  }
  
  return(array(cpp_paste(as.character(x), as.character(y), sep = " * "), dim = dim(x)))
    
}

#' Numerical and Symbolic Division
#' 
#' Elementwise division of \code{numeric} or \code{character} arrays.
#' 
#' @param x \code{numeric} or \code{character} array.
#' @param y \code{numeric} or \code{character} array.
#' 
#' @return \code{array}.
#' 
#' @examples 
#' ### vector 
#' x <- c("a+1","b+2")
#' x %div% x
#' 
#' ### matrix 
#' x <- matrix(letters[1:4], ncol = 2)
#' x %div% x
#' 
#' ### array
#' x <- array(letters[1:12], dim = c(2,2,3))
#' y <- array(1:12, dim = c(2,2,3))
#' x %div% y 
#' 
#' @family basic arithmetic
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
"%div%" <- function(x,y){
  
  x <- as.array(x)
  y <- as.array(y)
  
  if(length(x)==1) 
    x <- array(x, dim = dim(y))
  else if(length(y)==1) 
    y <- array(y, dim = dim(x))
  
  if(any(dim(x)!=dim(y)))
    stop('non-conformable arrays')
  
  x_num <- is.numeric(x)
  y_num <- is.numeric(y)
  if(x_num && y_num)
    return(x/y)
  
  if(getOption('calculus.auto.wrap', default = TRUE)){
    if(!x_num) x <- wrap(x)
    if(!y_num) y <- wrap(y)
  }
  
  return(array(cpp_paste(as.character(x), as.character(y), sep = " / "), dim = dim(x)))
  
}

#' Numerical and Symbolic Matrix Product
#' 
#' Multiplies two \code{numeric} or \code{character} matrices, if they are conformable. If one argument is a vector, it will be promoted to either a row or column matrix to make the two arguments conformable. If both are vectors of the same length, it will return the inner product (as a \code{matrix}).
#' 
#' @param x \code{numeric} or \code{character} matrix.
#' @param y \code{numeric} or \code{character} matrix.
#' 
#' @return \code{matrix}.
#' 
#' @examples 
#' ### numeric inner product 
#' x <- 1:4
#' mx(x, x)
#' 
#' ### symbolic inner product 
#' x <- letters[1:4]
#' mx(x, x)
#' 
#' ### numeric matrix product
#' x <- letters[1:4]
#' y <- diag(4)
#' mx(x, y)
#' 
#' ### symbolic matrix product
#' x <- array(1:12, dim = c(3,4))
#' y <- letters[1:4]
#' mx(x, y)
#' 
#' @family matrix algebra
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
mx <- function(x, y){
  
  if(is.numeric(x) && is.numeric(y))
    return(x %*% y)
  
  is.x <- is.matrix(x)
  is.y <- is.matrix(y)
  
  if(!is.x && !is.y){
    
    x <- t(x)
    y <- as.matrix(y)
    
  }
  else if(!is.x){
    
    if(dim(y)[1]==length(x))
      x <- t(x)
    else
      x <- as.matrix(x)
    
  }
  else if(!is.y){

    if(dim(x)[2]==length(y))
      y <- as.matrix(y)
    else
      y <- t(y)
    
  }

  index(x) <- c('i','j')
  index(y) <- c('j','k')
  
  z <- einstein(x, y)
  index(z) <- NULL
  
  return(z)
  
}

#' @describeIn mx binary operator.
#' 
#' @examples 
#' ### binary operator
#' x <- array(1:12, dim = c(3,4))
#' y <- letters[1:4]
#' x %mx% y
#' 
#' @export
#' 
"%mx%" <- function(x, y){
  
  mx(x, y)
  
}

#' Numerical and Symbolic Dot Product
#' 
#' The dot product between arrays with different dimensions is computed by taking the inner product on the last dimensions of the two arrays.
#' 
#' @details The dot product between two arrays \code{A} and \code{B} is computed as:
#' \deqn{C_{i_1\dots i_m} = \sum_{j_1\dots j_n} A_{i_1\dots i_mj_1\dots j_n}B_{j_1\dots j_n}}
#' 
#' @param x \code{numeric} or \code{character} array.
#' @param y \code{numeric} or \code{character} array.
#' 
#' @return \code{array}.
#' 
#' @examples 
#' ### inner product 
#' x <- array(1:12, dim = c(3,4))
#' x %dot% x
#' 
#' ### dot product 
#' x <- array(1:24, dim = c(3,2,4))
#' y <- array(letters[1:8], dim = c(2,4))
#' x %dot% y
#' 
#' @family basic arithmetic
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
"%dot%" <- function(x, y) {
  
  x <- as.array(x)
  y <- as.array(y)
  
  x.n.dim <- length(dim(x))
  y.n.dim <- length(dim(y))
  
  j.n <- min(x.n.dim, y.n.dim)
  j <- letters[1:j.n]
  
  index(x) <- c(seq(length.out = x.n.dim-j.n), j)
  index(y) <- c(seq(length.out = y.n.dim-j.n), j)
  
  return(einstein(x, y))
  
}

#' Numerical and Symbolic Inner Product
#' 
#' Computes the inner product of two \code{numeric} or \code{character} arrays.
#' 
#' @details The inner product between two arrays \code{A} and \code{B} is computed as:
#' \deqn{C = \sum_{j_1\dots j_n} A_{j_1\dots j_n}B_{j_1\dots j_n}}
#' 
#' @param x \code{numeric} or \code{character} array.
#' @param y \code{numeric} or \code{character} array.
#' 
#' @return \code{numeric} or \code{character}.
#' 
#' @examples 
#' ### numeric inner product 
#' x <- array(1:4, dim = c(2,2))
#' x %inner% x
#' 
#' ### symbolic inner product 
#' x <- array(letters[1:4], dim = c(2,2))
#' x %inner% x
#' 
#' @family basic arithmetic
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
"%inner%" <- function(x,y){
  
  x <- as.array(x)
  y <- as.array(y)
  
  if(length(x)==1) 
    x <- array(x, dim = dim(y))
  else if(length(y)==1) 
    y <- array(y, dim = dim(x))
  
  if(any(dim(x)!=dim(y)))
    stop('non-conformable arrays')
  
  x_num <- is.numeric(x)
  y_num <- is.numeric(y)
  if(x_num && y_num)
    return(sum(x*y))
  
  if(getOption('calculus.auto.wrap', default = TRUE)){
    if(!x_num) x <- wrap(x)
    if(!y_num) y <- wrap(y)
  }
  
  return(cpp_inner(as.character(x), as.character(y)))
  
}

#' Numerical and Symbolic Outer Product
#' 
#' Computes the outer product of two \code{numeric} or \code{character} arrays.
#' 
#' @details The outer product between two arrays \code{A} and \code{B} is computed as:
#' \deqn{C_{i_1\dots i_mj_1\dots j_n} = A_{i_1\dots i_m}B_{j_1\dots j_n}}
#' 
#' @param x \code{numeric} or \code{character} array.
#' @param y \code{numeric} or \code{character} array.
#' 
#' @return \code{array}.
#' 
#' @examples 
#' ### numeric outer product 
#' c(1,2) %outer% c(2,3)
#' 
#' ### symbolic outer product 
#' c("a","b") %outer% c("c","d")
#' 
#' @family basic arithmetic
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
"%outer%" <- function(x,y){
  
  x <- as.array(x)
  y <- as.array(y)
  
  x_num <- is.numeric(x)
  y_num <- is.numeric(y)
  if(x_num && y_num)
    return(x %o% y)
  
  if(getOption('calculus.auto.wrap', default = TRUE)){
    if(!x_num) x <- wrap(x)
    if(!y_num) y <- wrap(y)
  }
    
  return(array(cpp_outer(as.character(x), as.character(y)), dim = c(dim(x), dim(y))))
  
}

#' Numerical and Symbolic Kronecker Product
#' 
#' Computes the generalised Kronecker product of two \code{numeric} or \code{character} arrays.
#' 
#' @param x \code{numeric} or \code{character} array.
#' @param y \code{numeric} or \code{character} array.
#' 
#' @return \code{array}.
#' 
#' @examples 
#' ### numeric Kronecker product 
#' c(1,2) %kronecker% c(2,3)
#' 
#' ### symbolic Kronecker product 
#' array(1:4, dim = c(2,2)) %kronecker% c("a","b")
#' 
#' @family basic arithmetic
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
"%kronecker%" <- function(x,y){
  
  x_num <- is.numeric(x)
  y_num <- is.numeric(y)
  if(x_num && y_num)
    return(x%x%y)
  
  if(getOption('calculus.auto.wrap', default = TRUE)){
    if(!x_num) x <- wrap(x)
    if(!y_num) y <- wrap(y)
  }
  
  if(x_num) x[] <- as.character(x)
  if(y_num) y[] <- as.character(y)
  
  return(base::kronecker(x, y, cpp_paste, make.dimnames = FALSE, sep = " * "))
  
}

#' Numerical and Symbolic Cross Product
#' 
#' Computes the cross product of \eqn{n-1} vectors of length \eqn{n}. 
#' 
#' @param ... \eqn{n-1} vectors of length \eqn{n}.
#' 
#' @return \eqn{n}-dimensional vector orthogonal to the \eqn{n-1} vectors.
#' 
#' @examples
#' ### canonical basis 4-d
#' cross(c(1,0,0,0), c(0,1,0,0), c(0,0,0,1))
#' 
#' ### canonical basis 3-d
#' cross(c(1,0,0), c(0,1,0))
#'
#' @family vector algebra
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
cross <- function(...){
  
  args <- list(...)  
  
  if (length(args) == 0) {
    stop("no data supplied")
  }
  len <- unique(sapply(args, FUN=length))
  if (length(len) > 1) {
    stop("all vectors must be the same length")
  }
  if (len != length(args) + 1) {
    stop("need N-1 vectors of length N")
  }
  
  m <- do.call(rbind, args)
  
  if(!is.numeric(m) && !getOption('calculus.auto.wrap', default = TRUE))
    return(sapply(seq(len), function(i) {
      wrap(mxdet(m[,-i,drop=FALSE])) %prod% (-1)^(i+1)
    }))
  
  return(sapply(seq(len), function(i) {
    mxdet(m[,-i,drop=FALSE]) %prod% (-1)^(i+1)
  }))
  
}

#' @describeIn cross binary operator for 3-dimensional cross products.
#' 
#' @param x \code{numeric} or \code{character} vector of length 3.
#' @param y \code{numeric} or \code{character} vector of length 3.
#' 
#' @examples 
#' ### symbolic cross product 3-d
#' c(1,0,0) %cross% c(0,1,0)
#' 
#' ### symbolic cross product 3-d
#' c("a","b","c") %cross% c(0,0,1)
#' 
#' @export
#'
"%cross%" <- function(x, y){
  
  cross(x, y)
  
}

#' Numerical and Symbolic Gradient
#' 
#' Computes the numerical gradient of \code{functions} or the symbolic gradient of \code{characters} 
#' in arbitrary \href{https://en.wikipedia.org/wiki/Orthogonal_coordinates#Table_of_orthogonal_coordinates}{orthogonal coordinate systems}.
#' 
#' @details The gradient of a scalar-valued function \eqn{F} is the vector 
#' \eqn{(\nabla F)_i} whose components are the partial derivatives of \eqn{F} 
#' with respect to each variable \eqn{i}. 
#' The \code{gradient} is computed in arbitrary orthogonal coordinate systems using the 
#' scale factors \eqn{h_i}:
#' 
#' \deqn{(\nabla F)_i = \frac{1}{h_i}\partial_iF}
#' 
#' When the function \eqn{F} is a tensor-valued function \eqn{F_{d_1,\dots,d_n}}, 
#' the \code{gradient} is computed for each scalar component. In particular, it becomes
#' the Jacobian matrix for vector-valued function.
#' 
#' \deqn{(\nabla F_{d_1,\dots,d_n})_i = \frac{1}{h_i}\partial_iF_{d_1,\dots,d_n}}
#' 
#' @param f array of \code{characters} or a \code{function} returning a \code{numeric} array.
#' @param var vector giving the variable names with respect to which the derivatives are to be computed and/or the point where the derivatives are to be evaluated. See \code{\link{derivative}}.
#' @param params \code{list} of additional parameters passed to \code{f}.
#' @param accuracy degree of accuracy for numerical derivatives.
#' @param stepsize finite differences stepsize for numerical derivatives. It is based on the precision of the machine by default.
#' @param coordinates coordinate system to use. One of: \code{cartesian}, \code{polar}, \code{spherical}, \code{cylindrical}, \code{parabolic}, \code{parabolic-cylindrical} or a vector of scale factors for each varibale.
#' @param drop if \code{TRUE}, return the gradient as a vector and not as an \code{array} for scalar-valued functions.
#' 
#' @return Gradient vector for scalar-valued functions when \code{drop=TRUE}, \code{array} otherwise.
#' 
#' @examples 
#' ### symbolic gradient 
#' gradient("x*y*z", var = c("x", "y", "z"))
#' 
#' ### numerical gradient in (x=1, y=2, z=3)
#' f <- function(x, y, z) x*y*z
#' gradient(f = f, var = c(x=1, y=2, z=3))
#' 
#' ### vectorized interface
#' f <- function(x) x[1]*x[2]*x[3]
#' gradient(f = f, var = c(1, 2, 3))
#' 
#' ### symbolic vector-valued functions
#' f <- c("y*sin(x)", "x*cos(y)")
#' gradient(f = f, var = c("x","y"))
#' 
#' ### numerical vector-valued functions
#' f <- function(x) c(sum(x), prod(x))
#' gradient(f = f, var = c(0,0,0))
#' 
#' @family differential operators
#'
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#'      
#' @export
#' 
gradient <- function(f, var, params = list(), coordinates = 'cartesian', accuracy = 4, stepsize = NULL, drop = TRUE){
  
  x <- names(var)
  if(is.null(x))
    x <- var
  
  f <- derivative(f = f, var = var, params = params, order = 1, accuracy = accuracy, stepsize = stepsize, drop = FALSE, deparse = TRUE)
  
  n <- length(var)
  m <- length(f)
  
  if(drop & m==n)
    dim(f) <- NULL
    
  h <- sf(var = x, coordinates = coordinates)
  
  if(is.null(h))
    return(f)
  
  if(is.numeric(f)){
    
    h <- evaluate(h, var)
    h <- rep(h, each = m/n)
    f <- f/h
    
  }
  else{
  
    h <- sprintf("1/%s", h)
    h <- rep(h, each = m/n)
    f[] <- cpp_paste(h, wrap(f), sep = " * ")
    
  }
  
  return(f)  

}

#' @describeIn gradient binary operator with default parameters.
#' 
#' @examples 
#' ### binary operator
#' "x*y^2" %gradient% c(x=1, y=3)
#' 
#' @export
#' 
"%gradient%" <- function(f, var){
  
  gradient(f = f, var = var)
  
}

#' Numerical and Symbolic Jacobian
#' 
#' Computes the numerical Jacobian of \code{functions} or the symbolic Jacobian of \code{characters}
#' in arbitrary \href{https://en.wikipedia.org/wiki/Orthogonal_coordinates#Table_of_orthogonal_coordinates}{orthogonal coordinate systems}.
#' 
#' @details The function is basically a wrapper for \code{\link{gradient}} with \code{drop=FALSE}.
#' 
#' @param f array of \code{characters} or a \code{function} returning a \code{numeric} array.
#' @param var vector giving the variable names with respect to which the derivatives are to be computed and/or the point where the derivatives are to be evaluated. See \code{\link{derivative}}.
#' @param params \code{list} of additional parameters passed to \code{f}.
#' @param accuracy degree of accuracy for numerical derivatives.
#' @param stepsize finite differences stepsize for numerical derivatives. It is based on the precision of the machine by default.
#' @param coordinates coordinate system to use. One of: \code{cartesian}, \code{polar}, \code{spherical}, \code{cylindrical}, \code{parabolic}, \code{parabolic-cylindrical} or a vector of scale factors for each varibale.
#' 
#' @return \code{array}.
#' 
#' @examples 
#' ### symbolic Jacobian 
#' jacobian("x*y*z", var = c("x", "y", "z"))
#' 
#' ### numerical Jacobian in (x=1, y=2, z=3)
#' f <- function(x, y, z) x*y*z
#' jacobian(f = f, var = c(x=1, y=2, z=3))
#' 
#' ### vectorized interface
#' f <- function(x) x[1]*x[2]*x[3]
#' jacobian(f = f, var = c(1, 2, 3))
#' 
#' ### symbolic vector-valued functions
#' f <- c("y*sin(x)", "x*cos(y)")
#' jacobian(f = f, var = c("x","y"))
#' 
#' ### numerical vector-valued functions
#' f <- function(x) c(sum(x), prod(x))
#' jacobian(f = f, var = c(0,0,0))
#' 
#' @family differential operators
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
jacobian <- function(f, var, params = list(), coordinates = 'cartesian', accuracy = 4, stepsize = NULL){
  
  gradient(f = f, var = var, params = params, coordinates = coordinates, accuracy = accuracy, stepsize = stepsize, drop = FALSE)
  
}
  
#' @describeIn jacobian binary operator with default parameters.
#' 
#' @examples 
#' ### binary operator
#' "x*y^2" %jacobian% c(x=1, y=3)
#' 
#' @export
#' 
"%jacobian%" <- function(f, var){
  
  jacobian(f = f, var = var)
  
}

#' Numerical and Symbolic Hessian
#' 
#' Computes the numerical Hessian of \code{functions} or the symbolic Hessian of \code{characters}.
#' 
#' @details In Cartesian coordinates, the Hessian of a scalar-valued function \eqn{F} is the 
#' square matrix of second-order partial derivatives:
#' 
#' \deqn{(H(F))_{ij} = \partial_{ij}F}
#' 
#' When the function \eqn{F} is a tensor-valued function \eqn{F_{d_1,\dots,d_n}}, 
#' the \code{hessian} is computed for each scalar component.
#' 
#' \deqn{(H(F))_{d_1\dots d_n,ij} = \partial_{ij}F_{d_1\dots d_n}}
#' 
#' It might be tempting to apply the definition of the Hessian as the Jacobian of the 
#' gradient to write it in arbitrary orthogonal coordinate systems. However, this results in a 
#' Hessian matrix that is not symmetric and ignores the distinction between vector 
#' and covectors in tensor analysis. The generalization to arbitrary coordinate system 
#' is not currently supported.
#' 
#' @param f array of \code{characters} or a \code{function} returning a \code{numeric} array.
#' @param var vector giving the variable names with respect to which the derivatives are to be computed and/or the point where the derivatives are to be evaluated. See \code{\link{derivative}}.
#' @param params \code{list} of additional parameters passed to \code{f}.
#' @param accuracy degree of accuracy for numerical derivatives.
#' @param stepsize finite differences stepsize for numerical derivatives. It is based on the precision of the machine by default.
#' @param drop if \code{TRUE}, return the Hessian as a matrix and not as an \code{array} for scalar-valued functions.
#' 
#' @return Hessian matrix for scalar-valued functions when \code{drop=TRUE}, \code{array} otherwise.
#' 
#' @examples 
#' ### symbolic Hessian 
#' hessian("x*y*z", var = c("x", "y", "z"))
#' 
#' ### numerical Hessian in (x=1, y=2, z=3)
#' f <- function(x, y, z) x*y*z
#' hessian(f = f, var = c(x=1, y=2, z=3))
#' 
#' ### vectorized interface
#' f <- function(x) x[1]*x[2]*x[3]
#' hessian(f = f, var = c(1, 2, 3))
#' 
#' ### symbolic vector-valued functions
#' f <- c("y*sin(x)", "x*cos(y)")
#' hessian(f = f, var = c("x","y"))
#' 
#' ### numerical vector-valued functions
#' f <- function(x) c(sum(x), prod(x))
#' hessian(f = f, var = c(0,0,0))
#' 
#' @family differential operators
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
hessian <- function(f, var, params = list(), accuracy = 4, stepsize = NULL, drop = TRUE){
  
  n <- length(var)
  x <- names(var)
  if(is.null(x))
    x <- var
  
  if(is.function(f)){
    
    f.dim <- f.eval(f, var, params, dim = TRUE)
    m <- prod(f.dim)
    
    ii <- D.num(f = f, x0 = var, params = params, order = 2, accuracy = accuracy, stepsize = stepsize, drop = FALSE)
    if(n>1)
      ij <- D.num(f = f, x0 = var, params = params, order = 1, accuracy = accuracy, stepsize = stepsize, cross = TRUE, drop = FALSE)
    
    lwr <- lower.tri(matrix(nrow = n, ncol = n))
    dia <- lower.tri(lwr, diag = TRUE) & !lwr
    lwr <- rep(lwr, each = m)
    dia <- rep(dia, each = m)
    
    f.dij <- array(0, dim = c(m, n, n))
    if(n>1){
      f.dij[lwr] <- ij
      for(d in 1:m)
        f.dij[d,,] <- f.dij[d,,] + t(f.dij[d,,])
    }
    f.dij[dia] <- ii
  
    H <- array(f.dij, dim = c(f.dim, n, n))
    
  } 
  else {

    g <- gradient(f = f, var = x, params = params, coordinates = "cartesian", drop = FALSE)
    H <- gradient(f = g, var = var, params = params, coordinates = "cartesian", drop = FALSE)
        
  }

  if(drop & length(H)==n^2)
    dim(H) <- c(n, n)

  return(H)
  
}

#' @describeIn hessian binary operator with default parameters.
#' 
#' @examples 
#' ### binary operator
#' "x*y^2" %hessian% c(x=1, y=3)
#' 
#' @export
#' 
"%hessian%" <- function(f, var){
  
  hessian(f = f, var = var)
  
}

#' Numerical and Symbolic Divergence
#' 
#' Computes the numerical divergence of \code{functions} or the symbolic divergence of \code{characters}
#' in arbitrary \href{https://en.wikipedia.org/wiki/Orthogonal_coordinates#Table_of_orthogonal_coordinates}{orthogonal coordinate systems}.
#' 
#' @details
#' The divergence of a vector-valued function \eqn{F_i} produces a scalar value 
#' \eqn{\nabla \cdot F} representing the volume density of the outward flux of the 
#' vector field from an infinitesimal volume around a given point. 
#' The \code{divergence} is computed in arbitrary orthogonal coordinate systems using the 
#' scale factors \eqn{h_i}:
#' 
#' \deqn{\nabla \cdot F = \frac{1}{J}\sum_i\partial_i\Biggl(\frac{J}{h_i}F_i\Biggl)}
#' 
#' where \eqn{J=\prod_ih_i}. When \eqn{F} is an \code{array} of vector-valued functions 
#' \eqn{F_{d_1\dots d_n,i}}, the \code{divergence} is computed for each vector:
#' 
#' \deqn{(\nabla \cdot F)_{d_1\dots d_n} = \frac{1}{J}\sum_i\partial_i\Biggl(\frac{J}{h_i}F_{d_1\dots d_n,i}\Biggl)}
#' 
#' @param f array of \code{characters} or a \code{function} returning a \code{numeric} array.
#' @param var vector giving the variable names with respect to which the derivatives are to be computed and/or the point where the derivatives are to be evaluated. See \code{\link{derivative}}.
#' @param params \code{list} of additional parameters passed to \code{f}.
#' @param accuracy degree of accuracy for numerical derivatives.
#' @param stepsize finite differences stepsize for numerical derivatives. It is based on the precision of the machine by default.
#' @param coordinates coordinate system to use. One of: \code{cartesian}, \code{polar}, \code{spherical}, \code{cylindrical}, \code{parabolic}, \code{parabolic-cylindrical} or a vector of scale factors for each varibale.
#' @param drop if \code{TRUE}, return the divergence as a scalar and not as an \code{array} for vector-valued functions.
#' 
#' @return Scalar for vector-valued functions when \code{drop=TRUE}, \code{array} otherwise. 
#' 
#' @examples 
#' ### symbolic divergence of a vector field
#' f <- c("x^2","y^3","z^4")
#' divergence(f, var = c("x","y","z"))
#' 
#' ### numerical divergence of a vector field in (x=1, y=1, z=1)
#' f <- function(x,y,z) c(x^2, y^3, z^4)
#' divergence(f, var = c(x=1, y=1, z=1))
#' 
#' ### vectorized interface
#' f <- function(x) c(x[1]^2, x[2]^3, x[3]^4)
#' divergence(f, var = c(1,1,1)) 
#' 
#' ### symbolic array of vector-valued 3-d functions
#' f <- array(c("x^2","x","y^2","y","z^2","z"), dim = c(2,3))
#' divergence(f, var = c("x","y","z"))
#' 
#' ### numeric array of vector-valued 3-d functions in (x=0, y=0, z=0)
#' f <- function(x,y,z) array(c(x^2,x,y^2,y,z^2,z), dim = c(2,3))
#' divergence(f, var = c(x=0, y=0, z=0))
#'  
#' @family differential operators
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
divergence <- function(f, var, params = list(), coordinates = 'cartesian', accuracy = 4, stepsize = NULL, drop = TRUE){
  
  calculus.auto.wrap <- options(calculus.auto.wrap = TRUE)
  on.exit(options(calculus.auto.wrap), add = TRUE)
  
  is.fun <- is.function(f)
  if(is.fun)
    f.di <- f.eval(f, var, params)
  else
    f.di <- as.array(f)
  
  n <- length(var)
  f.dim <- dim(f.di)
  f.n.dim <- length(f.dim)
  
  if(f.dim[f.n.dim]!=n)
    stop('f must be the same length of var on the last dimension')

  if(is.fun){
    qvar <- var
    q <- paste0("q",1:n)
    names(qvar) <- q
  }
  else{
    qvar <- var
    q <- names(var)
    if(is.null(q))
      q <- var
  } 
  
  h <- sf(var = q, coordinates = coordinates)
  
  if(is.null(h)){
    
    df.dij <- derivative(f, var = var, params = params, order = 1, accuracy = accuracy, stepsize = stepsize, drop = FALSE)
    index(df.dij)[f.n.dim+0:1] <- "i"
    
    D <- einstein(df.dij)
    
  }
  else {
    
    J <- paste0(h, collapse = "*")
    
    if(n==1)
      h <- "1"
    else
      h <- sapply(1:n, function(k) paste0(h[-k], collapse = "*"))
    
    if(is.fun){
      
      index(f.di)[f.n.dim] <- "i"
      
      h.i <- evaluate(h, qvar)
      index(h.i) <- "i"
      
      dh.i <- derivative(h, var = qvar, order = 1, accuracy = accuracy, stepsize = stepsize, drop = FALSE)
      index(dh.i) <- c("i","i")
      
      df.dij <- derivative(f, var = var, params = params, order = 1, accuracy = accuracy, stepsize = stepsize, drop = FALSE)
      index(df.dij)[f.n.dim+0:1] <- "i"
      
      D <- (einstein(dh.i,f.di) + einstein(h.i,df.dij)) 
      
    }
    else {
      
      f[] <- cpp_paste(wrap(f), rep(h, each = prod(f.dim)/n), sep = " * ")

      df <- derivative(f, var = var, params = params, order = 1, accuracy = accuracy, stepsize = stepsize, drop = FALSE)
      index(df)[f.n.dim+0:1] <- "i"

      D <- einstein(df)
      
    }
    
    if(is.numeric(D))
      D <- D / eval(parse(text = J), as.list(qvar))
    else
      D[] <- cpp_paste(wrap(D), wrap(J), sep = " / ")
    
  }

  index(D) <- NULL
  if(drop & length(D)==1)
    dim(D) <- NULL

  return(D)

}

#' @describeIn divergence binary operator with default parameters.
#' 
#' @examples 
#' ### binary operator
#' c("x^2","y^3","z^4") %divergence% c("x","y","z")
#' 
#' @export
#' 
"%divergence%" <- function(f, var){
  
  divergence(f = f, var = var)
  
}

#' Numerical and Symbolic Curl
#' 
#' Computes the numerical curl of \code{functions} or the symbolic curl of \code{characters}
#' in arbitrary \href{https://en.wikipedia.org/wiki/Orthogonal_coordinates#Table_of_orthogonal_coordinates}{orthogonal coordinate systems}.
#' 
#' @details
#' The curl of a vector-valued function \eqn{F_i} at a point is represented by a 
#' vector whose length and direction denote the magnitude and axis of the maximum 
#' circulation. 
#' In 2 dimensions, the \code{curl} is computed in arbitrary orthogonal coordinate 
#' systems using the scale factors \eqn{h_i} and the Levi-Civita symbol \code{\link{epsilon}}:
#' 
#' \deqn{\nabla \times F = \frac{1}{h_1h_2}\sum_{ij}\epsilon_{ij}\partial_i\Bigl(h_jF_j\Bigl)= \frac{1}{h_1h_2}\Biggl(\partial_1\Bigl(h_2F_2\Bigl)-\partial_2\Bigl(h_1F_1\Bigl)\Biggl)}
#' 
#' In 3 dimensions:
#' 
#' \deqn{(\nabla \times F)_k = \frac{h_k}{J}\sum_{ij}\epsilon_{ijk}\partial_i\Bigl(h_jF_j\Bigl)}
#' 
#' where \eqn{J=\prod_i h_i}. In \eqn{m+2} dimensions, the \code{curl} is implemented in such
#' a way that the formula reduces correctly to the previous cases for \eqn{m=0} and \eqn{m=1}:
#' 
#' \deqn{(\nabla \times F)_{k_1\dots k_m} = \frac{h_{k_1}\cdots h_{k_m}}{J}\sum_{ij}\epsilon_{ijk_1\dots k_m}\partial_i\Bigl(h_jF_j\Bigl)}
#' 
#' When \eqn{F} is an \code{array} of vector-valued functions \eqn{F_{d_1,\dots,d_n,j}} the \code{curl} 
#' is computed for each vector:
#' 
#' \deqn{(\nabla \times F)_{d_1\dots d_n,k_1\dots k_m} = \frac{h_{k_1}\cdots h_{k_m}}{J}\sum_{ij}\epsilon_{ijk_1\dots k_m}\partial_i\Bigl(h_jF_{d_1\dots d_n,j}\Bigl)}
#'
#' @param f array of \code{characters} or a \code{function} returning a \code{numeric} array.
#' @param var vector giving the variable names with respect to which the derivatives are to be computed and/or the point where the derivatives are to be evaluated. See \code{\link{derivative}}.
#' @param params \code{list} of additional parameters passed to \code{f}.
#' @param accuracy degree of accuracy for numerical derivatives.
#' @param stepsize finite differences stepsize for numerical derivatives. It is based on the precision of the machine by default.
#' @param coordinates coordinate system to use. One of: \code{cartesian}, \code{polar}, \code{spherical}, \code{cylindrical}, \code{parabolic}, \code{parabolic-cylindrical} or a vector of scale factors for each varibale.
#' @param drop if \code{TRUE}, return the curl as a vector and not as an \code{array} for vector-valued functions.
#' 
#' @return Vector for vector-valued functions when \code{drop=TRUE}, \code{array} otherwise. 
#' 
#' @examples 
#' ### symbolic curl of a 2-d vector field
#' f <- c("x^3*y^2","x")
#' curl(f, var = c("x","y"))
#' 
#' ### numerical curl of a 2-d vector field in (x=1, y=1)
#' f <- function(x,y) c(x^3*y^2, x)
#' curl(f, var = c(x=1, y=1))
#' 
#' ### numerical curl of a 3-d vector field in (x=1, y=1, z=1)
#' f <- function(x,y,z) c(x^3*y^2, x, z)
#' curl(f, var = c(x=1, y=1, z=1))
#' 
#' ### vectorized interface
#' f <- function(x) c(x[1]^3*x[2]^2, x[1], x[3])
#' curl(f, var = c(1,1,1)) 
#' 
#' ### symbolic array of vector-valued 3-d functions
#' f <- array(c("x*y","x","y*z","y","x*z","z"), dim = c(2,3))
#' curl(f, var = c("x","y","z"))
#' 
#' ### numeric array of vector-valued 3-d functions in (x=1, y=1, z=1)
#' f <- function(x,y,z) array(c(x*y,x,y*z,y,x*z,z), dim = c(2,3))
#' curl(f, var = c(x=1, y=1, z=1))
#' 
#' @family differential operators
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#'
curl <- function(f, var, params = list(), coordinates = 'cartesian', accuracy = 4, stepsize = NULL, drop = TRUE){
  
  calculus.auto.wrap <- options(calculus.auto.wrap = TRUE)
  on.exit(options(calculus.auto.wrap), add = TRUE)
  
  is.fun <- is.function(f)
  if(is.fun)
    f.dj <- f.eval(f, var, params)
  else
    f.dj <- as.array(f)
  
  n <- length(var)
  f.dim <- dim(f.dj)
  f.n.dim <- length(f.dim)
  
  if(f.dim[f.n.dim]!=n)
    stop('f must be the same length of var on the last dimension')
  
  if(n<2)
    stop('at least 2 dimensions are required to compute the curl')
  
  if(is.fun){
    qvar <- var
    q <- paste0("q",1:n)
    names(qvar) <- q
  }
  else{
    qvar <- var
    q <- names(var)
    if(is.null(q))
      q <- var
  } 
  
  eps <- epsilon(n)
  index(eps)[1:2] <- c("i","j")
  
  h <- sf(var = q, coordinates = coordinates)
  
  if(is.null(h)){
    
    df.dji <- derivative(f, var = var, params = params, order = 1, accuracy = accuracy, stepsize = stepsize, drop = FALSE)
    index(df.dji)[f.n.dim+0:1] <- c("j","i")
    
    D <- einstein(df.dji, eps)
    
  }
  else if(is.fun){
    
    h.i <- h.j <- evaluate(h, qvar)
    index(h.i) <- "i"
    index(h.j) <- "j"
    
    dh.ji <- derivative(h, var = qvar, order = 1, accuracy = accuracy, stepsize = stepsize, drop = FALSE)
    index(dh.ji) <- c("j","i")   
    
    index(f.dj)[f.n.dim] <- "j"
    
    df.dji <- derivative(f, var = var, params = params, order = 1, accuracy = accuracy, stepsize = stepsize, drop = FALSE)
    index(df.dji)[f.n.dim+0:1] <- c("j","i")
    
    D <- einstein(f.dj, eps, 1/h.i, 1/h.j, dh.ji) + einstein(df.dji, eps, 1/h.i, 1/h.j, h.j)
    
  }
  else {
    
    f[] <- cpp_paste(wrap(f), rep(h, each = prod(f.dim)/n), sep = " * ")
    
    df.dji <- derivative(f, var = var, params = params, order = 1, accuracy = accuracy, stepsize = stepsize, drop = FALSE)
    index(df.dji)[f.n.dim+0:1] <- c("j","i")
    
    if(is.numeric(df.dji))
      h.i <- h.j <- 1/evaluate(h, var)
    else
      h.i <- h.j <- sprintf("%s^-1", h)
    
    index(h.i) <- "i"
    index(h.j) <- "j"
    
    D <- einstein(df.dji, eps, h.i, h.j)
    
  }
  
  index(D) <- NULL
  if(drop & length(D)==n^(n-2) & n<=3)
    dim(D) <- NULL
  
  return(D)
  
}
  
#' @describeIn curl binary operator with default parameters.
#' 
#' @examples 
#' ### binary operator
#' c("x*y","y*z","x*z") %curl% c("x","y","z")
#' 
#' @export
#'
"%curl%" <- function(f, var){
  
  curl(f = f, var = var)
  
}

#' Numerical and Symbolic Laplacian
#' 
#' Computes the numerical Laplacian of \code{functions} or the symbolic Laplacian of \code{characters} 
#' in arbitrary \href{https://en.wikipedia.org/wiki/Orthogonal_coordinates#Table_of_orthogonal_coordinates}{orthogonal coordinate systems}.
#' 
#' @details The Laplacian is a differential operator given by the divergence of the 
#' gradient of a scalar-valued function \eqn{F}, resulting in a scalar value giving 
#' the flux density of the gradient flow of a function. 
#' The \code{laplacian} is computed in arbitrary orthogonal coordinate systems using 
#' the scale factors \eqn{h_i}:
#' 
#' \deqn{\nabla^2F = \frac{1}{J}\sum_i\partial_i\Biggl(\frac{J}{h_i^2}\partial_iF\Biggl)}
#' 
#' where \eqn{J=\prod_ih_i}. When the function \eqn{F} is a tensor-valued function 
#' \eqn{F_{d_1\dots d_n}}, the \code{laplacian} is computed for each scalar component:
#' 
#' \deqn{(\nabla^2F)_{d_1\dots d_n} = \frac{1}{J}\sum_i\partial_i\Biggl(\frac{J}{h_i^2}\partial_iF_{d_1\dots d_n}\Biggl)}
#' 
#' @param f array of \code{characters} or a \code{function} returning a \code{numeric} array.
#' @param var vector giving the variable names with respect to which the derivatives are to be computed and/or the point where the derivatives are to be evaluated. See \code{\link{derivative}}.
#' @param params \code{list} of additional parameters passed to \code{f}.
#' @param accuracy degree of accuracy for numerical derivatives.
#' @param stepsize finite differences stepsize for numerical derivatives. It is based on the precision of the machine by default.
#' @param coordinates coordinate system to use. One of: \code{cartesian}, \code{polar}, \code{spherical}, \code{cylindrical}, \code{parabolic}, \code{parabolic-cylindrical} or a vector of scale factors for each varibale.
#' @param drop if \code{TRUE}, return the Laplacian as a scalar and not as an \code{array} for scalar-valued functions.
#' 
#' @return Scalar for scalar-valued functions when \code{drop=TRUE}, \code{array} otherwise.
#' 
#' @examples 
#' ### symbolic Laplacian 
#' laplacian("x^3+y^3+z^3", var = c("x","y","z"))
#' 
#' ### numerical Laplacian in (x=1, y=1, z=1)
#' f <- function(x, y, z) x^3+y^3+z^3
#' laplacian(f = f, var = c(x=1, y=1, z=1))
#' 
#' ### vectorized interface
#' f <- function(x) sum(x^3)
#' laplacian(f = f, var = c(1, 1, 1))
#' 
#' ### symbolic vector-valued functions
#' f <- array(c("x^2","x*y","x*y","y^2"), dim = c(2,2))
#' laplacian(f = f, var = c("x","y"))
#' 
#' ### numerical vector-valued functions
#' f <- function(x, y) array(c(x^2,x*y,x*y,y^2), dim = c(2,2))
#' laplacian(f = f, var = c(x=0,y=0))
#' 
#' @family differential operators
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
laplacian <- function(f, var, params = list(), coordinates = 'cartesian', accuracy = 4, stepsize = NULL, drop = TRUE){

  if(is.function(f)){

    n <- length(var)
    
    qvar <- var
    q <- paste0("q",1:n)
    names(qvar) <- q
    
    h <- sf(var = q, coordinates = coordinates)
    
    if(is.null(h)){
      
      ddf.di <- derivative(f = f, var = var, params = params, order = 2, accuracy = accuracy, stepsize = stepsize, drop = FALSE)
      L <- rowSums(ddf.di, dims = length(dim(ddf.di))-1)
      
    }
    else{
     
      J <- paste0(h, collapse = "*")
      
      if(n==1)
        h <- sprintf("1/%s", h)
      else
        h <- sapply(1:n, function(i) sprintf("%s/%s", paste0(h[-i], collapse = "*"), h[i]))
      
      h.i <- evaluate(h, qvar)
      index(h.i) <- "i"
      
      dh.i <- derivative(h, var = qvar, order = 1, accuracy = accuracy, stepsize = stepsize, drop = FALSE)
      index(dh.i) <- c("i","i")
      
      df.di <- derivative(f = f, var = var, params = params, order = 1, accuracy = accuracy, stepsize = stepsize, drop = FALSE)
      index(df.di)[length(dim(df.di))] <- "i"
      
      ddf.di <- derivative(f = f, var = var, params = params, order = 2, accuracy = accuracy, stepsize = stepsize, drop = FALSE)
      index(ddf.di)[length(dim(ddf.di))] <- "i"
      
      L <- (einstein(df.di, dh.i) + einstein(ddf.di, h.i)) / eval(parse(text = J), as.list(qvar))
       
    }
    
  }
  else {
    
    x <- names(var)
    if(is.null(x))
      x <- var
    
    g <- gradient(f = f, var = x, params = params, coordinates = coordinates, drop = FALSE)
    L <- divergence(g, var = var, params = params, coordinates = coordinates, drop = FALSE)
    
  }
  
  index(L) <- NULL
  if(drop & length(L)==1)
    dim(L) <- NULL
  
  return(L)
  
}

#' @describeIn laplacian binary operator with default parameters.
#' 
#' @examples 
#' ### binary operator
#' "x^3+y^3+z^3" %laplacian% c("x","y","z")
#' 
#' @export
#' 
"%laplacian%" <- function(f, var){
  
  return(laplacian(f = f, var = var))
  
}
