#' Numerical and Symbolic Sum
#' 
#' Adds numeric or character arrays element by element.
#' 
#' @param x character or numeric array.
#' @param y character or numeric array.
#' 
#' @return character or numeric array.
#' 
#' @examples 
#' # sum vector 
#' x <- c("a+1","b+2")
#' x %sum% x
#' 
#' # sum matrix 
#' x <- matrix(letters[1:4], ncol = 2)
#' x %sum% x
#' 
#' # sum array
#' x <- array(letters[1:12], dim = c(2,2,3))
#' y <- array(1:12, dim = c(2,2,3))
#' x %sum% x
#' y %sum% y
#' x %sum% y
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
#' Substracts numeric or character arrays element by element.
#' 
#' @param x character or numeric array.
#' @param y character or numeric array.
#' 
#' @return character or numeric array.
#' 
#' @examples 
#' # diff vector 
#' x <- c("a+1","b+2")
#' x %diff% x
#' 
#' # diff matrix 
#' x <- matrix(letters[1:4], ncol = 2)
#' x %diff% x
#' 
#' # diff array
#' x <- array(letters[1:12], dim = c(2,2,3))
#' y <- array(1:12, dim = c(2,2,3))
#' x %diff% x
#' y %diff% y
#' x %diff% y
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
  
  if(is.numeric(x) && is.numeric(y))
    return(x-y)
  
  if(getOption('auto.wrap', default = TRUE)){
    x <- wrap(x)
    y <- wrap(y)
  }
  
  return(array(cpp_paste(as.character(x), as.character(y), sep = " - "), dim = dim(x)))
  
}



#' Numerical and Symbolic Product
#' 
#' Multiplies numeric or character arrays element by element.
#' 
#' @param x character or numeric array.
#' @param y character or numeric array.
#' 
#' @return character or numeric array.
#' 
#' @examples 
#' # prod vector 
#' x <- c("a+1","b+2")
#' x %prod% x
#' 
#' # prod matrix 
#' x <- matrix(letters[1:4], ncol = 2)
#' x %prod% x
#' 
#' # prod array
#' x <- array(letters[1:12], dim = c(2,2,3))
#' y <- array(1:12, dim = c(2,2,3))
#' x %prod% x
#' y %prod% y
#' x %prod% y
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
  
  if(is.numeric(x) && is.numeric(y))
    return(x*y)
    
  if(getOption('auto.wrap', default = TRUE)){
    x <- wrap(x)
    y <- wrap(y)
  }
  
  return(array(cpp_paste(as.character(x), as.character(y), sep = " * "), dim = dim(x)))
    
}





#' Numerical and Symbolic Division
#' 
#' Divide numeric or character arrays element by element.
#' 
#' @param x character or numeric array.
#' @param y character or numeric array.
#' 
#' @return character or numeric array.
#' 
#' @examples 
#' # div vector 
#' x <- c("a+1","b+2")
#' x %div% x
#' 
#' # div matrix 
#' x <- matrix(letters[1:4], ncol = 2)
#' x %div% x
#' 
#' # div array
#' x <- array(letters[1:12], dim = c(2,2,3))
#' y <- array(1:12, dim = c(2,2,3))
#' x %div% x
#' y %div% y
#' x %div% y 
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
  
  if(is.numeric(x) && is.numeric(y))
    return(x/y)
  
  if(getOption('auto.wrap', default = TRUE)){
    x <- wrap(x)
    y <- wrap(y)
  }
  
  return(array(cpp_paste(as.character(x), as.character(y), sep = " / "), dim = dim(x)))
  
}




#' Numerical and Symbolic Matrix Product
#' 
#' Multiplies two character or numeric matrices, if they are conformable. If one argument is a vector, it will be promoted to either a row or column matrix to make the two arguments conformable. If both are vectors of the same length, it will return the inner product (as a matrix).
#' 
#' @param x character or numeric matrix.
#' @param y character or numeric matrix.
#' 
#' @return character or numeric matrix.
#' 
#' @examples 
#' # numeric inner product 
#' x <- 1:4
#' x %matrix% x  
#' 
#' # symbolic inner product 
#' x <- letters[1:4]
#' x %matrix% x
#' 
#' # matrix products
#' x <- letters[1:4]
#' y <- diag(4)
#' z <- array(1:12, dim = c(4,3))
#' y %matrix% z
#' y %matrix% x
#' x %matrix% z
#' 
#' @export
#' 
"%matrix%" <- function(x, y){
  
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
  
  return(einstein(x, y))
  
}



#' Numerical and Symbolic Dot Product
#' 
#' Computes the inner product on the last dimensions of two character or numeric arrays.
#' 
#' @param x character or numeric array.
#' @param y character or numeric array.
#' 
#' @return character or numeric array.
#' 
#' @examples 
#' # inner product 
#' x <- array(1:12, dim = c(3,4))
#' x %dot% x
#' 
#' # inner product on last dimensions 
#' x <- array(1:24, dim = c(3,2,4))
#' y <- array(letters[1:8], dim = c(2,4))
#' x %dot% y
#' y %dot% x
#' 
#' @export
#' 
"%dot%" <- function(x, y) {
  
  x <- as.array(x)
  y <- as.array(y)
  
  x.n.dim <- length(dim(x))
  y.n.dim <- length(dim(y))
  
  j.n <- min(x.n.dim, y.n.dim)
  j   <- letters[1:j.n]
  
  index(x) <- c(seq(length.out = x.n.dim-j.n), j)
  index(y) <- c(seq(length.out = y.n.dim-j.n), j)
  
  return(einstein(x, y))
  
}




#' Numerical and Symbolic Inner Product
#' 
#' Computes the inner product of two character or numeric arrays.
#' 
#' @param x character or numeric array.
#' @param y character or numeric array.
#' 
#' @return character or numeric.
#' 
#' @examples 
#' # numeric inner product 
#' x <- array(1:12, dim = c(3,4))
#' x %inner% x
#' 
#' # symbolic inner product 
#' x <- array(letters[1:12], dim = c(3,4))
#' x %inner% x
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
  
  if(is.numeric(x) && is.numeric(y))
    return(array(sum(x*y)))
  
  if(getOption('auto.wrap', default = TRUE)){
    x <- wrap(x)
    y <- wrap(y)
  }
  
  return(cpp_inner(as.character(x), as.character(y)))
  
}



#' Numerical and Symbolic Outer Product
#' 
#' Computes the outer product of two character or numeric arrays.
#' 
#' @param x character or numeric array.
#' @param y character or numeric array.
#' 
#' @return character or numeric array.
#' 
#' @examples 
#' # numeric outer product 
#' c(1,2) %outer% c(2,3)
#' 
#' # symbolic outer product 
#' c('a','b') %outer% c('c','d') %outer% c('e','f')
#' 
#' @export
#' 
"%outer%" <- function(x,y){
  
  x <- as.array(x)
  y <- as.array(y)
  
  if(is.numeric(x) && is.numeric(y))
    return(x %o% y)
  
  if(getOption('auto.wrap', default = TRUE)){
    x <- wrap(x)
    y <- wrap(y)
  }
    
  return(array(cpp_outer(as.character(x), as.character(y)), dim = c(dim(x), dim(y))))
  
}



#' Numerical and Symbolic Kronecker Product
#' 
#' Computes the Kronecker product of two character or numeric arrays.
#' 
#' @param x character or numeric array.
#' @param y character or numeric array.
#' 
#' @return character or numeric array.
#' 
#' @examples 
#' # numeric Kronecker product 
#' c(1,2) %kronecker% c(2,3)
#' 
#' # symbolic Kronecker product 
#' array(1:4, dim = c(2,2)) %kronecker% c('c','d')
#' 
#' @export
#' 
"%kronecker%" <- function(x,y){
  
  if(is.numeric(x) && is.numeric(y))
    return(x%x%y)
  
  if(getOption('auto.wrap', default = TRUE)){
    x <- wrap(x)
    y <- wrap(y)
  }
  
  return(base::kronecker(x, y, function(x,y){
    cpp_paste(x, y, sep = " * ")
  }))
  
}




#' Numerical and Symbolic Cross Product
#' 
#' Computes the generic cross product of N-1 vectors of length N. 
#' 
#' @describeIn cross N-d cross product 
#' 
#' @param ... N-1 vectors of length N.
#' @param x 3-d vector
#' @param y 3-d vector
#' 
#' @return N-dimensional vector orthogonal to the N-1 vectors.
#' 
#' @examples
#' # canonical basis 3-d
#' c(1,0,0) %cross% c(0,1,0)
#' 
#' # canonical basis 4-d
#' cross(c(1,0,0,0), c(0,1,0,0), c(0,0,0,1))
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
    stop("must supply N-1 vectors of length N")
  }
  
  m <- do.call(rbind, args)
  
  if(!is.numeric(m) && !getOption('auto.wrap', default = TRUE))
    return(sapply(seq(len), function(i) {
      wrap(det(m[,-i,drop=FALSE])) %prod% (-1)^(i+1)
    }))
  
  return(sapply(seq(len), function(i) {
    det(m[,-i,drop=FALSE]) %prod% (-1)^(i+1)
  }))
  
}



#' @describeIn cross 3-d cross product
#' 
#' @export
#'
"%cross%" <- function(x, y){
  
  return(cross(x, y))
  
}






#' Numerical and Symbolic Gradient
#' 
#' Computes the gradient or jacobian of functions, expressions and characters.
#' 
#' @describeIn gradient arbitrary coordinate system
#' 
#' @param f function, expression or character array.
#' @param var character vector, giving the variable names with respect to which derivatives will be computed. If a named vector is provided, derivatives will be computed at that point.
#' @param accuracy accuracy degree for numerical derivatives.
#' @param stepsize finite differences stepsize for numerical derivatives. Auto-optimized by default.
#' @param coordinates coordinate system to use. One of: \code{cartesian}, \code{polar}, \code{spherical}, \code{cylindrical}, \code{parabolic}, \code{parabolic-cylindrical} or a character vector of scale factors for each varibale.
#' 
#' @return gradient or jacobian array.
#' 
#' @examples 
#' # gradient with respect to x
#' gradient(f = "sin(x)", var = "x")
#' "sin(x)" %gradient% "x"
#' 
#' # gradient with respect to x and evaluate in x = 0
#' gradient(f = "sin(x)", var = c("x" = 0))
#' "sin(x)" %gradient% c(x=0)
#' 
#' # gradient with respect to (x,y)
#' gradient(f = "y*sin(x)", var = c("x","y"))
#' "y*sin(x)" %gradient% c("x","y")
#' 
#' # jacobian with respect to (x,y)
#' f <- c("y*sin(x)", "x*cos(y)")
#' gradient(f = f, var = c("x","y"))
#' f %gradient% c("x","y")
#' 
#' # jacobian with respect to (x,y) and evaluate in (x = 0, y = 0)
#' f <- c(function(x, y) y*sin(x), function(x, y) x*cos(y))
#' gradient(f = f, var = c(x=0,y=0))
#' f %gradient% c(x=0,y=0)
#' 
#' # gradient in spherical coordinates
#' gradient('r*theta*phi', var = c('r','theta','phi'), coordinates = 'spherical')
#' 
#' # numerical gradient in spherical coordinates
#' f <- function(r, theta, phi) r*theta*phi
#' gradient(f, var = c('r'=1, 'theta'=pi/4, 'phi'=pi/4), coordinates = 'spherical')
#' 
#' @export
#' 
gradient <- function(f, var, accuracy = 2, stepsize = NULL, coordinates = 'cartesian'){
  
  x <- names(var)
  if(is.null(x))
    x <- var
  
  f <- derivative(f = f, var = var, accuracy = accuracy, stepsize = stepsize)
  
  h <- sf(var = x, coordinates = coordinates)
  
  if(is.null(h))
    return(f)
  
  if(is.numeric(f)){
    
    h <- evaluate(h, as.list(var))
    h <- rep(h, each = length(f)/length(var))
    f <- f/h
    
  }
  else{
  
    h <- sprintf("1/%s", h)
    h <- rep(h, each = length(f)/length(var))
    f[] <- cpp_paste(h, wrap(f), sep = " * ")
    
  }
  
  return(f)  

}



#' @describeIn gradient cartesian coordinates
#' 
#' @export
#' 
"%gradient%" <- function(f, var){
  
  return(gradient(f = f, var = var))
  
}




#' Numerical and Symbolic Hessian
#' 
#' Computes the hessian matrix of functions, expressions and characters.
#' 
#' @describeIn hessian arbitrary coordinate system
#' 
#' @param f function, expression or character.
#' @param var character vector, giving the variable names with respect to which derivatives will be computed. If a named vector is provided, derivatives will be computed at that point.
#' @param accuracy accuracy degree for numerical derivatives.
#' @param stepsize finite differences stepsize for numerical derivatives. Auto-optimized by default.
#' @param coordinates coordinate system to use. One of: \code{cartesian}, \code{polar}, \code{spherical}, \code{cylindrical}, \code{parabolic}, \code{parabolic-cylindrical} or a character vector of scale factors for each varibale.
#' 
#' @return hessian matrix.
#' 
#' @examples 
#' # hessian with respect to x
#' hessian(f = "sin(x)", var = "x")
#' "sin(x)" %hessian% "x"
#' 
#' # hessian with respect to x and evaluate in x = 0
#' hessian(f = "sin(x)", var = c("x" = 0))
#' "sin(x)" %hessian% c(x=0)
#' 
#' # hessian with respect to (x,y)
#' hessian(f = "y*sin(x)", var = c("x","y"))
#' "y*sin(x)" %hessian% c("x","y")
#' 
#' # hessian in spherical coordinates
#' hessian('r*theta*phi', var = c('r','theta','phi'), coordinates = 'spherical')
#' 
#' # numerical hessian in spherical coordinates
#' f <- function(r, theta, phi) r*theta*phi
#' hessian(f, var = c('r'=1, 'theta'=pi/4, 'phi'=pi/4), coordinates = 'spherical')
#' 
#' @export
#' 
hessian <- function(f, var, accuracy = 2, stepsize = NULL, coordinates = 'cartesian'){
  
  if(length(f)>1)
    stop("f must be of length 1")
  
  x <- names(var)
  if(is.null(x))
    x <- var
  
  if(is.fun(f)){
      
    n <- length(x)
    p <- partitions(n = 2, length = n, perm = TRUE, fill = TRUE)
    h <- sf(var = x, coordinates = coordinates)
    hess <- matrix(nrow = n, ncol = n)
    
    apply(p, 2, function(o){
      
      ij <- which(o!=0)
      if(length(ij)==1) 
        ij <- c(ij, ij)
      
      d.ij <- DD.n(f = f, x0 = var, order = o, accuracy = accuracy, stepsize = stepsize)
      
      if(is.null(h)){
      
        hess[ij[1], ij[2]] <<- d.ij
        hess[ij[2], ij[1]] <<- d.ij
        
      }
      else{

        for(i in 1:2){
        
          h.i <- evaluate(h[ij[1]], as.list(var))
          h.j <- evaluate(h[ij[2]], as.list(var))
          
          o <- rep(0, n)
          o[ij[1]] <- 1
          d.i <- derivative(f = sprintf("1/%s", h[ij[2]]), var = var, order = o)[[1]]

          o <- rep(0, n)
          o[ij[2]] <- 1
          d.j <- DD.n(f = f, x0 = var, order = o, accuracy = accuracy, stepsize = stepsize)
          
          hess[ij[2], ij[1]] <<- 1/h.i * (d.i*d.j + 1/h.j*d.ij)
          
          if(ij[1]!=ij[2])
            ij <- rev(ij)
          else 
            break
          
        }
        
      }
      
      return(NULL)
      
    })
     
    return(hess)
         
  } 
  
  return(gradient(gradient(f = f, var = x, coordinates = coordinates), var = var, coordinates = coordinates)[1,,])
  
}



#' @describeIn hessian cartesian coordinates
#' 
#' @export
#' 
"%hessian%" <- function(f, var){
  
  return(hessian(f = f, var = var))
  
}







#' Numerical and Symbolic Divergence
#' 
#' Computes the divergence of functions, expressions and characters.
#' 
#' @describeIn divergence arbitrary coordinate system
#' 
#' @param f function, expression or character array.
#' @param var character vector, giving the variable names with respect to which derivatives will be computed. If a named vector is provided, derivatives will be computed at that point.
#' @param accuracy accuracy degree for numerical derivatives.
#' @param stepsize finite differences stepsize for numerical derivatives. Auto-optimized by default.
#' @param coordinates coordinate system to use. One of: \code{cartesian}, \code{polar}, \code{spherical}, \code{cylindrical}, \code{parabolic}, \code{parabolic-cylindrical} or a character vector of scale factors for each varibale.
#' 
#' @return divergence array.
#' 
#' @examples 
#' # divergence of a vector field
#' f <- c('x^2','y^3','z^4')
#' divergence(f, var = c('x','y','z'))
#' f %divergence% c('x','y','z')
#' 
#' # numerical divergence of a vector field
#' f <- c(function(x,y,z) x^2, function(x,y,z) y^3, function(x,y,z) z^4)
#' divergence(f, var = c('x'=1,'y'=1,'z'=1))
#' f %divergence% c('x'=1,'y'=1,'z'=1)
#' 
#' # divergence of array of vector fields
#' f1 <- c('x^2','y^3','z^4')
#' f2 <- c('x','y','z')
#' a <- matrix(c(f1,f2), nrow = 2, byrow = TRUE)
#' divergence(a, var = c('x','y','z'))
#' a %divergence% c('x','y','z')
#' 
#' # divergence in polar coordinates
#' f <- c('sqrt(r)/10','sqrt(r)')
#' divergence(f, var = c('r','phi'), coordinates = 'polar')
#' 
#' @export
#' 
divergence <- function(f, var, accuracy = 2, stepsize = NULL, coordinates = 'cartesian'){
  
  is.fun <- is.fun(f)
  
  if(is.fun && !is.array(f))
    f <- array(c(f))
  else 
    f <- as.array(f)
  
  n       <- length(var)
  f.dim   <- dim(f)
  f.n.dim <- length(f.dim)
  
  x <- names(var)
  if(is.null(x))
    x <- var
  
  if(f.dim[f.n.dim]!=n)
    stop('f must be the same length of var on the last dimension')
  
  H <- NULL
  h <- sf(var = x, coordinates = coordinates)
  
  if(!is.null(h)){
    
    if(is.fun){
      f.orig <- f
      f[] <- sapply(1:length(f), function(i){
        sprintf('do.call(get("f.orig")[[%s]], list(%s))', i, paste(x, x, sep = '=', collapse = ','))
      })
    }
    
    H <- paste0(h, collapse = "*")
    
    if(n==1) 
      h <- "1"
    else
      h <- sapply(1:n, function(k) paste0(h[-k], collapse = "*"))
    
    index(h)          <- 1
    index(f)[f.n.dim] <- 1
    
    f <- einstein(f, h, drop = FALSE)
    
    if(is.fun){
      f <- array(sapply(f, function(f){
        eval(c2e(sprintf("function(%s) {%s}", paste0(x, collapse = ','), f)))
      }), dim = dim(f))
    }
    
  }
  
  f <- derivative(f = f, var = var, accuracy = accuracy, stepsize = stepsize)
  index(f)[f.n.dim+0:1] <- 1  
  
  f <- trace(f)
  
  if(!is.null(H)){
    
    if(is.numeric(f))
      f <- f / as.numeric(evaluate(H, as.list(var)))
    else
      f[] <- sprintf("(%s)/(%s)", f, H)
    
  }

  return(f)

}





#' @describeIn divergence cartesian coordinates
#' 
#' @export
#' 
"%divergence%" <- function(f, var){
  
  return(divergence(f = f, var = var))
  
}





#' Numerical and Symbolic Curl
#' 
#' Computes the curl of functions, expressions and characters.
#' 
#' @describeIn curl arbitrary coordinate system
#' 
#' @param f function, expression or character array.
#' @param var character vector, giving the variable names with respect to which derivatives will be computed. If a named vector is provided, derivatives will be computed at that point.
#' @param accuracy accuracy degree for numerical derivatives.
#' @param stepsize finite differences stepsize for numerical derivatives. Auto-optimized by default.
#' @param coordinates coordinate system to use. One of: \code{cartesian}, \code{polar}, \code{spherical}, \code{cylindrical}, \code{parabolic}, \code{parabolic-cylindrical} or a character vector of scale factors for each varibale.
#' 
#' @return curl array.
#' 
#' @examples 
#' # curl of a vector field
#' f <- c('x*y','y*z','x*z')
#' curl(f, var = c('x','y','z'))
#' f %curl% c('x','y','z')
#' 
#' # irrotational vector field
#' f <- c('x','-y','z')
#' curl(f, var = c('x','y','z'))
#' f %curl% c('x','y','z')
#' 
#' # numerical curl of a vector field
#' f <- c(function(x,y,z) x*y, function(x,y,z) y*z, function(x,y,z) x*z)
#' curl(f, var = c('x'=1,'y'=1,'z'=1))
#' f %curl% c('x'=1,'y'=1,'z'=1)
#' 
#' # curl of array of vector fields
#' f1 <- c('x*y','y*z','z*x')
#' f2 <- c('x','-y','z')
#' a <- matrix(c(f1,f2), nrow = 2, byrow = TRUE)
#' curl(a, var = c('x','y','z'))
#' a %curl% c('x','y','z')
#' 
#' # curl in polar coordinates
#' f <- c('sqrt(r)/10','sqrt(r)')
#' curl(f, var = c('r','phi'), coordinates = 'polar')
#' 
#' @export
#'
curl <- function(f, var, accuracy = 2, stepsize = NULL, coordinates = 'cartesian'){
  
  is.fun <- is.fun(f)
  
  if(is.fun && !is.array(f))
    f <- array(c(f))
  else 
    f <- as.array(f)
  
  f.dim   <- dim(f)
  f.n.dim <- length(f.dim)
  
  x <- names(var)
  if(is.null(x))
    x <- var
  
  n <- length(var)
  if(f.dim[f.n.dim]!=n)
    stop('f must be the same length of var on the last dimension')
  
  if(n==1)
    return(array("0", dim = f.dim))
  
  h <- sf(var = x, coordinates = coordinates)
  
  eps <- levicivita(n)
  index(eps) <- 1:n
  
  if(!is.null(h)){
    
    if(is.fun){
      f.orig <- f
      f[] <- sapply(1:length(f), function(i){
        sprintf('do.call(get("f.orig")[[%s]], list(%s))', i, paste(x, x, sep = '=', collapse = ','))
      })
    }
    
    h.i <- sprintf("1/%s", h)
    index(h.i) <- 1
    
    h.j <- sprintf("1/%s", h)
    index(h.j) <- 2
    
    index(h)          <- 2
    index(f)[f.n.dim] <- 2
    f                 <- einstein(f, h, drop = FALSE)
    
    if(is.fun){
      f <- array(sapply(f, function(f){
        eval(c2e(sprintf("function(%s) {%s}", paste0(x, collapse = ','), f)))
      }), dim = dim(f))
    }
    
  }
  
  f <- derivative(f = f, var = var, accuracy = accuracy, stepsize = stepsize)
  index(f)[f.n.dim]   <- 2
  index(f)[f.n.dim+1] <- 1  
  
  if(!is.numeric(f) && !getOption('auto.wrap', default = TRUE))
    f <- wrap(f)
  
  if(is.null(h))
    return(einstein(f, eps))

  if(is.numeric(f)){
    h.i <- evaluate(h.i, as.list(var))
    h.j <- evaluate(h.j, as.list(var))
  }
  
  return(einstein(f, eps, h.i, h.j))
  
}
  






#' @describeIn curl cartesian coordinates
#' 
#' @export
#'
"%curl%" <- function(f, var){
  
  return(curl(f = f, var = var))
  
}






#' Numerical and Symbolic Laplacian
#' 
#' Computes the laplacian of functions, expressions and characters.
#' 
#' @describeIn laplacian arbitrary coordinate system
#' 
#' @param f function, expression or character array.
#' @param var character vector, giving the variable names with respect to which derivatives will be computed. If a named vector is provided, derivatives will be computed at that point.
#' @param accuracy accuracy degree for numerical derivatives.
#' @param stepsize finite differences stepsize for numerical derivatives. Auto-optimized by default.
#' @param coordinates coordinate system to use. One of: \code{cartesian}, \code{polar}, \code{spherical}, \code{cylindrical}, \code{parabolic}, \code{parabolic-cylindrical} or a character vector of scale factors for each varibale.
#' 
#' @return laplacian array.
#' 
#' @examples 
#' # laplacian of a scalar field
#' f <- 'x^2+y^2+z^2'
#' laplacian(f, var = c('x','y','z'))
#' f %laplacian% c('x','y','z')
#' 
#' # laplacian of scalar fields
#' f <- c('x^2','y^3','z^4')
#' laplacian(f, var = c('x','y','z'))
#' f %laplacian% c('x','y','z')
#' 
#' # numerical laplacian of scalar fields
#' f <- c(function(x,y,z) x^2, function(x,y,z) y^3, function(x,y,z) z^4)
#' laplacian(f, var = c('x'=1,'y'=1,'z'=1))
#' f %laplacian% c('x'=1,'y'=1,'z'=1)
#' 
#' # laplacian of array of scalar fields
#' f1 <- c('x^2','y^3','z^4')
#' f2 <- c('x','y','z')
#' a <- matrix(c(f1,f2), nrow = 2, byrow = TRUE)
#' laplacian(a, var = c('x','y','z'))
#' a %laplacian% c('x','y','z')
#' 
#' # laplacian in polar coordinates
#' f <- c('sqrt(r)/10','sqrt(r)')
#' laplacian(f, var = c('r','phi'), coordinates = 'polar')
#' 
#' @export
#' 
laplacian <- function(f, var, accuracy = 2, stepsize = NULL, coordinates = 'cartesian'){
  
  x <- names(var)
  if(is.null(x))
    x <- var
  
  if(is.fun(f)){
    
    if(!is.array(f))
      f <- array(c(f))
    
    n <- length(var)
    h <- sf(var = x, coordinates = coordinates)
    
    if(!is.null(h)){
      
      H <- as.numeric(evaluate(paste0(h, collapse = "*"), as.list(var)))
      
      if(n==1) 
        h <- sprintf("1/%s", h)
      else
        h <- sapply(1:n, function(i) sprintf("(%s)/%s", paste0(h[-i], collapse = "*"), h[i]))
      
      d.h <- sapply(1:n, function(i) {
        o <- rep(0, n)
        o[i] <- 1
        derivative(f = h[i], var = var, order = o)[[1]]
      })
      
      h <- evaluate(h, as.list(var))
      
    }
    
    nabla <- sapply(f, function(f){
    
      sum(sapply(1:n, function(i){
        
        o <- rep(0, n)
        o[i] <- 2
        d.ff <- DD.n(f = f, x0 = var, order = o, accuracy = accuracy, stepsize = stepsize)
        
        if(is.null(h))
          return(d.ff)
          
        o <- rep(0, n)
        o[i] <- 1
        d.f <- DD.n(f = f, x0 = var, order = o, accuracy = accuracy, stepsize = stepsize)
        
        return((d.h[i]*d.f + h[i]*d.ff)/H)
        
      }))
        
    })
    
    return(array(nabla, dim = dim(f)))
    
  }
  
  return(divergence(gradient(f = f, var = x, coordinates = coordinates), var = var, coordinates = coordinates))
  
}





#' @describeIn laplacian cartesian coordinates
#' 
#' @export
#' 
"%laplacian%" <- function(f, var){
  
  return(laplacian(f = f, var = var))
  
}




