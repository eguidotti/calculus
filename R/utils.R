#' Wrap Characters in Parentheses
#' 
#' Wraps \code{characters} in round brackets.
#' 
#' @param x \code{character}.
#' 
#' @details Characters are automatically wrapped when performing basic symbolic operations to prevent unwanted results. E.g.: 
#' \deqn{a+b * c+d} 
#' instead of 
#' \deqn{(a+b) * (c+d)}
#' To disable this behaviour run \code{options(calculus.auto.wrap = FALSE)}.
#' 
#' @return \code{character}.
#' 
#' @examples
#' ### wrap characters
#' wrap("a+b")
#' 
#' ### wrap array of characters
#' wrap(array(letters[1:9], dim = c(3,3)))
#' 
#' @family utilities
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
wrap <- function(x){
  
  x[] <- paste0('(',x,')')
  return(x)
  
}

#' Expressions to Characters
#' 
#' Converts \code{expressions} to \code{characters}.
#' 
#' @param x \code{expression}.
#' 
#' @return \code{character}.
#' 
#' @examples
#' ### convert expressions
#' expr <- parse(text = "a")
#' e2c(expr)
#' 
#' ### convert array of expressions
#' expr <- array(parse(text = "a"), dim = c(2,2))
#' e2c(expr)
#' 
#' @family utilities
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
e2c <- function(x){
  
  char <- gsub(x = c(x), pattern = '\\s*\\\n\\s*', replacement = ' ')
  
  if(is.array(x)) 
    x <- array(char, dim = dim(x))
  else 
    x <- char
  
  return(x)
  
}

#' Characters to Expressions
#' 
#' Converts \code{characters} to \code{expressions}.
#' 
#' @param x \code{character}.
#' 
#' @return \code{expression}.
#' 
#' @examples
#' ### convert characters
#' c2e("a")
#' 
#' ### convert array of characters
#' c2e(array("a", dim = c(2,2)))
#' 
#' @family utilities
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
c2e <- function(x){
  
  expr <- sapply(x, function(x) parse(text = x))
  
  if(is.array(x)) 
    x <- array(expr, dim = dim(x))
  else 
    x <- expr
  
  return(x)
  
}

#' Tensor Indices
#' 
#' Functions to get or set the names of the dimensions of an \code{array}. 
#' 
#' @param x \code{array}.
#' 
#' @return Vector of indices.
#' 
#' @examples
#' ### array with no indices
#' x <- array(1, dim = c(1, 3, 2))
#' index(x)
#' 
#' ### indices on initialization
#' x <- array(1, dim = c(i=1, j=3, k=2))
#' index(x)
#' 
#' @family tensor algebra
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
index <- function(x){

  names(dim(x))
  
}

#' @describeIn index set indices.
#' 
#' @param value vector of indices.
#' 
#' @examples 
#' ### set indices on the fly
#' x <- array(1, dim = c(1, 3, 2))
#' index(x) <- c("i", "j", "k")
#' index(x)
#' 
#' @export
#' 
"index<-" <- function(x, value){
  
  x <- as.array(x)
  
  names(dim(x)) <- value
  
  return(x)
  
}

#' Tensor Diagonals
#' 
#' Functions to extract or replace the diagonals of an \code{array}, or construct a diagonal \code{array}.
#' 
#' @param x an \code{array} from which to extract the diagonals, or a vector giving the diagonal values to construct the \code{array}.
#' @param dim the dimensions of the (square) \code{array} to construct when \code{x} is a vector.
#' 
#' @return Vector of the diagonal entries of \code{x} if \code{x} is an \code{array}. 
#' If \code{x} is a vector, returns the diagonal \code{array} with the 
#' entries given by \code{x}.
#' 
#' @examples 
#' ### 3x3 matrix
#' diagonal(x = 1, dim = c(3,3))
#'
#' ### 2x2x2 array
#' diagonal(x = 1:2, dim = c(2,2,2))
#' 
#' ### extract diagonals 
#' x <- diagonal(1:5, dim = c(5,5,5))
#' diagonal(x)
#' 
#' @family tensor algebra
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
diagonal <- function(x = 1, dim = rep(2,2)){
  
  d <- dim(x)
  n <- length(d)
  if(n>0){
    if(length(unique(d)) > 1)
      stop("Unable to extract diagonals: not a square array")
    else
      return(x[1 + 0:(d[1]-1) * sum(d[1]^(n-1:n))])
  }
  
  a <- array(0, dim = dim)
  diagonal(a) <- x
  
  return(a)
}

#' @describeIn diagonal set diagonals.
#' 
#' @param value vector giving the values of the diagonal entries.
#' 
#' @examples 
#' ### set diagonals
#' x <- array(0, dim = c(2,2,2))
#' diagonal(x) <- 1:2
#' x
#' 
#' @export
#' 
"diagonal<-" <- function(x, value){
  
  x <- as.array(x)
  d <- dim(x)
  n <- length(d)
  
  if(length(unique(d))!=1)
    stop("not a square array")
  
  x[1 + 0:(d[1]-1) * sum(d[1]^(n-1:n))] <- rep(value, length.out = d[1])
  
  return(x)
  
}

# check coordinates
ccheck <- function(var, coordinates){
  
  n   <- length(var)
  n.c <- length(coordinates)
  
  supported <- c('cartesian', 'polar', 'spherical', 'cylindrical', 'parabolic', 'parabolic-cylindrical')
  if((n.c==1 && n>1 && !(coordinates %in% supported)) | (n.c>1 && n.c!=n))
      stop(paste("coordinates must be one of: \n 
               -", paste0("'", supported, "'", collapse = ','), "\n 
               - vector of scale factors for each coordinate"))
  
  if(n.c==1 && n>1){
    
    if(coordinates=='polar' && n>2)
      stop("var is not a vector of polar coordinates: c('rho','phi')")
    
    if(coordinates=='spherical' && n>3)
      stop("var is not a vector of spherical coordinates: c('r','theta','phi')")
    
    if(coordinates=='cylindrical' && n>3)
      stop("var is not a vector of cylindrical coordinates: c('rho','phi','z')")
  
    if(coordinates=='parabolic' && n>3)
      stop("var is not a vector of Parabolic coordinates: c('u','v','phi')")
    
    if(coordinates=='parabolic-cylindrical' && n>3)
      stop("var is not a vector of parabolic cylindrical coordinates: c('u','v','z')")
    
  }
  
}

# scale factors
sf <- function(var, coordinates){
  
  ccheck(var = var, coordinates = coordinates)
  
  if(length(coordinates)>1)
    return(wrap(coordinates))
  
  h <- switch (coordinates,
               
               "cartesian" = NULL,
               
               "polar" = c("1", var[1]),
               
               "spherical" = c("1", var[1], sprintf("(%s*sin(%s))", var[1], var[2])),
               
               "cylindrical" = c("1", var[1], "1"),
               
               "parabolic" = c(sprintf("((%s^2+%s^2)^(0.5))", var[1], var[2]), sprintf("((%s^2+%s^2)^(0.5))", var[1], var[2]), sprintf("(%s*%s)", var[1], var[2])),
               
               "parabolic-cylindrical" = c(sprintf("((%s^2+%s^2)^(0.5))", var[1], var[2]), sprintf("((%s^2+%s^2)^(0.5))", var[1], var[2]), "1"),
               
               wrap(coordinates)
  )
  
  return(h[1:length(var)])
  
}

# function dimension
f.eval <- function(f, var, params, array = TRUE, dim = FALSE){
  
  if(!is.list(var) && is.null(names(var)))
    var <- list(var)
  else 
    var <- as.list(var)
  
  x <- do.call(f, c(var, params))
  
  if(array & !is.array(x))
    x <- as.array(x)
  
  if(dim)
    return(dim(x))
  
  return(x)
  
}
