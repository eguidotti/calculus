#' Wrap Character
#' 
#' Wraps characters in round brackets.
#' 
#' @param x characters
#' 
#' @details Characters are automatically wrapped when performing basic symbolic operations to prevent unwanted results. E.g.: 
#' \deqn{a+b * c+d} 
#' instead of 
#' \deqn{(a+b) * (c+d)}
#' To disable this behaviour run \code{options(auto.wrap = FALSE)}.
#' 
#' @return wrapped characters.
#' 
#' @examples
#' # wrap characters
#' wrap('a+b')
#' 
#' # wrap array of characters
#' wrap(array(letters[1:9], dim = c(3,3)))
#' 
#' @export
#' 
wrap <- function(x){
  
  x[] <- paste0('(',x,')')
  return(x)
  
}



#' Expression to Character
#' 
#' Converts expressions to characters
#' 
#' @param x expressions
#' 
#' @return characters.
#' 
#' @examples
#' # convert expressions
#' expr <- parse(text = 'a')
#' e2c(expr)
#' 
#' # convert array of expressions
#' expr <- array(parse(text = 'a'), dim = c(2,2))
#' e2c(expr)
#' 
#' @seealso \code{\link{c2e}}
#' 
#' @export
#' 
e2c <- function(x){
  
  char <- gsub(x = gsub(x = c(x), pattern = '\\s*\\\n\\s*', replacement = ' '), pattern = ' +', ' ')
  
  if(is.array(x)) 
    x <- array(char, dim = dim(x))
  else 
    x <- char
  
  return(x)
  
}



#' Character to Expression
#' 
#' Converts characters to expressions
#' 
#' @param x characters
#' 
#' @return expressions.
#' 
#' @examples
#' # convert characters
#' c2e('a')
#' 
#' # convert array of characters
#' c2e(array('a', dim = c(2,2)))
#' 
#' @seealso \code{\link{e2c}}
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



#' Einstein Notation Indices
#' 
#' Get and set indices: names of the array's dimensions. See also \code{\link{einstein}}.
#' 
#' @describeIn index get indices.
#' 
#' @param x array.
#' @param value vector of indices.
#' 
#' @return array indices.
#' 
#' @examples
#' # define array
#' a <- array(1, dim = c(1,3,2))
#' 
#' # get indices
#' index(a)
#' 
#' # set indices
#' index(a) <- c('i', 'j', 'k')
#' 
#' # get indices
#' index(a)
#' 
#' # dimensions
#' dim(a)
#' 
#' @seealso \code{\link{einstein}}, \code{\link[base]{dim}}
#' 
#' @export
#' 
index <- function(x){
  
  return(names(dim(x)))
  
}


#' @describeIn index set indices.
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
#' Extracts or replace the diagonal of a tensor, or construct a diagonal tensor.
#' 
#' @describeIn diag get diagonals.
#' 
#' @param x array, vector or integer.
#' @param dim the dimension of the tensor. 
#' @param value the value for the diagonal elements.
#' 
#' 
#' @return array diagonals.
#' 
#' @examples 
#' # construct a diagonal 2x2 matrix
#' diag(2)
#'
#' # construct a diagonal 2x2x2 tensor
#' diag(2, dim = 3)
#' 
#' # construct a diagonal 2x2x2 tensor with values 3 and 4
#' diag(2, dim = 3, value = c(3,4))
#'   
#' # construct a diagonal 3x3 matrix with values 1,2,3
#' diag(1:3) 
#'   
#' # extract diagonals 
#' x <- diag(1:4, dim = 3)
#' diag(x)
#' 
#' # replace diagonals
#' x <- diag(1:4, dim = 3)
#' diag(x) <- c(5,6,7,8)
#' x
#' 
#' @export
#' 
diag <- function(x, dim = 2, value = 1){
  
  if(is.array(x) && length(x)>1){
    
    d <- dim(x)
    n <- length(d)
    
    if(length(unique(d))!=1)
      stop("not a square array")
    
    return(x[1 + 0:(d[1]-1) * sum(d[1]^(n-1:n))])
    
  }
  
  if(length(x)>1){
    
    value <- x
    x     <- length(x)
    
  }
  
  a       <- array(0, dim = rep(x, dim))
  diag(a) <- value
  
  return(a)
  
}



#' @describeIn diag set diagonals.
#' 
#' @export
#' 
"diag<-" <- function(x, value){
  
  x <- as.array(x)
  d <- dim(x)
  n <- length(d)
  
  if(length(unique(d))!=1)
    stop("not a square array")
  
  x[1 + 0:(d[1]-1) * sum(d[1]^(n-1:n))] <- rep(value, length.out = d[1])
  
  return(x)
  
}




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




sf <- function(var, coordinates){
  # check coordinates
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


is.fun <- function(x){
  
  is.fun <- try(is.function(x[[1]]), silent = TRUE)
  
  if(class(is.fun)=='try-error')
    is.fun  <- is.function(x) || (is.array(x) && is.function(x[[1]]))
  
  return(is.fun)
  
}


