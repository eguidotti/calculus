#' Numerical and Symbolic Einstein Summation
#' 
#' Implements the Einstein notation for summation over repeated indices.
#' 
#' @param ... arbitrary number of indexed \code{arrays}. See \code{\link{index}}.
#' @param drop \code{logical}. Drop summation indices? If \code{FALSE}, keep dummy dimensions.
#' 
#' @return \code{array}.
#' 
#' @examples 
#' ### A{i,j} B{j,k}
#' a <- array(letters[1:6], dim = c(i=2, j=3))
#' b <- array(letters[1:3], dim = c(j=3, k=1))
#' einstein(a,b)
#' 
#' ### A{i,j} B{j,k,k} C{k,l} D{j,k}
#' a <- array(1:10, dim = c(i=2, j=5))
#' b <- array(1:45, dim = c(j=5, k=3, k=3))
#' c <- array(1:12, dim = c(k=3, l=4))
#' d <- array(1:15, dim = c(j=5, k=3))
#' einstein(a,b,c,d)
#' 
#' @family tensor algebra
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
einstein <- function(..., drop = TRUE){
  
  a <- list(...)
  a.n <- length(a)
  
  if(a.n==1){
    return(contraction(a[[1]], drop = drop))
  }
  
  x <- contraction(a[[1]], drop = FALSE)
  dummy <- NULL
  
  if(!is.numeric(x))
    if(getOption('calculus.auto.wrap', default = TRUE))
      x <- wrap(x)
  
  for(n in 2:a.n){
    
    y <- contraction(a[[n]], drop = FALSE)
    
    if(!is.numeric(y))
      if(getOption('calculus.auto.wrap', default = TRUE))
        y <- wrap(y)
      
    x.dim <- dim(x)
    y.dim <- dim(y)
    
    x.n.dim <- length(x.dim)
    y.n.dim <- length(y.dim)
    
    i.x <- index(x)
    i.y <- index(y)
    
    if(is.null(i.x) | is.null(i.y))
      stop("no index found: use 'index(x) <- ...' to set summation indices")
    
    j     <- i.x[i.x %in% i.y]
    j     <- j[!is.na(j)]
    dummy <- unique(c(dummy, j))
    
    j.drop <- !(j %in% unlist(lapply(a[-(1:n)], index)))
    
    j.x <- NULL
    j.y <- NULL
    for(i in 1:length(j)){
      j.x[i] <- which(i.x==j[i])
      j.y[i] <- which(i.y==j[i])
    }
    
    if(!all(x.dim[j.x]==y.dim[j.y]))
      stop('non-conformable arrays')
    
    d.x <- 1:x.n.dim
    if(x.n.dim>1)
      x <- aperm(x, perm = c(d.x[!(d.x %in% j.x)], j.x))
    
    is.num <- is.numeric(x) & is.numeric(y)
    
    if(is.num & drop & all(j.drop)){
      
      d.y <- 1:y.n.dim
      if(y.n.dim>1)
        y <- aperm(y, perm = c(j.y, d.y[!(d.y %in% j.y)]))
      
      p <- prod(x.dim[j.x])
      dim(x) <- c(length(x)/p, p)
      dim(y) <- c(p, length(y)/p)
      
      z <- x %*% y

    } else {
      
      d.y <- 1:y.n.dim
      if(y.n.dim>1)
        y <- aperm(y, perm = c(d.y[!(d.y %in% j.y)], j.y))
      
      if(!is.num){
        x <- as.character(x)
        y <- as.character(y)
      }
      
      z <- cpp_einstein(x = x, y = y, dim = x.dim[j.x], drop = (drop & all(j.drop)))

    }
    
    if(length(dummy)==0){
      
      x <- array(z, dim = c(x.dim, y.dim))
      
    }
    else {
      
      z.dim.dummy <- x.dim[j.x]
      
      if(drop & all(j.drop))
        z.dim.dummy <- x.dim[j.x[!j.drop]]  
      
      z.dim.free  <- c(x.dim[-j.x], y.dim[-j.y])
      z.dim       <- c(z.dim.dummy, z.dim.free)
      
      if(length(z.dim)>0)
        x <- array(z, dim = z.dim)
      else 
        x <- c(z)
      
      if(drop & any(!j.drop)) 
        x <- contraction(x, i = j[j.drop])
      
    }
    
  }
  
  if(length(dummy)>0 & !drop){
    
    x.n.dim <- length(dim(x))
    d.x     <- 1:x.n.dim
    
    if(x.n.dim>1){
      
      is.dummy <- (index(x) %in% dummy)
      x <- aperm(x, perm = c(d.x[!is.dummy], d.x[is.dummy])) 
      
    }
    
  }
  
  return(x)
  
}
