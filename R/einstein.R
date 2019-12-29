#' Numerical and Symbolic Einstein Summation
#' 
#' Implements the Einstein notation for summation over repeated indices.
#' 
#' @param ... arbitrary number of indexed arrays.
#' @param drop logical. Drop summation indices? If \code{FALSE}, keep dummy dimensions.
#' 
#' @return array.
#' 
#' @seealso \code{\link{index}}, \code{\link{trace}}
#' 
#' @examples 
#' ##################################
#' # A{i,j} B{j,k,k} C{k,l} D{j,k}
#' #
#' 
#' a <- array(1:10, dim = c(2,5))
#' b <- array(1:45, dim = c(5,3,3))
#' c <- array(1:12, dim = c(3,4))
#' d <- array(1:15, dim = c(5,3))
#' 
#' index(a) <- c('i','j')
#' index(b) <- c('j','k','k')
#' index(c) <- c('k', 'l')
#' index(d) <- c('j', 'k')
#' 
#' einstein(a,b,c,d)
#' 
#' 
#' ##################################
#' # A{i,j} B{j,k}
#' #
#' 
#' a <- array(letters[1:6], dim = c(2,3))
#' b <- array(letters[1:12], dim = c(3,4))
#' 
#' index(a) <- c('i','j')
#' index(b) <- c('j','k')
#' 
#' einstein(a,b)
#' 
#' @export
#' 
einstein <- function(..., drop = TRUE){
  
  a     <- list(...)
  a.n   <- length(a)
  
  if(a.n==1){
    return(trace(a[[1]], drop = drop))
  }
  
  x     <- trace(a[[1]], drop = FALSE)
  dummy <- NULL
  
  if(!is.numeric(x))
     if(getOption('calculus.auto.wrap', default = TRUE))
       x <- wrap(x)
  
  for(n in 2:a.n){
    
    y <- trace(a[[n]], drop = FALSE)
    
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
    
    d.y <- 1:y.n.dim
    if(y.n.dim>1)
      y <- aperm(y, perm = c(d.y[!(d.y %in% j.y)], j.y))
    
    
    if(is.numeric(x) && is.numeric(y)){
      
      z <- cpp_einstein(x = x, y = y, dim = x.dim[j.x], drop = (drop && all(j.drop)))
      
    } else {
      
      if(getOption('calculus.auto.wrap', default = TRUE))
        y <- wrap(y)
      
      z <- cpp_einstein(x = as.character(x), y = as.character(y), dim = x.dim[j.x], drop = (drop && all(j.drop)))
      
    }
    
    if(length(dummy)==0){
      
      x <- array(z, dim = c(x.dim, y.dim))
    
    }
    else {
    
      z.dim.dummy <- x.dim[j.x]
      
      if(drop && all(j.drop))
        z.dim.dummy <- x.dim[j.x[!j.drop]]  

      z.dim.free  <- c(x.dim[-j.x], y.dim[-j.y])
      z.dim       <- c(z.dim.dummy, z.dim.free)
      
      if(length(z.dim)>0)
        x <- array(z, dim = z.dim)
      else 
        x <- z
      
      if(drop && any(!j.drop)) 
        x <- trace(x, i = j[j.drop])
      
    }
    
  }
  
  if(length(dummy)>0 && !drop){
    
    x.n.dim <- length(dim(x))
    d.x     <- 1:x.n.dim
    
    if(x.n.dim>1){
      
      is.dummy <- (index(x) %in% dummy)
      x <- aperm(x, perm = c(d.x[!is.dummy], d.x[is.dummy])) 
      
    }
    
  }
  
  return(x)

}


