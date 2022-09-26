#' Numerical and Symbolic Tensor Contraction
#' 
#' Sums over repeated indices in an \code{array}.
#' 
#' @param x indexed \code{array}. See \code{\link{index}}.
#' @param i subset of repeated indices to sum up. If \code{NULL}, the summation takes place on all the repeated indices. 
#' @param drop \code{logical}. Drop summation indices? If \code{FALSE}, keep dummy dimensions.
#' 
#' @return \code{array}.
#' 
#' @examples 
#' ### matrix trace
#' x <- matrix(letters[1:4], nrow = 2)
#' contraction(x)
#' 
#' ### tensor trace
#' x <- array(1:27, dim = c(3,3,3))
#' contraction(x)
#' 
#' #### tensor contraction over repeated indices
#' x <- array(1:27, dim = c(3,3,3))
#' index(x) <- c("i","i","j")
#' contraction(x)
#' 
#' #### tensor contraction over specific repeated indices only
#' x <- array(1:16, dim = c(2,2,2,2))
#' index(x) <- c("i","i","k","k")
#' contraction(x, i = "k")
#' 
#' #### tensor contraction keeping dummy dimensions
#' x <- array(letters[1:16], dim = c(2,2,2,2))
#' index(x) <- c("i","i","k","k")
#' contraction(x, drop = FALSE)
#' 
#' @family tensor algebra
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
contraction <- function(x, i = NULL, drop = TRUE){
  
  x <- as.array(x)
  j <- i  
  
  if(is.null(j)){
   
    i.x <- index(x)
    
    if(is.null(i.x)){
      i.x      <- rep('i', length(dim(x)))
      index(x) <- i.x
    }
    
    j <- unique(i.x[duplicated(i.x)])
    j <- j[!is.na(j)]
    
  }
  
  if(length(j)>0) for(jj in j) {
    
    x.dim   <- dim(x)
    x.n.dim <- length(x.dim)
    i.x     <- index(x)
    
    ii <- which(i.x==jj)
    
    if(length(unique(x.dim[ii]))!=1)
      stop('non-conformable dimensions')
    
    d.x <- 1:x.n.dim
    if(x.n.dim>1)
      x <- aperm(x, perm = c(d.x[!(d.x %in% ii)], ii))
    
    x <- cpp_trace(x, x.dim[ii], drop = drop)
    
    if(drop)
      x.dim <- x.dim[-ii]
    else
      x.dim <- c(x.dim[ii][1], x.dim[-ii])
    
    x.n.dim <- length(x.dim)
    
    if(x.n.dim>0) 
      x <- array(x, dim = x.dim)
    
    if(!drop && x.n.dim>1){
      x <- aperm(x, perm = c(2:x.n.dim, 1))
    }
      
  }  
  
  return(x)
}
