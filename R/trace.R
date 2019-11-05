#' Tensor Contraction
#' 
#' Sums over repeated indices in a tensor. Can be seen as a generalization of the trace.
#' 
#' @param x array.
#' @param i subset of repeated indices to sum up. If \code{NULL}, the tensor contraction takes place on all repeated indices of \code{x}. 
#' @param drop logical. Drop summation indices? If \code{FALSE}, keep dummy dimensions.
#' 
#' @return array.
#' 
#' @examples 
#' # trace of numeric matrix
#' x <- matrix(1:4, nrow = 2)
#' trace(x)
#' 
#' # trace of character matrix
#' x <- matrix(letters[1:4], nrow = 2)
#' trace(x)
#' 
#' # trace of a tensor (sum over diagonals)
#' x <- array(1:27, dim = c(3,3,3))
#' trace(x)
#' 
#' # tensor contraction over repeated indices
#' x <- array(1:27, dim = c(3,3,3))
#' index(x) <- c('i','i','j')
#' trace(x)
#' 
#' # tensor contraction over specific indices only
#' x <- array(1:16, dim = c(2,2,2,2))
#' index(x) <- c('i','i','k','k')
#' trace(x, i = 'k')
#' 
#' # tensor contraction keeping dummy dimensions
#' x <- array(letters[1:16], dim = c(2,2,2,2))
#' index(x) <- c('i','i','k','k')
#' trace(x, drop = FALSE)
#' 
#' @seealso \code{\link{index}}, \code{\link{einstein}}
#' 
#' @export
#' 
trace <- function(x, i = NULL, drop = TRUE){
  
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



