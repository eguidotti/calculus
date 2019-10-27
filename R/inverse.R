#' Numeric and Symbolic Matrix Inverse
#' 
#' Calculates the inverse of a matrix.
#' 
#' @param x numeric or character matrix. 
#' 
#' @return numeric or character matrix. 
#' 
#' @examples 
#' 
#' # numeric matrix
#' x <- matrix(1:4, nrow = 2)
#' inverse(x)
#' 
#' # symbolic matrix
#' x <- matrix(letters[1:4], nrow = 2)
#' inverse(x)
#' 
#' @export
#' 
inverse <- function(x) {

  x <- as.matrix(x)
  if(is.numeric(x))
    return(base::solve(x))
  
  x.dim   <- dim(x)
  x.n.dim <- length(x.dim)
  
  if(x.n.dim!=2)
    stop("not a matrix")
  
  n <- unique(x.dim)
  if(length(n)!=1)
    stop("not a square matrix")
  
  det <- det(x)
  if(det=="0")
    stop("not invertible")
  if(n==1)
    return(array(sprintf("1/%s", det), dim = c(1,1)))
  
  ij <- 1:n
  ij <- expand.grid(list(ij, ij))
    
  adj <- apply(ij, 1, function(ij){
    det(x[-ij[2],-ij[1]])
  })
  
  odd <- rowSums(ij) %% 2 == 1
  adj[odd]  <- cpp_paste('-1', wrap(adj[odd]), sep = " * ")
  adj[!odd] <- wrap(adj[!odd])
  
  return(array(cpp_paste(adj, wrap(det), sep = " / "), dim = rep(n,2)))

}


