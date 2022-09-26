#' Integer Partitions
#' 
#' Provides fast algorithms for generating integer partitions.
#' 
#' @param n positive integer.
#' @param max maximum integer in the partitions.
#' @param length maximum number of elements in the partitions.
#' @param perm logical. Permute partitions?
#' @param fill logical. Fill partitions with zeros to match \code{length}?
#' @param equal logical. Return only partition of \code{n}? If \code{FALSE}, partitions of all integers less or equal to \code{n} are returned.
#' 
#' @return \code{list} of partitions, or \code{matrix} if \code{length>0} and \code{fill=TRUE}.
#' 
#' @examples
#' ### partitions of 4
#' partitions(4)
#' 
#' ### partitions of 4 and permute
#' partitions(4, perm = TRUE)
#' 
#' ### partitions of 4 with max element equal to 2
#' partitions(4, max = 2)
#' 
#' ### partitions of 4 with 2 elements
#' partitions(4, length = 2)
#' 
#' ### partitions of 4 with 3 elements, fill with zeros
#' partitions(4, length = 3, fill = TRUE)
#' 
#' ### partitions of 4 with 2 elements, fill with zeros and permute
#' partitions(4, length = 2, fill = TRUE, perm = TRUE)
#' 
#' ### partitions of all integers less or equal to 3 
#' partitions(3, equal = FALSE) 
#' 
#' ### partitions of all integers less or equal to 3, fill to 2 elements and permute
#' partitions(3, equal = FALSE, length = 2, fill = TRUE, perm = TRUE) 
#' 
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#' 
#' @export
#' 
partitions <- function(n, max = 0, length = 0, perm = FALSE, fill = FALSE, equal = T){
  
  p <- cpp_partitions(n = n, max = max, length = length, perm = perm, fill = fill, equal = equal)
  
  if(fill & length) {
    p <- as.matrix(as.data.frame(p))
    colnames(p) <- NULL
  }
  
  return(p)
  
}