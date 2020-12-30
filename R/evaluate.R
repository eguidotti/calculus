#' Evaluate Characters and Expressions
#' 
#' Evaluates an array of \code{characters} or \code{expressions}.
#' 
#' @param f array of \code{characters} or \code{expressions} to be evaluated.
#' @param var named vector or \code{data.frame} in which \code{f} is to be evaluated.
#' 
#' @return Evaluated object. When \code{var} is a named vector, the return is an array 
#' with the same dimensions of \code{f}. When \code{var} is a \code{data.frame}, the
#' return is a \code{matrix} with columns corresponding to the entries of \code{f} and 
#' rows corresponding to the rows of \code{var}.
#' 
#' @examples 
#' ### single evaluation
#' f <- array(letters[1:4], dim = c(2,2))
#' var <- c(a = 1, b = 2, c = 3, d = 4)
#' evaluate(f, var)
#' 
#' ### vectorized evaluation
#' f <- array(letters[1:4], dim = c(2,2))
#' var <- data.frame(a = 1:3, b = 2:4, c = 3:5, d = 4:6)
#' evaluate(f, var)
#' 
#' @family utilities
#' 
#' @export
#' 
evaluate <- function(f, var){
  
  is_df <- is.data.frame(var)
  is_num <- is.vector(var, mode = "numeric")
  
  if(!is_num & !is_df)
    stop("var must be a named vector or data.frame")
  
  if(is_num)
    var <- as.list(var)
  
  if(is.expression(f))
    f <- e2c(f)
  
  d <- dim(f)
  n <- length(f)
  
  f <- c2e(sprintf("c(%s)", paste(f, collapse = ",")))
  f <- eval(f, envir = var, enclos = baseenv())

  if(is_num)
    dim(f) <- d
  else
    dim(f) <- c(length(f)/n, n)
   
  return(f)
}
