#' Numerical Evaluation
#' 
#' Evaluate an array of characters, expressions or functions.
#' 
#' @param x an object to be evaluated: array of characters, expressions or functions.
#' @param envir the \code{\link[base]{environment}} in which \code{x} is to be evaluated. May also be \code{NULL}, a list, a data frame, a pairlist or an integer as specified to \code{\link[base]{sys.call}}.
#' @param enclos relevant when \code{envir} is a (pair)list or a data frame. Specifies the enclosure, i.e., where \code{R} looks for objects not found in \code{envir}. This can be \code{NULL} (interpreted as the base package environment, \code{\link[base]{baseenv}()}) or an environment.
#' @param simplify logical. Simplify the output? If \code{FALSE}, return a \code{list}.
#' 
#' @return evaluated object.
#' 
#' @examples 
#' ##################################
#' # Evaluate an array of characters
#' #
#' 
#' x <- array(letters[1:4], dim = c(2,2))
#' 
#' e <- list(a = 1, b = 2, c = 3, d = 4)
#' evaluate(x, env = e)
#' evaluate(x, env = e, simplify = FALSE)
#' 
#' e <- list(a = 1:3, b = 2, c = 3, d = 4)
#' evaluate(x, env = e)
#' evaluate(x, env = e, simplify = FALSE)
#'
#'
#' ##################################
#' # Evaluate an array of functions
#' #
#' 
#' f1 <- function(x,y) sin(x)
#' f2 <- function(x,y) sin(y)
#' f3 <- function(x,y) x*y
#' x <- array(c(f1,f3,f3,f2), dim = c(2,2))
#' 
#' e <- list(x = 0, y = pi/2)
#' evaluate(x, env = e)
#' evaluate(x, env = e, simplify = FALSE)
#' 
#' e <- list(x = c(0, pi/2), y = c(0, pi/2))
#' evaluate(x, env = e)
#' evaluate(x, env = e, simplify = FALSE)
#'   
#' @export
#' 
evaluate <- function(x, envir = parent.frame(), enclos = if(is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv(), simplify = TRUE){
  
  if(is.character(x))
    x <- c2e(x)
  
  is.fun <- is.fun(x)
  if(!is.array(x) && (is.fun || is.expression(x) || is.call(x) || is.symbol(x)))
    x <- array(c(x))
  
  x <- as.array(x)
  d <- dim(x)
  
  if(!is.fun) {
    
    x <- sapply(x, function(x){
        eval(expr = x, envir = envir, enclos = enclos)
    }, simplify = FALSE, USE.NAMES = FALSE)  
    
  }
  else {
    
    x <- sapply(x, function(x){
        do.call(x, args = envir)
    }, simplify = FALSE, USE.NAMES = FALSE)
    
  }
  
  
  x <- as.matrix(as.data.frame(x))
  colnames(x) <- NULL
  
  if(nrow(x)==1 && simplify)
    return(array(unlist(x), dim = d))
  
  if(!simplify) {
    x <- lapply(1:nrow(x), function(i){
      array(unlist(x[i,]), dim = d)
    })  
  }
  
  return(x)
}
