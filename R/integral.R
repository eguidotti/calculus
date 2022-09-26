#' Numerical Integration
#' 
#' Computes the integrals of \code{functions} or \code{characters} in arbitrary 
#' \href{https://en.wikipedia.org/wiki/Orthogonal_coordinates#Table_of_orthogonal_coordinates}{orthogonal coordinate systems}.
#' 
#' @details The function integrates seamlessly with \pkg{cubature} for efficient 
#' numerical integration in \proglang{C}. If the package \pkg{cubature} is not 
#' installed, the function implements a naive Monte Carlo integration by default.
#' For arbitrary orthogonal coordinates \eqn{q_1\dots q_n} the integral is computed as:
#' 
#' \deqn{\int J\cdot f(q_1\dots q_n) dq_1\dots dq_n}
#' 
#' where \eqn{J=\prod_i h_i} is the Jacobian determinant of the transformation 
#' and is equal to the product of the scale factors \eqn{h_1\dots h_n}.
#' 
#' @param f array of \code{characters} or a \code{function} returning a \code{numeric} array.
#' @param bounds \code{list} containing the lower and upper bounds for each variable. If the two bounds coincide, or if a single number is specified, the corresponding variable is not integrated and its value is fixed.
#' @param params \code{list} of additional parameters passed to \code{f}.
#' @param relTol the maximum relative tolerance.
#' @param absTol the absolute tolerance.
#' @param coordinates coordinate system to use. One of: \code{cartesian}, \code{polar}, \code{spherical}, \code{cylindrical}, \code{parabolic}, \code{parabolic-cylindrical} or a character vector of scale factors for each variable.
#' @param method the method to use. One of \code{"mc"}, \code{"hcubature"}, \code{"pcubature"}, \code{"cuhre"}, \code{"divonne"}, \code{"suave"} or \code{"vegas"}. Methods other than \code{"mc"} (naive Monte Carlo) require the \pkg{cubature} package to be installed (efficient integration in C). The defaul uses \code{"hcubature"} if \pkg{cubature} is installed or \code{"mc"} otherwise.
#' @param vectorize \code{logical}. Use vectorization? If \code{TRUE}, it can significantly boost performance but \code{f} needs to handle the vector of inputs appropriately. The default uses \code{FALSE} if \code{f} is a \code{function}, \code{TRUE} otherwise.
#' @param drop if \code{TRUE}, return the integral as a vector and not as an \code{array} for vector-valued functions.
#' @param verbose \code{logical}. Print on progress?
#' @param ... additional arguments passed to \code{\link[cubature]{cubintegrate}}, when method \code{"hcubature"}, \code{"pcubature"}, \code{"cuhre"}, \code{"divonne"}, \code{"suave"} or \code{"vegas"} is used. 
#' 
#' @return list with components
#' \describe{
#'  \item{value}{the final estimate of the integral.}
#'  \item{error}{estimate of the modulus of the absolute error.}
#'  \item{cuba}{\pkg{cubature} output when method \code{"hcubature"}, \code{"pcubature"}, \code{"cuhre"}, \code{"divonne"}, \code{"suave"} or \code{"vegas"} is used.}
#' }
#' 
#' @examples 
#' ### unidimensional integral
#' i <- integral("sin(x)", bounds = list(x = c(0,pi)))
#' i$value
#' 
#' ### multidimensional integral
#' f <- function(x,y) x*y
#' i <- integral(f, bounds = list(x = c(0,1), y = c(0,1)))
#' i$value
#' 
#' ### vector-valued integrals
#' f <- function(x,y) c(x, y, x*y)
#' i <- integral(f, bounds = list(x = c(0,1), y = c(0,1)))
#' i$value
#' 
#' ### tensor-valued integrals
#' f <- function(x,y) array(c(x^2, x*y, x*y, y^2), dim = c(2,2))
#' i <- integral(f, bounds = list(x = c(0,1), y = c(0,1)))
#' i$value
#' 
#' ### area of a circle
#' i <- integral(1, 
#'               bounds = list(r = c(0,1), theta = c(0,2*pi)), 
#'               coordinates = "polar")
#' i$value
#' 
#' ### surface of a sphere
#' i <- integral(1, 
#'               bounds = list(r = 1, theta = c(0,pi), phi = c(0,2*pi)), 
#'               coordinates = "spherical")
#' i$value
#' 
#' ### volume of a sphere
#' i <- integral(1, 
#'          bounds = list(r = c(0,1), theta = c(0,pi), phi = c(0,2*pi)), 
#'          coordinates = "spherical")
#' i$value
#' 
#' @family integrals
#'  
#' @references 
#' Guidotti E (2022). "calculus: High-Dimensional Numerical and Symbolic Calculus in R." Journal of Statistical Software, 104(5), 1-37. \doi{10.18637/jss.v104.i05}
#'      
#' @export
#' 
integral <- function(f, bounds, params = list(), coordinates = "cartesian", relTol = 1e-3, absTol = 1e-12, method = NULL, vectorize = NULL, drop = TRUE, verbose = FALSE, ...){
  
  vol <- 1
  is.fun <- is.function(f)
  if(is.null(vectorize))
    vectorize <- !is.fun
  
  for(i in 1:length(bounds)){
    
    if(length(bounds[[i]])>2)
      stop("bounds must be a list like: list(x = c(lower, upper))")
    
    if(length(bounds[[i]])==2){
      
      if(bounds[[i]][1] == bounds[[i]][2])
        return(0)
      
      if(bounds[[i]][1] > bounds[[i]][2]) {
        
        tmp  <- bounds[[i]][2]
        bounds[[i]][2] <- bounds[[i]][1]
        bounds[[i]][1] <- tmp
        
        vol <- -vol
        
      }
      
      vol <- vol*(bounds[[i]][2]-bounds[[i]][1])
      
    }
    
    if(length(bounds[[i]])==1){
      
      bounds[[i]][2] <- bounds[[i]][1]
      
    }
    
  }
  
  f1 <- f
  var <- names(bounds)
  fixed <- sapply(bounds, function(x) x[1]==x[2])
  
  x.fixed <- NULL
  if(any(fixed))
    x.fixed <- lapply(bounds[fixed], function(x) x[1]) 
  
  x0 <- lapply(bounds, function(x) x[1])
  if(is.fun)
    f.dim <- f.eval(f1, x0, params, dim = TRUE)
  else
    f.dim <- dim(as.array(f1))
  
  H <- NULL
  h <- sf(var = var, coordinates = coordinates)  
  
  if(!is.null(h) & any(!fixed))
      H <- c2e(paste0(h[!fixed], collapse = "*"))
  
  if(!is.null(H)){
    if(!is.fun) 
      f1[] <- sprintf("(%s)*%s", e2c(f1), e2c(H))
    else 
      f1 <- function(...) f(...) * eval(H, envir = list(...))
  }
  
  if(is.character(f1))
    f1 <- c2e(f1)
  
  is.cubature <- requireNamespace("cubature", quietly = TRUE)
  
  if(is.null(method)){
    if(is.cubature)
      method <- "hcubature"
    else 
      method <- "mc"
  }

  if(method=="mc"){
    
    S  <- 0
    V  <- 0
    N  <- 0
    n  <- 1E5
    done <- FALSE
    while(!done){
      
      x <- lapply(bounds[!fixed], function(x) {
        stats::runif(n = n, min = x[1], max = x[2])
      })
      
      if(is.fun) {
        if(vectorize) {
          y <- do.call(f1, args = c(x, x.fixed, params))
          dim(y) <- c(n, prod(f.dim))
        }
        else {
          y <- matrix(apply(as.data.frame(x), 1, function(x){
            do.call(f1, args = c(as.list(x), x.fixed, params))
          }), nrow = n, byrow = TRUE)
        }
      }
      else {
        y <- evaluate(f1, as.data.frame(c(x, x.fixed)), params = params, vectorize = vectorize)
      }
      
      s <- vol*colSums(y)
      if(any(V==0))
        V <- vol^2*apply(y, 2, stats::var)
      
      S <- S + s
      N <- N + n
      n <- min(2*n, 1E7)
      done <- all(sqrt(N*V)/abs(S+.Machine$double.eps)<relTol) | all(sqrt(V/N)<absTol)
      done <- done | all(V==0)
      
      if(verbose){
        
        dec <- ifelse(all(V==0), 1, 1-log(sqrt(min(V[V!=0])/N), base = 10))
        cat(sprintf("n.iter = %s \n", format(N, scientific = TRUE)))
        cat(sprintf("   value = %s; error = %s \n", 
                    format(S/N, digits = dec, nsmall = dec), 
                    format(sqrt(V/N), digits = 2, scientific = TRUE)))
    
      }
        
    }
    
    value <- S/N
    error <- sqrt(V/N)
    cuba <- NULL
    
  }
  else if(is.cubature){
    
    f2 <- function(x){
      
      if(is.null(dim(x)))
        dim(x) <- c(length(x), 1)
      
      n <- ncol(x)
      
      x <- split(x, row(x))
      names(x) <- var[!fixed]
      x <- c(x, x.fixed)
      
      if(is.fun)
        return(matrix(do.call(f1, args = c(x, params)), ncol = n, byrow = TRUE))
      
      return(t(evaluate(f1, as.data.frame(x), params = params, vectorize = vectorize)))
      
    }
    
    lower <- sapply(bounds[!fixed], function(x) x[1])
    upper <- sapply(bounds[!fixed], function(x) x[2])
    
    cuba  <- cubature::cubintegrate(f = f2, fDim = prod(f.dim), lower = lower, upper = upper, method = method, relTol = relTol, absTol = absTol, nVec = ifelse(vectorize, 1024, 1), ...)
    value <- sign(vol)*cuba$integral
    error <- cuba$error
    
  }
  else {
    
    stop('methods "hcubature", "pcubature", "cuhre", "divonne", "suave" and "vegas" require the "cubature" package to be installed (efficient integration in C).\n\n Install cubature -> install.packages("cubature")\n')
    
  }
  
  if(length(f.dim)>1 | !drop){
    dim(value) <- f.dim
    dim(error) <- f.dim
  }
  
  return(list(value = value, error = error, cuba = cuba))
  
}
