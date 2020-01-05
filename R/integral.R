#' Numerical Integration
#' 
#' Integrates multidimensional functions, expressions, and characters in arbitrary \href{https://en.wikipedia.org/wiki/Orthogonal_coordinates#Table_of_orthogonal_coordinates}{orthogonal coordinate systems}.
#' 
#' @param f function, expression or character.
#' @param bounds list of integration bounds.
#' @param relTol relative accuracy requested.
#' @param absTol absolute accuracy requested.
#' @param coordinates coordinate system to use. One of: \code{cartesian}, \code{polar}, \code{spherical}, \code{cylindrical}, \code{parabolic}, \code{parabolic-cylindrical} or a character vector of scale factors for each varibale.
#' @param method the method to use. Should be one of \code{"mc"}, \code{"hcubature"}, \code{"pcubature"}, \code{"cuhre"}, \code{"divonne"}, \code{"suave"} or \code{"vegas"}. Naive Monte Carlo integration by default. The additional methods require the cubature package to be installed (efficient integration in C).
#' @param verbose logical. Print on progress?
#' @param ... additional arguments passed to the \code{\link[cubature]{cubintegrate}} function, when method \code{"hcubature"}, \code{"pcubature"}, \code{"cuhre"}, \code{"divonne"}, \code{"suave"} or \code{"vegas"} is used. 
#' 
#' @return list with components
#' \describe{
#'  \item{value}{the final estimate of the integral.}
#'  \item{abs.error}{estimate of the modulus of the absolute error.}
#'  \item{cuba}{cubature output when method \code{"hcubature"}, \code{"pcubature"}, \code{"cuhre"}, \code{"divonne"}, \code{"suave"} or \code{"vegas"} is used.}
#' }
#' 
#' @examples 
#' # integrate character
#' integral('sin(x)', bounds = list(x = c(0,2*pi)))
#' 
#' # integrate expression
#' integral(parse(text = 'x'), bounds = list(x = c(0,1)))
#' 
#' # integrate function
#' integral(function(x) exp(x), bounds = list(x = c(0,1)))
#' 
#' # multivariate integral
#' integral(function(x,y) x*y, bounds = list(x = c(0,1), y = c(0,1)))
#' 
#' # surface of a sphere
#' integral('1', 
#'          bounds = list(r = 1, theta = c(0,pi), phi = c(0,2*pi)), 
#'          coordinates = 'spherical')
#' 
#' # volume of a sphere
#' integral('1', 
#'          bounds = list(r = c(0,1), theta = c(0,pi), phi = c(0,2*pi)), 
#'          coordinates = 'spherical')
#'          
#' \dontrun{
#' # efficient integration in C (requires the cubature package to be installed)
#' integral('1',
#'         bounds = list(r = c(0,1), theta = c(0,pi), phi = c(0,2*pi)),
#'         coordinates = 'spherical',
#'         method = 'cuhre',
#'         relTol = 1e-06,
#'         absTol = 1e-12)
#' }
#' 
#' ##################################
#' # Electric charge contained in a region of space
#' # (see divergence theorem and Maxwell's equations)
#' # 
#' 
#' # electric potential of unitary point charge 
#' V <- '1/(4*pi*r)'
#' 
#' # electric field
#' E <- -1 %prod% gradient(V, c('r', 'theta', 'phi'), coordinates = 'spherical')
#' 
#' # electric charge
#' integral(E[1], 
#'          bounds = list(r = 1, theta = c(0,pi), phi = c(0,2*pi)), 
#'          coordinates = 'spherical')
#' 
#' 
#' @export
#' 
integral <- function(f, bounds, relTol = 1e-02, absTol = 1e-03, coordinates = "cartesian", method = "mc", verbose = TRUE, ...){
  
  vol    <- 1
  is.fun <- is.fun(f)
  
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
  
  f1    <- f
  var   <- names(bounds)
  fixed <- sapply(bounds, function(x) x[1]==x[2])
  
  H <- NULL
  h <- sf(var = var, coordinates = coordinates)  
  
  if(!is.null(h) & any(!fixed))
      H <- paste0(h[!fixed], collapse = "*")
  
  if(!is.null(H)){
    
    if(!is.fun) {
      
      f1[] <- sprintf("(%s)*%s", e2c(f1), H)
      f1   <- c2e(f1)
      
    }
    else {
      
      f1 <- function(...){
        f(...) * evaluate(H, envir = list(...))
      }
      
    }
    
  }
    
  if(method=="mc"){
    
    S  <- 0
    V  <- 0
    N  <- 0
    n  <- 1E5
    done <- FALSE
    while(!done){
      
      x <- lapply(bounds, function(x) stats::runif(n = n, min = x[1], max = x[2]))
      y <- evaluate(f1, x)
      
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
        cat(sprintf("   value = %s; abs.error = %s \n", 
                    format(S/N, digits = dec, nsmall = dec), 
                    format(sqrt(V/N), digits = 2, scientific = TRUE)))
    
      }
        
    }
    
    value     <- S/N
    abs.error <- sqrt(V/N)
    cuba      <- NULL
    
  }
  else if(requireNamespace("cubature", quietly = TRUE)){
    
    if(any(fixed))
      x.fixed <- lapply(bounds[fixed], function(x) x[1]) 
    
    f2 <- function(x){
      
      names(x) <- var[!fixed]
      x <- as.list(x)
      
      if(any(fixed))
        x <- c(x, x.fixed)
      
      if(is.fun)
        return(do.call(f1, args = x))
      
      return(eval(f1, envir = x))
      
    }
    
    lower <- sapply(bounds[!fixed], function(x) x[1])
    upper <- sapply(bounds[!fixed], function(x) x[2])
    
    cuba  <- cubature::cubintegrate(f = f2, lower = lower, upper = upper, method = method, relTol = relTol, absTol = absTol, ...)
    value <- sign(vol)*cuba$integral
    
    if(method %in% c("hcubature","pcubature"))
      abs.error <- cuba$error*abs(cuba$integral)
    else 
      abs.error <- cuba$error
    
  }
  else {
    
    stop('methods "hcubature", "pcubature", "cuhre", "divonne", "suave" and "vegas" require the "cubature" package to be installed (efficient integration in C).\n\n Install cubature -> install.packages("cubature")\n')
    
  }
  
  return(list(value = value, abs.error = abs.error, cuba = cuba))
  
}