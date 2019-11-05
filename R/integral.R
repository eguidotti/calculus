#' Monte Carlo Integration
#' 
#' Integrates multidimensional functions, expressions, and characters in arbitrary \href{https://en.wikipedia.org/wiki/Orthogonal_coordinates#Table_of_orthogonal_coordinates}{orthogonal coordinate systems}.
#' 
#' @param f function, expression or characters.
#' @param ... integration bounds.
#' @param err acuracy requested.
#' @param rel logical. Relative accuracy? If \code{FALSE}, use absolute accuracy.
#' @param coordinates coordinate system to use. One of: \code{cartesian}, \code{polar}, \code{spherical}, \code{cylindrical}, \code{parabolic}, \code{parabolic-cylindrical} or a character vector of scale factors for each varibale.
#' @param verbose logical. Print on progress?
#' 
#' @return list with components
#' \describe{
#'  \item{value}{the final estimate of the integral.}
#'  \item{abs.error}{estimate of the modulus of the absolute error.}
#' }
#' 
#' @examples 
#' # integrate character
#' integral('sin(x)', x = c(0,2*pi), rel = FALSE)
#' 
#' # integrate expression
#' integral(parse(text = 'x'), x = c(0,1))
#' 
#' # integrate function
#' integral(function(x) exp(x), x = c(0,1))
#' 
#' # multivariate integral
#' integral(function(x,y) x*y, x = c(0,1), y = c(0,1))
#' 
#' # surface of a sphere
#' integral('1', r = 1, theta = c(0,pi), phi = c(0,2*pi), coordinates = 'spherical')
#' 
#' # volume of a sphere
#' integral('1', r = c(0,1), theta = c(0,pi), phi = c(0,2*pi), coordinates = 'spherical')
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
#' integral(E[1], r = 1, theta = c(0,pi), phi = c(0,2*pi), coordinates = 'spherical')
#' 
#' 
#' @export
#' 
integral <- function(f, ..., err = 0.01, rel = TRUE, coordinates = 'cartesian', verbose = TRUE){
  
  vol    <- 1
  bounds <- list(...)
  is.fun <- is.fun(f)
  
  for(i in 1:length(bounds)){
    
    if(length(bounds[[i]])>2)
      stop("bounds must be of type c(lower, upper)")
    
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
  
  H <- NULL
  h <- sf(var = names(bounds), coordinates = coordinates)  
  
  if(!is.null(h)){
    
    i.var <- sapply(bounds, function(x) x[1]!=x[2])
    
    if(any(i.var))
      H <- paste0(h[i.var], collapse = "*")
    
  }
  
  
  if(!is.null(H) && !is.fun){
    f[] <- sprintf("(%s)*%s", e2c(f), H)
    f  <- c2e(f)
  }
    
  S  <- 0
  V  <- 0
  N  <- 0
  n  <- 1E5
  done <- FALSE
  while(!done){
    
    x <- lapply(bounds, function(x) stats::runif(n = n, min = x[1], max = x[2]))
    y <- evaluate(f, x)
    
    if(!is.null(H) && is.fun)
      y <- y * evaluate(H, envir = x)
    
    s <- vol*colSums(y)
    
    if(any(V==0))
      V <- vol^2*apply(y, 2, stats::var)
    
    S <- S + s
    N <- N + n
    n <- min(2*n, 1E7)
    done <- ifelse(rel, all(sqrt(N*V)/abs(S+.Machine$double.eps)<err), all(sqrt(V/N)<err))
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
  
  return(list(value = value, abs.error = abs.error))
  
}