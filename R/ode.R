#' Ordinary Differential Equations
#' 
#' Solves a numerical or symbolic system of ordinary differential equations.
#' 
#' @param f vector of \code{characters}, or a \code{function} returning a numeric vector, giving the values of the derivatives in the ODE system at time \code{timevar}. See examples.
#' @param var vector giving the initial conditions. See examples.
#' @param times discretization sequence, the first value represents the initial time.
#' @param timevar the time variable used by \code{f}, if any. 
#' @param params \code{list} of additional parameters passed to \code{f}.
#' @param method the solver to use. One of \code{"rk4"} (Runge-Kutta) or \code{"euler"} (Euler).
#' @param drop if \code{TRUE}, return only the final solution instead of the whole trajectory.
#' 
#' @return Vector of final solutions if \code{drop=TRUE}, otherwise a \code{matrix} with as many 
#' rows as elements in \code{times} and as many columns as elements in \code{var}.
#' 
#' @examples 
#' ## ==================================================
#' ## Example: symbolic system 
#' ## System:  dx = x dt
#' ## Initial: x0 = 1
#' ## ==================================================
#' f <- "x"
#' var <- c(x=1)
#' times <- seq(0, 2*pi, by=0.001)
#' x <- ode(f, var, times)
#' plot(times, x, type = "l")
#' 
#' ## ==================================================
#' ## Example: time dependent system
#' ## System:  dx = cos(t) dt
#' ## Initial: x0 = 0
#' ## ==================================================
#' f <- "cos(t)"
#' var <- c(x=0)
#' times <- seq(0, 2*pi, by=0.001)
#' x <- ode(f, var, times, timevar = "t")
#' plot(times, x, type = "l")
#' 
#' ## ==================================================
#' ## Example: multivariate time dependent system
#' ## System:  dx = x dt 
#' ##          dy = x*(1+cos(10*t)) dt
#' ## Initial: x0 = 1
#' ##          y0 = 1
#' ## ==================================================
#' f <- c("x", "x*(1+cos(10*t))")
#' var <- c(x=1, y=1)
#' times <- seq(0, 2*pi, by=0.001)
#' x <- ode(f, var, times, timevar = "t")
#' matplot(times, x, type = "l", lty = 1, col = 1:2)
#' 
#' ## ==================================================
#' ## Example: numerical system
#' ## System:  dx = x dt 
#' ##          dy = y dt 
#' ## Initial: x0 = 1
#' ##          y0 = 2
#' ## ==================================================
#' f <- function(x, y) c(x, y)
#' var <- c(x=1, y=2)
#' times <- seq(0, 2*pi, by=0.001)
#' x <- ode(f, var, times)
#' matplot(times, x, type = "l", lty = 1, col = 1:2)
#' 
#' ## ==================================================
#' ## Example: vectorized interface
#' ## System:  dx = x dt 
#' ##          dy = y dt 
#' ##          dz = y*(1+cos(10*t)) dt  
#' ## Initial: x0 = 1
#' ##          y0 = 2
#' ##          z0 = 2
#' ## ==================================================
#' f <- function(x, t) c(x[1], x[2], x[2]*(1+cos(10*t)))
#' var <- c(1,2,2)
#' times <- seq(0, 2*pi, by=0.001)
#' x <- ode(f, var, times, timevar = "t")
#' matplot(times, x, type = "l", lty = 1, col = 1:3)
#' 
#' @family integrals
#' 
#' @references 
#' Guidotti, E. (2020). "calculus: High dimensional numerical and symbolic calculus in R". \url{https://arxiv.org/abs/2101.00086}
#' 
#' @export
#' 
ode <- function(f, var, times, timevar = NULL, params = list(), method = "rk4", drop = FALSE){
  
  is.named <- !is.null(names(var))
  is.fun <- is.function(f)
  is.t <- !is.null(timevar) 
  h <- diff(times)
  n <- length(h)
  
  if(!is.fun){
    f <- e2c(f)
    f <- sprintf("c(%s)", paste(f, collapse = ","))
    f <- c2e(f)
  }
    
  if(!drop){
    m <- matrix(nrow = n+1, ncol = length(var), dimnames = list(times, names(var)))
    m[1,] <- var
  }
  
  if(is.t)
    names(times) <- rep(timevar, n+1)
  
  if(method=="euler"){
    if(is.fun){
      if(!is.named){
        if(!is.t){
          for(i in 1:n){
            var <- var + h[i] * do.call(f, c(list(var), params))
            if(!drop) m[i+1,] <- var
          }
        } 
        else{
          for(i in 1:n){
            var <- var + h[i] * do.call(f, c(list(var), as.list(times[i]), params))
            if(!drop) m[i+1,] <- var
          }
        }
      } 
      else {
        if(!is.t){
          for(i in 1:n){
            var <- var + h[i] * do.call(f, c(as.list(var), params))
            if(!drop) m[i+1,] <- var
          }
        }
        else{
          for(i in 1:n){
            var <- var + h[i] * do.call(f, c(as.list(c(var, times[i])), params)) 
            if(!drop) m[i+1,] <- var
          }
        }
      }
    } else {
      env <- list2env(params, hash = TRUE)
      if(!is.t){
        for(i in 1:n){
          list2env(as.list(var), envir = env)
          var <- var + h[i] * eval(f, envir = env)
          if(!drop) m[i+1,] <- var
        }
      }
      else{
        for(i in 1:n){
          list2env(as.list(c(var, times[i])), envir = env)
          var <- var + h[i] * eval(f, envir = env)
          if(!drop) m[i+1,] <- var
        }
      }
    }
  } 
  else if(method=="rk4") {
    if(is.fun){
      if(!is.named){
        if(!is.t){
          for(i in 1:n){
            k1 <- do.call(f, c(list(var), params))
            k2 <- do.call(f, c(list(var+h[i]*k1/2), params))
            k3 <- do.call(f, c(list(var+h[i]*k2/2), params))
            k4 <- do.call(f, c(list(var+h[i]*k3), params))
            var <- var + (h[i]*(k1+2*k2+2*k3+k4))/6
            if(!drop) m[i+1,] <- var
          }
        }
        else{
          for(i in 1:n){
            k1 <- do.call(f, c(list(var), as.list(times[i]), params))
            k2 <- do.call(f, c(list(var+h[i]*k1/2), as.list(times[i]+h[i]/2), params))
            k3 <- do.call(f, c(list(var+h[i]*k2/2), as.list(times[i]+h[i]/2), params))
            k4 <- do.call(f, c(list(var+h[i]*k3), as.list(times[i]+h[i]), params))
            var <- var + (h[i]*(k1+2*k2+2*k3+k4))/6
            if(!drop) m[i+1,] <- var
          }
        }
      } 
      else {
        if(!is.t){
          for(i in 1:n){
            k1 <- do.call(f, c(as.list(var), params))
            k2 <- do.call(f, c(as.list(var+h[i]*k1/2), params))
            k3 <- do.call(f, c(as.list(var+h[i]*k2/2), params))
            k4 <- do.call(f, c(as.list(var+h[i]*k3), params))
            var <- var + (h[i]*(k1+2*k2+2*k3+k4))/6
            if(!drop) m[i+1,] <- var
          }
        }
        else{
          for(i in 1:n){
            k1 <- do.call(f, c(as.list(c(times[i], var)), params))
            k2 <- do.call(f, c(as.list(c(times[i]+h[i]/2, var+h[i]*k1/2)), params))
            k3 <- do.call(f, c(as.list(c(times[i]+h[i]/2, var+h[i]*k2/2)), params))
            k4 <- do.call(f, c(as.list(c(times[i]+h[i], var+h[i]*k3)), params))
            var <- var + (h[i]*(k1+2*k2+2*k3+k4))/6
            if(!drop) m[i+1,] <- var
          }
        }
      }
    } 
    else {
      env1 <- list2env(params, hash = TRUE)
      env2 <- list2env(params, hash = TRUE)
      env3 <- list2env(params, hash = TRUE)
      env4 <- list2env(params, hash = TRUE)
      if(!is.t){
        for(i in 1:n){
          list2env(as.list(var), envir = env1)
          k1 <- eval(f, envir = env1)
          list2env(as.list(var+h[i]*k1/2), envir = env2)
          k2 <- eval(f, envir = env2)
          list2env(as.list(var+h[i]*k2/2), envir = env3)
          k3 <- eval(f, envir = env3)
          list2env(as.list(var+h[i]*k3), envir = env4)
          k4 <- eval(f, envir = env4)
          var <- var + (h[i]*(k1+2*k2+2*k3+k4))/6
          if(!drop) m[i+1,] <- var
        }
      }
      else{
        for(i in 1:n){
          list2env(as.list(c(times[i], var)), envir = env1)
          k1 <- eval(f, envir = env1)
          list2env(as.list(c(times[i]+h[i]/2, var+h[i]*k1/2)), envir = env2)
          k2 <- eval(f, envir = env2)
          list2env(as.list(c(times[i]+h[i]/2, var+h[i]*k2/2)), envir = env3)
          k3 <- eval(f, envir = env3)
          list2env(as.list(c(times[i]+h[i], var+h[i]*k3)), envir = env4)
          k4 <- eval(f, envir = env4)
          var <- var + (h[i]*(k1+2*k2+2*k3+k4))/6
          if(!drop) m[i+1,] <- var
        }
      }
    }
  } else {
    stop("method not supported.")
  }
  
  if(!drop)
    return(m)
  
  return(var)

}
