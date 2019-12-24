# High Dimensional Numerical and Symbolic Calculus in R

![](https://www.r-pkg.org/badges/version/calculus) ![](https://www.r-pkg.org/badges/last-release/calculus) ![](https://cranlogs.r-pkg.org/badges/grand-total/calculus)

Efficient C++ optimized functions for numerical and symbolic calculus. It includes basic symbolic arithmetic, tensor calculus, Einstein summing convention, fast computation of the Levi-Civita symbol and generalized Kronecker delta, Taylor series expansion, multivariate Hermite polynomials, accurate high-order derivatives, differential operators (Gradient, Jacobian, Hessian, Divergence, Curl, Laplacian) and Monte Carlo integration in arbitrary orthogonal coordinate systems: cartesian, polar, spherical, cylindrical, parabolic or user defined by custom scale factors.

## Quickstart

```R
# Install calculus
install.packages('calculus')

# Load calculus
require('calculus')
```
## Usage


### `derivative`: Numerical and Symbolic Derivatives

__Description__


Compute symbolic derivatives based on the `D` function, or accurate and reliable numerical derivatives based on finite differences.


__Usage__

```r
derivative(f, var = "x", order = 1, accuracy = 2, stepsize = NULL, deparse = TRUE)
```


__Arguments__

Argument      |Description
------------- |----------------
```f```     |     function, expression or character array.
```var```     |     character vector, giving the variable names with respect to which derivatives will be computed. If a named vector is provided, derivatives will be computed at that point. See examples.
```order```     |     integer vector, giving the differentiation order for each variable. See details.
```accuracy```     |     accuracy degree for numerical derivatives.
```stepsize```     |     finite differences stepsize for numerical derivatives. Auto-optimized by default.
```deparse```     |     logical. Return character instead of expression or call?

__Details__


The function behaves differently depending on the length of the `order` argument.

If `order` is of length 1, then the n-th order derivative is computed for each function with respect to each
variable:

<p align="center"><img src="http://1.618034.com/blog_data/math/formula.58010.png" height="30"></p>

where _F_ is the tensor of functions and <img src="http://1.618034.com/blog_data/math/formula.58012.png" height="16"> is the tensor of variable names with respect to which the _n_-th order derivatives will be computed.

If `order` matches the length of `var` , then it is assumed that the differentiation order is provided
for each variable. In this case, each function will be derived _n<sub>i</sub>_ times with respect to the _i_-th variable,
for each of the _j_ variables:

<p align="center"><img src="http://1.618034.com/blog_data/math/formula.58011.png" height="30"></p>

where _F_ is the tensor of functions to differentiate.

If `var` is a named vector, e.g. `c(x = 0, y = 0)` , derivatives will be computed at that point.
Note that if `f` is a function, then `var` must be a named vector giving the point at which the numerical derivatives will be computed.


__Value__


array of derivatives.


__Examples__

```r 
# derive f with respect to x
derivative(f = "sin(x)", var = "x")

# derive f with respect to x and evaluate in x = 0
derivative(f = "sin(x)", var = c("x" = 0))

# derive f twice with respect to x
derivative(f = "sin(x)", var = "x", order = 2)

# derive f once with respect to x, and twice with respect to y
derivative(f = "y^2*sin(x)", var = c("x","y"), order = c(1,2))

# compute the gradient of f with respect to (x,y)
derivative(f = "y*sin(x)", var = c("x","y"))

# compute the Jacobian of f with respect to (x,y)
f <- c("y*sin(x)", "x*cos(y)")
derivative(f = f, var = c("x","y"))

# compute the Hessian of f with respect to (x,y)
g <- derivative(f = "y^2*sin(x)", var = c("x","y"))
derivative(f = g, var = c("x","y"))

# compute the Jacobian of f with respect to (x,y) and evaluate in (0,0)
f1 <- function(x, y) y*sin(x)
f2 <- function(x, y) x*cos(y)
derivative(f = c(f1, f2), var = c("x"=0,"y"=0))

``` 



### `gradient`: Numerical and Symbolic Gradient

__Description__


Compute the gradient or jacobian of functions, expressions and characters.


__Usage__

```r
gradient(f, var, accuracy = 2, stepsize = NULL, coordinates = "cartesian")
f %gradient% var
```


__Arguments__

Argument      |Description
------------- |----------------
```f```     |     function, expression or character array.
```var```     |     character vector, giving the variable names with respect to which derivatives will be computed. If a named vector is provided, derivatives will be computed at that point.
```accuracy```     |     accuracy degree for numerical derivatives.
```stepsize```     |     finite differences stepsize for numerical derivatives. Auto-optimized by default.
```coordinates```     |     coordinate system to use. One of: `cartesian` , `polar` , `spherical` , `cylindrical` , `parabolic` , `parabolic-cylindrical` or a character vector of scale factors for each varibale.

__Value__


gradient or jacobian array.


__Examples__

```r 
# gradient with respect to x
gradient(f = "sin(x)", var = "x")
"sin(x)" %gradient% "x"

# gradient with respect to x and evaluate in x = 0
gradient(f = "sin(x)", var = c("x" = 0))
"sin(x)" %gradient% c(x=0)

# gradient with respect to (x,y)
gradient(f = "y*sin(x)", var = c("x","y"))
"y*sin(x)" %gradient% c("x","y")

# jacobian with respect to (x,y)
f <- c("y*sin(x)", "x*cos(y)")
gradient(f = f, var = c("x","y"))
f %gradient% c("x","y")

# jacobian with respect to (x,y) and evaluate in (x = 0, y = 0)
f <- c(function(x, y) y*sin(x), function(x, y) x*cos(y))
gradient(f = f, var = c(x=0,y=0))
f %gradient% c(x=0,y=0)

# gradient in spherical coordinates
gradient('r*theta*phi', var = c('r','theta','phi'), coordinates = 'spherical')

# numerical gradient in spherical coordinates
f <- function(r, theta, phi) r*theta*phi
gradient(f, var = c('r'=1, 'theta'=pi/4, 'phi'=pi/4), coordinates = 'spherical')

``` 




### `hessian`: Numerical and Symbolic Hessian

__Description__


Compute the hessian matrix of functions, expressions and characters.


__Usage__

```r
hessian(f, var, accuracy = 2, stepsize = NULL, coordinates = "cartesian")
f %hessian% var
```


__Arguments__

Argument      |Description
------------- |----------------
```f```     |     function, expression or character.
```var```     |     character vector, giving the variable names with respect to which derivatives will be computed. If a named vector is provided, derivatives will be computed at that point.
```accuracy```     |     accuracy degree for numerical derivatives.
```stepsize```     |     finite differences stepsize for numerical derivatives. Auto-optimized by default.
```coordinates```     |     coordinate system to use. One of: `cartesian` , `polar` , `spherical` , `cylindrical` , `parabolic` , `parabolic-cylindrical` or a character vector of scale factors for each varibale.

__Value__


hessian matrix.


__Examples__

```r
# hessian with respect to x
hessian(f = "sin(x)", var = "x")
"sin(x)" %hessian% "x"

# hessian with respect to x and evaluate in x = 0
hessian(f = "sin(x)", var = c("x" = 0))
"sin(x)" %hessian% c(x=0)

# hessian with respect to (x,y)
hessian(f = "y*sin(x)", var = c("x","y"))
"y*sin(x)" %hessian% c("x","y")

# hessian in spherical coordinates
hessian('r*theta*phi', var = c('r','theta','phi'), coordinates = 'spherical')

# numerical hessian in spherical coordinates
f <- function(r, theta, phi) r*theta*phi
hessian(f, var = c('r'=1, 'theta'=pi/4, 'phi'=pi/4), coordinates = 'spherical')

``` 




### `divergence`: Numerical and Symbolic Divergence

__Description__


Compute the divergence of functions, expressions and characters.


__Usage__

```r
divergence(f, var, accuracy = 2, stepsize = NULL, coordinates = "cartesian")
f %divergence% var
```


__Arguments__

Argument      |Description
------------- |----------------
```f```     |     function, expression or character array.
```var```     |     character vector, giving the variable names with respect to which derivatives will be computed. If a named vector is provided, derivatives will be computed at that point.
```accuracy```     |     accuracy degree for numerical derivatives.
```stepsize```     |     finite differences stepsize for numerical derivatives. Auto-optimized by default.
```coordinates```     |     coordinate system to use. One of: `cartesian` , `polar` , `spherical` , `cylindrical` , `parabolic` , `parabolic-cylindrical` or a character vector of scale factors for each varibale.

__Value__


divergence array.


__Examples__

```r 
# divergence of a vector field
f <- c('x^2','y^3','z^4')
divergence(f, var = c('x','y','z'))
f %divergence% c('x','y','z')

# numerical divergence of a vector field
f <- c(function(x,y,z) x^2, function(x,y,z) y^3, function(x,y,z) z^4)
divergence(f, var = c('x'=1,'y'=1,'z'=1))
f %divergence% c('x'=1,'y'=1,'z'=1)

# divergence of array of vector fields
f1 <- c('x^2','y^3','z^4')
f2 <- c('x','y','z')
a <- matrix(c(f1,f2), nrow = 2, byrow = TRUE)
divergence(a, var = c('x','y','z'))
a %divergence% c('x','y','z')

# divergence in polar coordinates
f <- c('sqrt(r)/10','sqrt(r)')
divergence(f, var = c('r','phi'), coordinates = 'polar')

``` 




### `curl`: Numerical and Symbolic Curl

__Description__


Compute the curl of functions, expressions and characters.


__Usage__

```r
curl(f, var, accuracy = 2, stepsize = NULL, coordinates = "cartesian")
f %curl% var
```


__Arguments__

Argument      |Description
------------- |----------------
```f```     |     function, expression or character array.
```var```     |     character vector, giving the variable names with respect to which derivatives will be computed. If a named vector is provided, derivatives will be computed at that point.
```accuracy```     |     accuracy degree for numerical derivatives.
```stepsize```     |     finite differences stepsize for numerical derivatives. Auto-optimized by default.
```coordinates```     |     coordinate system to use. One of: `cartesian` , `polar` , `spherical` , `cylindrical` , `parabolic` , `parabolic-cylindrical` or a character vector of scale factors for each varibale.

__Value__


curl array.


__Examples__

```r 
# curl of a vector field
f <- c('x*y','y*z','x*z')
curl(f, var = c('x','y','z'))
f %curl% c('x','y','z')

# irrotational vector field
f <- c('x','-y','z')
curl(f, var = c('x','y','z'))
f %curl% c('x','y','z')

# numerical curl of a vector field
f <- c(function(x,y,z) x*y, function(x,y,z) y*z, function(x,y,z) x*z)
curl(f, var = c('x'=1,'y'=1,'z'=1))
f %curl% c('x'=1,'y'=1,'z'=1)

# curl of array of vector fields
f1 <- c('x*y','y*z','z*x')
f2 <- c('x','-y','z')
a <- matrix(c(f1,f2), nrow = 2, byrow = TRUE)
curl(a, var = c('x','y','z'))
a %curl% c('x','y','z')

# curl in polar coordinates
f <- c('sqrt(r)/10','sqrt(r)')
curl(f, var = c('r','phi'), coordinates = 'polar')

``` 



### `laplacian`: Numerical and Symbolic Laplacian

__Description__


Compute the laplacian of functions, expressions and characters.


__Usage__

```r
laplacian(f, var, accuracy = 2, stepsize = NULL, coordinates = "cartesian")
f %laplacian% var
```


__Arguments__

Argument      |Description
------------- |----------------
```f```     |     function, expression or character array.
```var```     |     character vector, giving the variable names with respect to which derivatives will be computed. If a named vector is provided, derivatives will be computed at that point.
```accuracy```     |     accuracy degree for numerical derivatives.
```stepsize```     |     finite differences stepsize for numerical derivatives. Auto-optimized by default.
```coordinates```     |     coordinate system to use. One of: `cartesian` , `polar` , `spherical` , `cylindrical` , `parabolic` , `parabolic-cylindrical` or a character vector of scale factors for each varibale.

__Value__


laplacian array.


__Examples__

```r 
# laplacian of a scalar field
f <- 'x^2+y^2+z^2'
laplacian(f, var = c('x','y','z'))
f %laplacian% c('x','y','z')

# laplacian of scalar fields
f <- c('x^2','y^3','z^4')
laplacian(f, var = c('x','y','z'))
f %laplacian% c('x','y','z')

# numerical laplacian of scalar fields
f <- c(function(x,y,z) x^2, function(x,y,z) y^3, function(x,y,z) z^4)
laplacian(f, var = c('x'=1,'y'=1,'z'=1))
f %laplacian% c('x'=1,'y'=1,'z'=1)

# laplacian of array of scalar fields
f1 <- c('x^2','y^3','z^4')
f2 <- c('x','y','z')
a <- matrix(c(f1,f2), nrow = 2, byrow = TRUE)
laplacian(a, var = c('x','y','z'))
a %laplacian% c('x','y','z')

# laplacian in polar coordinates
f <- c('sqrt(r)/10','sqrt(r)')
laplacian(f, var = c('r','phi'), coordinates = 'polar')

``` 




### `integral`: Monte Carlo Integration

__Description__


Integrate multidimensional functions, expressions, and characters in arbitrary [orthogonal coordinate systems](https://en.wikipedia.org/wiki/Orthogonal_coordinates#Table_of_orthogonal_coordinates) .


__Usage__

```r
integral(f, ..., err = 0.01, rel = TRUE, coordinates = "cartesian", verbose = TRUE)
```


__Arguments__

Argument      |Description
------------- |----------------
```f```     |     function, expression or characters.
```...```     |     integration bounds.
```err```     |     acuracy requested.
```rel```     |     logical. Relative accuracy? If `FALSE` , use absolute accuracy.
```coordinates```     |     coordinate system to use. One of: `cartesian` , `polar` , `spherical` , `cylindrical` , `parabolic` , `parabolic-cylindrical` or a character vector of scale factors for each varibale.
```verbose```     |     logical. Print on progress?

__Value__

`list` with components

Name      |Description
------------- |----------------
```value```     |     the final estimate of the integral.
```abs.error```     |     estimate of the modulus of the absolute error.


__Examples__

```r 
# integrate character
integral('sin(x)', x = c(0,2*pi), rel = FALSE, verbose = FALSE)

# integrate expression
integral(parse(text = 'x'), x = c(0,1), verbose = FALSE)

# integrate function
integral(function(x) exp(x), x = c(0,1), verbose = FALSE)

# multivariate integral
integral(function(x,y) x*y, x = c(0,1), y = c(0,1), verbose = FALSE)

# surface of a sphere
integral('1', r = 1, theta = c(0,pi), phi = c(0,2*pi), coordinates = 'spherical', verbose = FALSE)

# volume of a sphere
integral('1', r = c(0,1), theta = c(0,pi), phi = c(0,2*pi), coordinates = 'spherical', verbose = FALSE)

```

```r

# Electric charge contained in a region of space
# Based on the divergence theorem and Maxwell's equations

# electric potential of unitary point charge
V <- '1/(4*pi*r)'

# electric field
E <- -1 %prod% gradient(V, c('r', 'theta', 'phi'), coordinates = 'spherical')

# electric charge
integral(E[1], r = 1, theta = c(0,pi), phi = c(0,2*pi), coordinates = 'spherical', verbose = FALSE)


``` 





## Full Documentation

https://cran.r-project.org/package=calculus
