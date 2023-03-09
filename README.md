[![](https://www.r-pkg.org/badges/version/calculus)](https://cran.r-project.org/package=calculus) 
[![](https://www.r-pkg.org/badges/last-release/calculus)](https://cran.r-project.org/package=calculus) 
[![](https://cranlogs.r-pkg.org/badges/grand-total/calculus)](https://cran.r-project.org/package=calculus)
[![build](https://github.com/eguidotti/calculus/actions/workflows/build.yaml/badge.svg)](https://github.com/eguidotti/calculus/actions/workflows/build.yaml)

# High Dimensional Numerical and Symbolic Calculus in R <img src="man/figures/logo.png" width="128" align="right" style="background-color:white" />

> Efficient C++ optimized functions for numerical and symbolic calculus. 

The R package **calculus** implements C++ optimized functions for numerical and symbolic calculus, such as the [Einstein summing convention](https://calculus.eguidotti.com/articles/einstein.html), fast computation of the [Levi-Civita](https://calculus.eguidotti.com/reference/epsilon.html) symbol and generalized [Kronecker delta](https://calculus.eguidotti.com/reference/delta.html), [Taylor series expansion](https://calculus.eguidotti.com/articles/taylor.html), multivariate [Hermite polynomials](https://calculus.eguidotti.com/articles/hermite.html), high-order [derivatives](https://calculus.eguidotti.com/articles/derivatives.html), [ordinary differential equations](https://calculus.eguidotti.com/articles/ode.html), [differential operators](https://calculus.eguidotti.com/articles/differential-operators.html) and [numerical integration](https://calculus.eguidotti.com/articles/integrals.html) in arbitrary orthogonal coordinate systems. The library applies numerical methods when working with `functions` or symbolic programming when working with `characters` or `expressions`. The package handles multivariate numerical calculus in arbitrary dimensions and coordinates and implements the symbolic counterpart of the numerical methods whenever possible, without depending on external computer algebra systems. Except for [Rcpp](https://cran.r-project.org/package=Rcpp), the package has no strict dependencies in order to provide a stable self-contained toolbox that invites re-use. 

## Quickstart 

Install the package.

```R
install.packages("calculus")
```
Load the package.

```R
library(calculus)
```

[Read](https://CRAN.R-project.org/package=calculus/calculus.pdf) or [browse](https://calculus.eguidotti.com/reference/index.html) the documentation and the [vignettes](https://calculus.eguidotti.com/articles/index.html).

## Philosophy

The package provides a unified interface to work with mathematical objects in R. The library applies numerical methods when working with `functions` or symbolic programming when working with `characters` or `expressions`. To describe multidimensional objects such as vectors, matrices, and tensors, the package uses the class `array` regardless of the dimension. This is done to prevent unwanted results due to operations among different classes such as `vector` for unidimensional objects or `matrix` for bidimensional objects.


## Dependencies

The package integrates seamlessly with [cubature](https://cran.r-project.org/package=cubature) for efficient numerical integration in C. However, except for [Rcpp](https://cran.r-project.org/package=Rcpp), the package has no strict dependencies in order to provide a stable self-contained toolbox that invites re-use. 

## Testing

Several [unit tests](https://github.com/eguidotti/calculus/tree/master/tests/testthat) are implemented via the standard framework offered by [testthat](https://cran.r-project.org/package=testthat) and run via continuous integration.

## Contribute

[Report a bug](https://github.com/eguidotti/calculus/issues) and star the [repository](https://github.com/eguidotti/calculus/).

## Cite as

Guidotti E (2022). “calculus: High-Dimensional Numerical and Symbolic Calculus in R.” _Journal of Statistical Software_, *104*(5), 1-37. [doi:10.18637/jss.v104.i05](https://doi.org/10.18637/jss.v104.i05)

A BibTeX entry for LaTeX users is

```bibtex
@Article{calculus,
  title = {{calculus}: High-Dimensional Numerical and Symbolic Calculus in {R}},
  author = {Emanuele Guidotti},
  journal = {Journal of Statistical Software},
  year = {2022},
  volume = {104},
  number = {5},
  pages = {1--37},
  doi = {10.18637/jss.v104.i05},
}
```

