#' The Package
#' 
#' @docType package
#' 
#' @import Rcpp
#' @importFrom Rcpp evalCpp
#' @useDynLib calculus, .registration=TRUE
NULL  

.onAttach <- function(libname, pkgname) {
  
  packageStartupMessage(
"#######################################################
  __           __                __ 
 /    /\\  |   /   /  \\ |   /  \\ (_  
 \\__ /--\\ |__ \\__ \\__/ |__ \\__/ __) 
                                             
Bug reports and new feature requests: 
https://github.com/emanuele-guidotti/calculus/issues/
 
Using the package? Please star the repository: 
https://github.com/emanuele-guidotti/calculus/

Happy coding!

#######################################################"
 )
  
}




