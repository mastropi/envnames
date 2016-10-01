#' Return the name of a calling function and its environment
#'
#' This is a wrapper for \code{get_fun_calling_chain(n)} and returns the name of the calling function
#' including the environment where it is defined n levels up. The two pieces of infomration are separated by the
#' \code{$} sign.
#' This function is intended to be called only within a function.
#' 
#' @param n non-negative integer indicating the number of levels to go up from the calling function
#' to retrieve the function in the calling chain.
#' Defaults to 1, which means "return the last function in the calling chain". 
#' 
#' @examples
#' # Prepare environments
#' env1 <- new.env()
#' env2 <- new.env()
#' with(env2, env21 <- new.env())
#' 
#' # Function that shows the names of calling functions in the chain and their environments
#' f <- function(x) {
#'  cat("Now in function:", get_fun_calling(0), "\n")
#'  cat("\tName of the calling function:", get_fun_calling(), "\n")
#'  cat("\tName of the calling function two levels up:", get_fun_calling(2), "\n")
#'  cat("\tName of the calling function three levels up:", get_fun_calling(3), "\n")
#'  cat("\tName of the calling function four levels up:", get_fun_calling(4), "\n")
#' }
#' 
#' # Prepare a calling chain  
#' with(env1, g <- function() { f(3) })
#' with(env2, h <- function() { env1$g() })
#' with(env2$env21, hh <- function() { env2$h() })
#' 
#' # Run the different functions defined to show the different calling chains
#' env1$g()
#' env2$h()
#' env2$env21$hh()
get_fun_calling <- function(n=1) {
	return(get_fun_calling_chain(n+1))	# We must sum 1 to the n value passed because we have added a new layer
                                      # in the calling function chain, namely the layer of this function
                                      # get_fun_calling().
}
