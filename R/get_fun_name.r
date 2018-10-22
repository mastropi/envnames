#' Return the name of the current function or a calling function in the chain
#' 
#' Return the name of the function that has been called \code{n} levels up from a given function's body.
#' This function is intended to be called only within a function.
#' 
#' @param n number of levels to go up in the calling chain in search of the calling function name.
#' Defaults to \code{n=0}, meaning that the name returned is the name of the function that calls
#' \code{get_fun_name}.
#' 
#' @return A string containing the name of the function that has been called \code{n} levels up
#' from the function calling \code{get_env_name}. The function name is returned without context,
#' that is the enclosing environment of the function is not part of the returned value.
#' (e.g. if the function is \code{env1$f} or \code{env1$env2$f} only \code{"f"} will be returned).
#' 
#' @examples
#' # Show the name of the active function
#' f <- function() { cat("We are in function:", get_fun_name(), "\n") }
#' f()
#' 
#' # Show the name of the calling function
#' f <- function(x) { cat("Calling function name is:", get_fun_name(1), "\n") }
#' env1 <- new.env()
#' with(env1, g <- function() { f(3) })
#' env1$g()
get_fun_name = function(n=0)
# When n=0 (default) the function returns the function name of the function calling get_fun_name().
{
  # Increase n by 1 so that we do as if we were working in the environment of the calling function
  n = n + 1

  # Call received by the function n levels up
  # When n=1 (the default), we get the call of the function calling get_fun_name().
  cur_call = sys.call(sys.parent(n))
  fun_name = as.character(cur_call)[1]

	fun_name = unlist(extract_last_member(fun_name)["name"])

  return(fun_name)
}
