#' Return the name of the function called n levels up
#' 
#' Returns the name of the function that has been called \code{n} levels above the function that calls
#' \code{get_fun_name}.
#' 
#' @param n number of levels to go up in the calling chain in search of the calling function name.
#' Defaults to \code{n=0}, meaning that the name returned is the name of the function that calls
#' \code{get_fun_name}.
#' 
#' @return a string containing the name of the function that has been called \code{n} levels up
#' from the function calling \code{get_env_name}. Any environment name that was used to call such
#' function is removed (e.g. if the calling function is 'env1$f' only "f" will be returned). 
#' 
#' @examples
#' # Print the name of the active function
#' f = function() { print(get_fun_name()) }
#' f()
#' 
#' # Print the name of the calling function
#' f = function(x) { print(get_fun_name(1)) }  
#' g = function() { f(3) }
#' g()
get_fun_name = function(n=0)
# When n=0 (default) the function returns the function name of the function calling get_fun_name().
{
  # Call received by the n+1 calling function environment.
  # When n=0 (the default), we get the call of the function calling get_fun_name().
  cur_call = sys.call(sys.parent(n=1+n))
  fun_name = as.character(cur_call)[1]
  
  # Remove any $ (e.g. when calling f as env$f)
  pos_dollar = regexpr("\\$", fun_name)[[1]]
  if (pos_dollar > 0) {
    fun_name = substr(fun_name, pos_dollar+1, nchar(fun_name))
  }

  return( fun_name )
}
