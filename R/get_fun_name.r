#' Returns the name of the function being called n+1 levels counted back from the present function environment
#'
#' @export
get_fun_name = function(n=0)
# When n=0 (default) the function returns the function name of the calling function.
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
