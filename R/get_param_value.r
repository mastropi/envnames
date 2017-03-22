#' Get the value of an object passed as parameter
#'
#' Return the value of an object passed as parameter to a function, regardless of whether the object
#' was passed as the object per se (e.g. as in \code{x}) or whether it was passed as a string representing
#' the object name (as in \code{"x"}). See details.
#' 
#' @param obj object whose value should be returned. The object can be passed either as a variable name or
#' as a string representing the object whose value is of interest.
#' 
#' @details
#' The main purpose of this function is to parse input parameters received by functions so that the
#' parameter can either be passed as a variable name or as the string representing the variable name.
#' In both cases, the result of a call to get_param_value() on the function's parameter will be the value
#' of the parameter passed.
#' 
#' If the object cannot be resolved to a value the object itself is returned. This happens when for instance
#' the object contains an expression such as as.name(), get(), v[1], etc.
#' 
#' This function is intended to be called from another function or from the global environment. When called
#' from another environment, the value of an object passed as string is that same string.
#' For instance: \code{with(env1, get_param_value("z"))} returns "z" and \emph{not} the value of variable 'z'
#' even if variable 'z' exists in the env1 environment. 
#' 
#' @return The value of the object passed as parameter.
#' 
#' @examples
#' x = 3
#' v = c(1, 2)
#' get_param_value(x)         # 3
#' get_param_value("x")       # 3
#' get_param_value(3)         # 3
#' get_param_value(v[1])      # 1
#' 
#' # Example of use to parse input parameters
#' test = function(x) {
#' 	x = get_param_value(x)
#'  return(x+1)
#' }
#' y = 3
#' test(y)                  # Returns 4
#' test("y")                # Returns 4
get_param_value = function(obj) {
	# Note that the evaluation in eval() is done on parent.frame(2), i.e. on the environment two levels up
	# from this function. That is:
	# - get_param_value() is expected to be called from a function to parse the value of a parameter
	# - the name of the parameter is returned by get_obj_name(obj, n=1) (i.e. the name of the object in the function calling get_param_value())
	# - this parameter needs to be evaluated in the function *calling* that function!
	# (which is 2 levels up from get_param_value())
	# Ex:
	# test = function(x) {
	# 	x = get_param_value(x)
	# 	return(x+1)
	# }
	# y = 3
	# test(y)    # returns 4
	# test("y")  # returns 4
	# In this example, the name of x inside function test() as returned by the get_obj_name() call done in this
	# function (get_param_value) is "y"; the value of as.symbol("y") is 3 when evaluated in the calling environment
	# to test() which is parent.frame(2) from within get_param_value(). Note that x should contain the *value*
	# of the variable received as parameter (e.g. 3), NOT the variable as a symbol ('y'), o.w. x+1 fails.
	# In fact, whenever 'x' is used inside a function, the function says: "give me the *value* of the variable
	# referenced by 'x')
	value = try( eval(as.symbol(get_obj_name(obj, n=1, silent=TRUE)), parent.frame(2)), silent=TRUE )
	if (inherits(value, "try-error")) {
		return(obj)
	} else {
		return(value)
	}
}
