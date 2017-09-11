#' Return the value of an object referenced by a parameter at a given parent generation
#'
#' Return the value of the object referenced by a parameter at the \code{n}-th parent generation.
#' At each parent generation, there is a pair of "object name" <-> "object value". The task of this function
#' is to retrieve the object name at a given parent generation and then its value based on the path that
#' leads to the parameter in the current function.
#' 
#' @param obj object whose value should be returned. The object can be passed either as a variable name or
#' as a string representing the object whose value is of interest.
#' @param n number of parent generations to go back to retrieve the value of the object that leads to \code{obj}
#' in the function calling chain. See details for more information.
#' @param silent when \code{FALSE}, the names of the environments and objects in those environments are printed
#' as those environments are traversed by this function.
#' 
#' @details
#' The purpose of this function is to get the value of objects as they are passed through different functions.  
#' 
#' Note that this is not the same as doing \code{eval(obj, parent.frame(n))} because this expression evaluates
#' the object named \code{obj} in the environment that is at the \code{n}-th parent generation.  
#' The \code{get_obj_value()} function instead, evaluates the object that LED to the current \code{obj}
#' object, in the environment that is n parent generations back.
#' 
#' If the \code{obj} is given as a string, it also evaluates to the object value when an object
#' with that name exists in the given parent generation. However, the object should be passed with no explicit reference
#' to the environment where it is defined. For instance we should use \code{with(env1, get_obj_value("z"))} and
#' NOT \code{get_obj_value("env1$z")}, which returns simply \code{"env1$z"}.
#' 
#' @return The value of the object as described in the Details section.
#' 
#' @examples
#' # Define functions that are called to show the behaviour of get_obj_value()
#' h <- function(x) {
#'   # Get the value of parameter 'x' n levels up, i.e. the value of the parameter that led to the current parameter x
#'   # in the environment that is n levels up in the function calling chain.
#'   xval = get_obj_value(x, n=1, silent=TRUE)
#'   return(xval)
#' }
#' g <- function(y) {
#'   x = 2
#'   return( h(y) )
#' }
#' z = 3
#' g(z)                     # Returns 3, because the value of x in h() is the value of y in the calling function g() which is 3.
#' 
#' # Example of calling get_obj_value() from outside a function
#' x = 3
#' v = c(1, 2)
#' get_obj_value(x)         # 3
#' get_obj_value("x")       # 3
#' get_obj_value(3)         # 3
#' get_obj_value(v[1])      # 1
#' 
get_obj_value = function(obj, n=0, silent=TRUE) {
	# Note that the evaluation in eval() is done on parent.frame(n+1), i.e. on the environment n+1 levels up
	# from this function, which is the same environment where the object's name is retrieved from
  # (by get_obj_name(obj, n=n+1)). In fact:
	# - the name of the object to be evaluated is returned by get_obj_name(obj, n=n+1)
  # (i.e. the name of the object in the environment that is n levels up from the function that called get_obj_value())
	# - this parameter needs to be evaluated in that environment (parent.frame(n+1))
	# (which is n+1 levels up from get_obj_value())
	value = try( eval(as.symbol(get_obj_name(obj, n=n+1, silent=silent)), parent.frame(n+1)), silent=TRUE )
	if (inherits(value, "try-error")) {
		return(obj)
	} else {
		return(value)
	}
}
