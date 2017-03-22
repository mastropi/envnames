#' Return the value of an object in an environment n levels up
#' 
#' @param obj object whose value is of interest.
#' @param n number of levels to go up from the function calling \code{get_obj_value}
#' to evaluate the object passed through the functions in the chain.
#' @return the value of the object in environment n levels up leading to \code{obj}
#' in the environment of the function calling \code{get_obj_value}.
#' 
#' @seealso
#' \link{get_obj_name}
get_obj_value = function(obj, n=0, silent=TRUE) {
	# Increase n by 1 so that we do as if we were working in the environment of the calling function
	n = n + 1
	return( get_obj_name(obj, n=n+1, eval=TRUE, silent=silent) )
}
