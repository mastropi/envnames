#' Get the name of an object as a string
#'
#' Returns the name of an object as a string either when the object is given as an object name per se
#' (e.g. variable name as in \code{x}) or when it is given as a string containing the object name (as in \code{"x"})
#' This function can be called either from within another function or from e.g. the global environment.
#' The name is returned even if the object does not exist.
#'
#' @param obj object whose name should be returned. The value should be passed either as a variable name or
#' as a string representing an object. The actual referenced object may or may not exist. The value passed
#' should not contain any function that results in objects, such as as.name(), get(), etc.
#' @param n number of levels to go up from the \code{get_obj_name} environment to evaluate \code{obj}.
#' It defaults to the calling environment. This parameter is useful when calling \code{get_obj_name} from
#' within an \code{*apply} function where we should use n=2 to get to the proper environment where the object
#' should be evaluated.
#' 
#' @details The main purpose of this function is to parse input parameters received by functions so that the
#' parameter can either be passed as a variable name or as the string corresponding to the variable name.  
#' The function uses the deparse(substitute()) call on the object passed to the function and then
#' removes any quotes that could have been added to the result if the object passed is already a string.
#'   
#' @examples
#' get_obj_name(x)		# returns "x"
#' get_obj_name("x")	# returns "x"
get_obj_name = function(obj, n=1) {
	# NOTES:
	# - This whole process is equivalent to doing the following in the calling function:
	# obj_name = deparse(substitute(obj))    # this returns "\"x\"" when obj = "x" and "x" if obj = x (the variable x)
  # if (length(grep("\"", obj_name)) > 0)  # this is TRUE when obj is already a string
	# 	obj_name = obj
	# - We cannot use is.name() to check if the object is given as a string because is.name() returns TRUE
	# when the argument is an object created with as.name(), e.g. as.name("x").
	# - We cannot use is.character() to check if the object is given as a string because is.character()
	# returns TRUE when the CONTENT of an object is of type character...!
	# - it's IMPORTANT to evaluate substitute() in the parent.frame() (see below) (i.e. the environment of the
	# calling function because that's where the actual value of 'obj' that we want to convert to a string is
	# stored! 
	
  # Get the object whose name should be returned. This is the object that was **passed** to this function!
  obj_parent = deparse(substitute(obj))

  # Expression to evaluate at the parent environment where the actual object 'obj' needs to be 'substitute'd
  expr = parse(text=paste("deparse(substitute(", obj_parent, "))"))

  # Get the object name (i.e. "x" if obj = x (the variable x) or obj = "x" (the string "x"))
	# by evaluating the deparse(substitute()) expression in the environment of the calling function. 
	obj_parent_name = eval(expr, envir=parent.frame(n))

	# Check if obj was given as a variable name or as a string
  if (length(grep("\"", obj_parent_name)) > 0)  # this is TRUE when the value of obj_name is already a string (e.g. obj_name = "\"x\"" when obj = "x")
    obj_parent_name = gsub("\"", "", obj_parent_name)  # Remove the internal quotes from the object name

  return(obj_parent_name)
}
