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
#' It defaults to 1 which implies the calling environment. This parameter is useful in very
#' special circumstances, and normally should be left at its default value.
#' 
#' @details The main purpose of this function is to parse input parameters received by functions so that the
#' parameter can either be passed as a variable name or as the string corresponding to the variable name.  
#' The function uses the deparse(substitute()) call on the object passed to the function and then
#' removes any quotes that could have been added to the result if the object passed is already a string.
#' The function CANNOT be applied in *apply() contexts.
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
	# - it's IMPORTANT to evaluate substitute() in the parent.frame() --or in the parent frame several levels up when n > 1--
  # (see below) (i.e. the environment of the calling function n leveles up, because that's where the actual value of
  # 'obj' that we want to convert to a string is stored!
	
  # Get the object whose name should be returned.
  # When n = 1, this is the object that was **passed** to this function
  # When n > 1, this is the object that was passed to the function that is n - 1 levels up in the calling chain.
  # In the latter case, we need to iterate in order to get to that object
  # Example: if we call environment_name(env=env1)
  #   environment_name(env=env1)    --> calls get_obj_address(obj1=env, n=2)
  #   get_obj_address(obj1=env, n=2) --> calls get_obj_name(obj=obj1, n=2)
  # In this case we are interested in retrieving the value of parameter 'env' received by environment_name()
  # which in this case is variable 'env1', whose name ("env1") we are interested in retrieving
  # by calling environment_name()
  # This works as follows:
  # 1) The first substitute() call gives obj_parent = obj1
  # 2) The while loop with nback = 1 gives expr = substitute(obj1) and obj_parent = 'substitute(obj1)' evaluated
  # in the calling function environment (get_obj_address()) via eval.parent(), which is 'env' 
  # (since this is the value of obj1 received by get_obj_address())
  # 3) Then we end the while loop and we get the value (or object name) of parameter 'env' in the environment
  # that is n=2 levels up (via the deparse(subsitute()) expression), in this case this is the execution environment
  # of environment_name(env=env1). Clearly the value or object name of 'env' is "env1", which is what is returned
  # to the outside world.
  obj_parent = substitute(obj)
  nback = 1
  while (nback < n) {
    expr = parse(text=paste("substitute(", obj_parent, ")"))
    obj_parent = eval.parent(expr, n=nback)
    nback = nback + 1
  }

  # Expression to evaluate at the parent.frame(n) environment where object 'obj_parent' needs to be 'substitute'd
  # with the value it received when the function was called
  expr = parse(text=paste("deparse(substitute(", obj_parent, "))"))

  # Get the object name (i.e. "x" if obj = x (the variable x) or obj = "x" (the string "x"))
	# by evaluating the deparse(substitute()) expression in the environment of the calling function n levels up.
	obj_parent_name = eval(expr, envir=parent.frame(n))

	# Check if obj_parent was given as a variable name or as a string
  if (length(grep("\"", obj_parent_name)) > 0)  # this is TRUE when the value of obj_parent_name is already a string (e.g. obj_parent_name = "\"x\"" when obj_parent = "x")
    obj_parent_name = gsub("\"", "", obj_parent_name)  # Remove the internal quotes from the object name

  return(obj_parent_name)
}
