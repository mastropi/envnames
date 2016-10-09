#' Get the name of an object as a string
#'
#' Return the name of an object as a string either when the object is given as an object name per se
#' (i.e. as a symbol as in e.g. \code{x}) or when it is given as a string containing the object name
#' (as in \code{"x"}). See details.
#'
#' @param obj object whose name should be returned. The object can be passed either as a variable name or
#' as a string representing an object. The actual referenced object may or may not exist.
#' @param n non-negative integer indicating the number of levels to go up from the calling function environment
#' to evaluate \code{obj}. It defaults to 0 which implies that \code{obj} should be evaluated in the calling
#' environment (e.g. the environment of the calling function or the global environment if \code{get_obj_name}
#' is called from the global environment).
#' @param silent flag indicating whether to show messages about problems evaluating the object.
#' Defaults to \code{FALSE}.
#' 
#' @details
#' The main purpose of this function is to parse input parameters received by functions so that the
#' parameter can either be passed as a variable name or as the string representing the variable name.
#' In both cases, the result of a call to get_obj_name() on the input parameter will be the string
#' containing the name of the parameter passed (e.g. \code{"x"}) and this standardization can be used to
#' refer to the variable passed using \code{eval(as.name(param_name), envir=parent.frame())}
#' where param_name is the result of calling \code{get_obj_name()} on \code{param}. See example below.
#' 
#' The function uses the deparse(substitute()) call on the object passed to the function and then
#' removes any quotes that could have been added to the result if the object passed is already a string.
#' The function is meaningless when applied in *apply() contexts or to array elements.
#' For instance:
#' \code{v = c("x", "y"); get_obj_name(v[1])} returns NULL with a message indicating that the object name is not valid.
#' \code{lapply(c("x","y"), FUN=get_obj_name)} returns NULL for both entries of the array input to \code{lapply()}.
#' 
#' The value passed to \code{obj} should not contain any function that results in objects, such as
#' as.name(), get(), etc.
#' 
#' When called from outside a function (e.g. from the global environment), the object name is returned
#' even if the object does not exist.
#' 
#' @return A string giving the name of the object passed, or NULL if the object does not have a valid name,
#' in which case an appropriate message is shown if \code{silent=FALSE}
#' (for example when the object is "<aa" or "v[1]").
#' 
#' @seealso
#' get_obj_value which returns the value stored in the object passed as parameter. 
#' 
#' @examples
#' x = 3
#' v = c("x", "y")
#' get_obj_name(x)						# "x"
#' get_obj_name("x")					# "x"
#' get_obj_name(as.name("x")) # "x"
#' get_obj_name(3)						# NULL
#' get_obj_name(TRUE)					# NULL
#' get_obj_name(v[1])					# NULL, and a message of invalid object name is shown
#' 
#' # Example of use to parse input parameters
#' test = function(x) {
#'	# Store in x the value of the object passed as parameter, regardless of whether the object is passed 
#'	# as object per se (e.g. x=y) or passed as a string representing the object name (e.g. x="y").
#'  # Note the use of parent.frame() as the environment of evaluation of as.name() since the object
#'  # whose name is returned by get_obj_name() should be evaluated in the calling environment in order
#'  # to get its value.
#' 	x = eval(as.name(get_obj_name(x)), envir=parent.frame())
#'  return(x+1)
#' }
#' y = 3
#' test(y)                    # Returns 4
#' test("y")                  # Returns 4
get_obj_name = function(obj, n=0, silent=FALSE) {
	# NOTES:
	# - When n=0, this whole process is equivalent to doing the following in the calling function:
	# obj_name = deparse(substitute(obj))    # this returns "\"x\"" when obj = "x" and "x" if obj = x (the variable x)
  # if (length(grep("\"", obj_name)) > 0)  # this is TRUE when obj is already a string
	# 	obj_name = obj
	# - We cannot simply use is.name() to check if the object is given as a symbol because is.name() returns FALSE
	# when the argument is a symbol (as opposed to a string). In order for is.name() to return true,
	# the parameter to the function must be passed using as.name(). Ex: get_obj_name(as.name("x"))
	# - We cannot simply use is.character() to check if the object is given as a string because is.character()
	# returns TRUE when the CONTENT of an object is of type character...!
	# - When implementing the function, it's IMPORTANT to evaluate substitute() in the parent.frame()
	# --or in the parent frame several levels up when n > 1 (see below)-- i.e. in the environment of the
	# calling function (as opposed to the environment of this function get_obj_name),
	# because that's where the actual object we are interested in parsing resides.

  # Get the object whose name should be returned.
  # When n = 0, this is the object that was **passed** to this function
  # When n > 0, this is the object that was passed to the function that is n levels up in the calling chain.
  # In the latter case, we need to iterate in order to get to that object
  # Example: if we call environment_name(env=env1), this is what happens:
  #   environment_name(env=env1)    --> calls get_obj_address(obj=env, n=1)
  #   get_obj_address(obj=env, n=1) --> calls get_obj_name(obj=obj, n=1)
  # In this case we are interested in retrieving the value of parameter 'env' received by environment_name()
  # which in this case is variable 'env1', whose name ("env1") we are interested in retrieving
  # by calling environment_name()
  # This works as follows inside get_obj_name() called by get_obj_name(obj=obj, n=1) from get_obj_address():
  # 1) The first substitute(obj) call gives obj_parent = obj
  # 2) The while loop with nback = 0 gives expr = substitute(obj) and the expression 'obj_parent = 'substitute(obj)'
	# **evaluated in the calling function environment** (i.e. environment of get_obj_address()) via eval.parent()
	# gives 'env'.
  # 3) Only one loop is run (because the condition for continuing is nback < 1 in this case), and then we get
	# the value (or object name) of parameter 'env' in the environment that is n=1 levels up
	# (via the deparse(subsitute()) expression), which in this case this is the execution environment of
	# environment_name(env=env1). Clearly the value or object name of 'env' inside environment_name() is "env1",
	# which is what is returned to the outside world.
  obj_parent = substitute(obj)
  nback = 0
  while (nback < n) {
		expr = tryCatch(parse(text=paste("substitute(", obj_parent, ")")), error=function(e) if(!silent) error_NotValidExpression(obj_parent), silent=TRUE )
		if (!is.expression(expr)) return(NULL)
		obj_parent = eval.parent(expr, n=nback+1)
    nback = nback + 1
  }

  # Expression to evaluate at the parent.frame(n) environment where object 'obj_parent' needs to be 'substitute'd
  # with the value it received when the function was called
	expr = tryCatch(parse(text=paste("deparse(substitute(", obj_parent, "))")), error=function(e) if(!silent) error_NotValidExpression(obj_parent), silent=TRUE )
	if (!is.expression(expr)) return(NULL)

	# Get the object name (i.e. "x" either if obj = x (the variable x) or obj = "x" (the string "x"))
	# by evaluating the deparse(substitute()) expression in the environment of the calling function n levels up.
	obj_parent_name = eval(expr, envir=parent.frame(n+1))

	# Check if obj_parent was given as a variable name or as a string
	# This is checked by looking for starting and ending double quotes by looking for '\"'.
	# NOTE that it's important to look for thess quotes at the beginning and at the end of the name because
	# there may be quotes in the middle and they should not be removed!
	# (this is especially true when the name is e.g. "\"as.environment(\"package:stats\")\"" where we should
	# NOT remove the quotes enclosing 'package:stats'! --> this would make obj_find() fail when called on
	# as.environment("package:stats")$aov)
	if (length(grep("^\".*\"$", obj_parent_name)) > 0)  # this is TRUE when the value of obj_parent_name is already a string (e.g. obj_parent_name = "\"x\"" when obj_parent = "x")
		obj_parent_name = gsub("^\"|\"$", "", obj_parent_name)  # Remove the internal quotes from the object name

	return(obj_parent_name)
}
