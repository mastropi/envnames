#' Return the name of an object referenced by a parameter at any parent generation
#' 
#' Return the name of the object at the specified parent generation leading to a function's parameter.
#' The object is optionally evaluated in the environment of the specified parent generation
#' before retrieving its name, in the same way the deparse() function does.
#' 
#' @param obj object whose name is of interest, or whose evaluated name is of interest.
#' @param n number of parent generations to go back to retrieve the name of the object that leads to \code{obj}
#' in the function calling chain. See details for more information.
#' @param eval whether to evaluate \code{obj} in the \code{n}-th parent generation before
#' getting the object's name in that environment.
#' @param silent when \code{FALSE}, the names of the environments and objects in those environments are printed
#' as those environments are traversed by this function.
#' 
#' @return The name of the object in the \code{n}-th parent generation environment.
#' 
#' @details To better understand the meaning of this function when \code{n} > 0, consider the first example below:  
#' Function \code{f(x)} is called with parameter \code{z}, which calls \code{g(y)} with parameter \code{x}.
#' The result of the call to \code{get_obj_name()} from function \code{g()} is \code{"z"}
#' because that call is telling "give me the name of object \code{y} two levels up from
#' the current environment, i.e. from the environment of \code{g()}. The name of \code{y} in the environment
#' of \code{f(x)} is \code{"x"}, and the name of \code{y} in the global environment is \code{"z"}.  
#' 
#' When eval=TRUE, the result of the function is the same as the output of \code{deparse()} except for the following two cases:
#' \itemize{
#' \item the result of \code{NULL} is \code{NULL} instead of \code{"NULL"} which is the case with \code{deparse()}
#' \item when \code{eval=TRUE}, if the object passed to \code{get_obj_name()} evaluates to a name, it returns that name,
#' without added quotes. For example, if \code{v = "x"} then \code{get_obj_name(v, eval=TRUE)} returns \code{"x"} while
#' \code{deparse(v)} returns \code{"\"x\""}.
#' }
#' 
#' @seealso
#' \link{get_obj_value}
#' 
#' @examples
#' # In its default behaviour (eval=FALSE), get_obj_name() returns the name of an object in the n-th parent generation.
#' g <- function(y) { get_obj_name(y, n=2) }
#' f <- function(x) { g(x) }
#' z = 3; 
#' f(z)                # Prints "z", which is the object leading to object "y" inside function g() if we follow the function calling chain.
#'
#' # When eval=TRUE, get_obj_name() behaves the same way as deparse(), except for the cases noted above.
#' g <- function(y) {
#'   print(get_obj_name(y, n=2, eval=TRUE));
#'   print(deparse(y))
#' }
#' f <- function(x) { g(x) }
#' z = 3; 
#' f(z)                # Prints "3" twice, once as the output of get_obj_name(), once as the output of deparse()
get_obj_name = function(obj, n=0, eval=FALSE, silent=TRUE) {
  # Increase n by 1 so that we do as if we were working in the environment of the calling function
  n = n + 1

  # Get the object of interest (i.e. the object in the calling environment) which will be the
  # starting point of the process performed in this function, including:
  # - evaluating the object in the n-th calling environment if eval=TRUE
  # - performing the iteration to evaluate the object in level n of the calling chain
  # going back from the current function.
  obj_parent = substitute(obj)

  # Iterate on the calling chain to get the name of the object passed to thie function
  # Ex: starting with z we may have called f(x) which in turns calls g(y) and g(y)
  # calls this function with n=2, which means: "give me the name of the object that
  # led to object 'y' 2 levels up in the calling chain". This name is "z".
  # note that the name of object 'y' is "y" in any environment if we do not follow this
  # chain logic).
  nback = 1
  while (nback < n) {
    #cat("nback =", nback, "\n")
    #print(deparse(obj_parent))
    # TODO: Should we use environmentName() instead of deparse() when obj_parent is a named environment?
    # (as done at the very end below when retrieving the name of obj_parent)
    expr = parse(text=paste("substitute(", deparse(obj_parent), ")"))
    obj_parent = eval.parent(expr, nback)
    if (!silent) {
      # Get the name of the function environment that corresponds to the parent frame nlevels back
      env_back = parent.frame(n=nback)
      fun_calling = environment_name(env_back)
      cat("Level ", nback, " back: environment = ", fun_calling, ", object evaluates to '", deparse(obj_parent), "'\n", sep="")
    }
    nback = nback + 1
  }

  # When eval=TRUE, first evaluate the object in the chain n levels up
  # (evaluation happens in that very same environment --i.e. in the environment n levels up)
  # NOTES:
  # a) It's important to use parent.frame(n) and NOT sys.frame(-n) which allows to avoid issues when for instance
  # get_obj_name() is enclosed in a print() function: in such case, sys.frame(-n) will include the print() environment
  # in the chain when looking for environment n levels back, while parent.frame(n) will NOT --and this is what we want,
  # i.e. to not have the result of get_obj_name() depend on whether we enclose its call in a print() function or not
  # (and any other similar situation)
  # For more info on the differences between parent.frame(n) and sys.frame(-n) see the Note in the documentation of sys.parent()
  # b) the result of the object evaluation is the same in all parent frames when the object's value is not changed by any
	# function of the chain. In fact: if z -> f(x) -> g(y) (where '->' means "calls") and the parameter passed to the
	# different functions (z, x, y) does not change, then the the value of y in g(y) is the same as the value of x in f(x)
	# which is the same as the value of z. However, the values may differ if each function changes the value passed as input
	# parameter before calling the next function in the chain.
  if (eval) {
    obj_parent_eval = try( eval( obj_parent, parent.frame(n) ), silent=TRUE )
    # Check if the evaluation gives an error
    # If so, set the evaluated value to NULL (e.g. the object does not exist in the given parent environment)
    if (inherits(obj_parent_eval, "try-error")) {
      obj_parent = NULL
    } else {
      # Re-establish the original value of obj prior to evaluation
      obj_parent = obj_parent_eval
    }
  }

  # Now get the object name (either evaluated or not) unless the object is NULL in which case we keep those values
  # A special case needs to be considered when the object is a named environment
  # (e.g. global environment, package environments, etc.) because those objects
  # cannot be cast to string with regular functions dealing with strings
  # (as e.g. cat(), grep(), etc. --in particular grep() is used below)
  if (is.null(obj_parent)) {
    obj_parent_name = obj_parent
  } else if (is.environment(obj_parent) && environmentName(obj_parent) != "") {
    obj_parent_name = environmentName(obj_parent)
  } else {
    # In regular cases, get the object name by simply calling deparse()
    obj_parent_name = deparse(obj_parent)
  }

  # Check if obj_parent was already a string before deparsing
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
