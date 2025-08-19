#' Return the name of an object at a given parent generation from an environment
#'
#' A practical use of this function is to retrieve the name of the object leading to
#' a function's parameter in the function calling chain, at any parent generation.
#'
#' In particular, it provides a handy way of retrieving the name of a function's parameter
#' and use it in e.g. messages to the user describing the arguments received by the function.
#' In this context, it is a shortcut to calling \code{as.list(environment())}, which returns
#' a list of parameter names and parameter values.
#' See the Examples section for an illustration.
#'
#' @param obj object whose name at a given parent generation is of interest.
#' @param n number of parent generations to go back from the calling environment
#' to retrieve the name of the object that leads to \code{obj}
#' in the function calling chain. See details for more information.
#' @param eval whether to evaluate \code{obj} in the \code{n}-th parent generation before
#' getting the object's name in that environment. See details for more information.
#' @param silent when \code{FALSE}, the names of the environments and objects in those environments are printed
#' as those environments are traversed by this function.
#'
#' @return The name of the object in the \code{n}-th parent generation environment.
#'
#' @details
#' This function goes back to each parent generation from the calling function's environment
#' and at each of those parent generations it retrieves the name of the object that is part of
#' the parameter chain leading to the calling function's parameter.
#'
#' To illustrate: suppose we call a function \code{f <- function(x)} by running the piece of code \code{f(z)},
#' and that \code{f} calls another function \code{g <- function(y)} by running the piece of code \code{g(x)}.
#'
#' That is, we have the parameter chain:
#' \code{z -> x -> y}
#'
#' If, inside function \code{g()}, we call \code{get_obj_name()} as follows, we obtain respectively:
#' \code{get_obj_name(y, n=1)} yields \code{"x"}
#' \code{get_obj_name(y, n=2)} yields \code{"z"}
#'
#' because these calls are telling "give me the name of object \code{y} as it was called
#' \code{n} levels up from the calling environment --i.e. from the environment of \code{g()}.
#'
#' Note that the results of these two calls are different from making the following two
#' \code{deparse(substitute())} calls:
#' \code{deparse(substitute(y, parent.frame(n=1)))}
#' \code{deparse(substitute(y, parent.frame(n=2)))}
#' because these calls simply \code{substitute} or evaluate \code{y} at the \code{n}-th parent generation.
#' If \code{y} is not defined at those parent generations, the \code{substitute()} calls return
#' simply \code{"y"}.
#'
#' On the contrary, the previous two calls to \code{get_obj_name()} return the name of the object
#' in the parameter chain (\code{z -> x -> y}) \emph{leading} to \code{y}, which is a quite different
#' piece of information.
#'
#' When eval=TRUE, the result is the same as the result of \code{deparse()}
#' except for the following three cases:
#' \itemize{
#' \item if the object passed to \code{get_obj_name()} evaluates to a name, it returns that name,
#' without any added quotes. For example, if \code{v = "x"} then \code{get_obj_name(v, eval=TRUE)} returns \code{"x"}
#' while \code{deparse(v)} returns \code{"\"x\""}.
#' \item the result of \code{NULL} is \code{NULL} instead of \code{"NULL"} which is the case with \code{deparse()}.
#' \item the result of a non-existent object is \code{NULL}, while \code{deparse()} returns an error stating
#' that the object does not exist.
#' }
#'
#' When \code{get_obj_name} operates on non-existent objects it works at follows:
#' \itemize{
#' \item when \code{eval=FALSE} it returns the name of the non-existent object
#' enclosed in quotes (e.g. \code{get_obj_name(nonexistent)} returns \code{"nonexistent"}, assuming \code{nonexistent}
#' does not exist).
#' \item when \code{eval=TRUE} it returns NULL.
#' }
#'
#' Finally \code{get_obj_name(NULL)} returns \code{NULL}, while \code{as.character(NULL)} returns \code{as.character(0)}.
#'
#' @seealso
#' \link{get_obj_value}
#'
#' @examples
#' # Example 1:
#' # This example shows the difference between using get_obj_name() and deparse(substitute())
#' g <- function(y) { return(list(obj_name=get_obj_name(y, n=2, silent=FALSE),
#'                                substitute=deparse(substitute(y, parent.frame(n=2))) )) }
#' f <- function(x) { g(x) }
#' z = 3;
#' f(z)           # After showing the names of objects as they
#'                # are traversed in the parameter chain (silent=FALSE),
#'                # this function returns a list where
#'                # the first element (result of get_obj_name()) is "z"
#'                # and the second element (result of deparse(substitute())) is "y".
#'                # Note that 'z' is the object leading to object 'y'
#'                # inside function g() if we follow the parameter names
#'                # leading to 'y' in the function calling chain.
#'
#' # Example 2:
#' # When eval=TRUE, get_obj_name() behaves the same way as deparse()
#' # (except for the cases noted in the Details section)
#' # because the values of all objects linked by the parameter chain
#' # are ALL the same.
#' g <- function(y) { return(list(obj_name=get_obj_name(y, n=2, eval=TRUE),
#'                                deparse=deparse(y))) }
#' f <- function(x) { g(x) }
#' z = 3
#' f(z)           # Returns a list where both elements are equal to "3"
#'                # because the output of get_obj_name() with eval=TRUE
#'                # and deparse() are the same.
#'
#' # Example 3:
#' # This example shows how we can use get_obj_name() to get the parameter names
#' # of non '...' parameters, which are then used in messages to the user.
#' # The advantage of using get_obj_name() as opposed to the hard-coded parameter name
#' # is that an error is raised if the parameter does not exist.
#' # An example is also shown that uses as.list(environment()), which clearly is more
#' # general... get_obj_name() should be used when referring to a couple of specific
#' # parameters.
#' f <- function(x, y, ...) {
#'   cat("Arguments received by the function (using get_obj_name()) (explicit listing):\n")
#'   cat(get_obj_name(x), ":", x, "\n")
#'   cat(get_obj_name(y), ":", y, "\n")
#'   cat("Arguments received by the function (using as.list(environment())) (automatic listing):\n")
#'   paramsList = as.list(environment())
#'   paramsNames = names(paramsList)
#'   sapply(paramsNames, get_obj_name)
#'   for (p in paramsNames) {
#'     cat(p, ":", paramsList[[p]], "\n")
#'   }
#' }
#' z = 5
#' extra_param = "a '...' parameter"
#'   ## Note: this exra parameter is NOT shown neither by get_obj_name()
#'   ## nor by as.list(environment())
#' f("test", z, extra_param)
get_obj_name = function(obj, n=0, eval=FALSE, silent=TRUE) {
  # Increase n by 1 so that we do as if we were working in the environment of the calling function
  n = n + 1

  # Get the object of interest (i.e. the object in the calling environment) which will be the
  # starting point of the process performed in this function, including:
  # - evaluating the object in the n-th calling environment if eval=TRUE
  # - performing the iteration to evaluate the object in level n of the calling chain
  # going back from the current function.
  obj_parent = substitute(obj)

  if (!silent) {
    # Show the environment where the process starts: this is the environment of the calling function
    env_back = parent.frame(n=1)
    fun_calling = environment_name(env_back)
    cat("Start at environment ", fun_calling, ", object name is '", paste(deparse(obj_parent), collapse = "\n"), "'\n", sep="")
  }

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
    if (is.environment(obj_parent)) {
      obj_parent = eval.parent(obj_parent, nback)
    } else {
      expr = parse(text=paste("substitute(", paste(deparse(obj_parent), collapse = "\n"), ")"))
      obj_parent = eval.parent(expr, nback)
    }
    if (!silent) {
      # Get the environment where the name of obj_parent is retrieved using deparse(substitute()) (called recursively of course, as done in this loop)
      # Note that this environment is nback+1 levels up (and NOT nback levels up) because the environment nback levels up is where the call to
      # substitute(obj_parent) is done, which actually retrieves the value (which normally is a variable name) of obj_parent one level up from where
      # substitute() is called. Showing that environment makes the message more understandable. If you don't believe me try using parent.frame(n=nback)
      # here and looking at the messages shown when n > 0...
      # Still the level number we show in the message is 'nback' and NOT 'nback+1' because the relative level should refer to the function that calls
      # get_obj_name(), which is what the user "sees".
      env_back = parent.frame(n=nback+1)
      fun_calling = environment_name(env_back)
      cat("Level ", nback, " back: environment = ", fun_calling, ", object name is '", paste(deparse(obj_parent), collapse = "\n"), "'\n", sep="")
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

  # Now get the object name (either evaluated or not)
  # unless the object is NULL in which case we keep those values.
  # A special case needs to be considered when the object is an environment
  # because calling deparse() on those objects returns "<environment>"
  # as opposed to the name of the environment.
  if (is.null(obj_parent)) {
    obj_parent_name = obj_parent
  } else if (is.environment(obj_parent)) {
    obj_parent_name = environment_name(obj_parent)
  } else {
    # In regular cases, get the object name by simply calling deparse()
    obj_parent_name = paste(deparse(obj_parent), collapse = "\n")
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
