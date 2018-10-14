#' Return the value of the object at a given parent generation leading to the specified object
#'
#' This function is mostly useful in debugging contexts to see the name of the variables
#' that lead to a particular object through the calling stack (which however can also be
#' obtained by running \code{get_obj_name()}).
#' In fact, the value of an object is *the same troughout all the objects* in the different
#' generations leading to the queried object, meaning that such value can be obtained
#' by simply referencing the object at the calling environment.
#'
#' See the Details and Examples sections below for a comparison with the result of using
#' \code{eval()} and \code{evalq()}.
#' 
#' @param obj object whose value should be returned. The object can be passed either as a variable name or
#' as a string representing the object whose value is of interest.
#' @param n number of parent generations to go back to retrieve the value of the object that leads to \code{obj}
#' in the function calling chain. See details for more information.
#' @param silent when \code{FALSE}, the names of the environments and objects in those environments are printed,
#' as those environments are traversed by this function.
#' 
#' @details
#' The purpose of this function is to get the value of objects as they are passed through different functions
#' (although, as explained in the Description section the value is the same in all environments).
#' 
#' Note that conceptually this is NOT the same as calling \code{evalq(obj, parent.frame(n))},
#' because of the following:
#' \itemize{
#' \item \code{evalq()} evaluates the object named \code{obj} in the environment that is at the
#' \code{n}-th parent generation. (Note the use of \code{evalq()} and not \code{eval()} because 
#' the latter evaluates the object at the calling environment first, before passing it
#' for evaluation to the given parent environment.)
#' \item \code{get_obj_value()} first looks for the object in the \code{n}-th parent generation
#' that \emph{led} to the \code{obj} object in the calling environment (i.e. the environment
#' that calls \code{get_obj_value()} and only \emph{then} evaluates it at the \code{n}-th parent generation.
#' }
#' 
#' The job performed by \code{get_obj_value()} is done as follows:
#' at each parent generation, there is a pair of "object name" <-> "object value".
#' The task of this function is to retrieve the object name at a given parent generation
#' and then its value based on the "path" (of variable names) that leads to the variable
#' in the function that calls \code{get_obj_value()}.
#' 
#' In practice though --as explained above-- the result of \code{get_obj_value()} is the same as the value
#' of the queried object at the calling function, since the value of the variables leading
#' to that object are all the same through the calling stack.
#' 
#' Therefore, using \code{get_obj_value()} doesn't add useful information to the use
#' of \code{eval()} or simply to referencing the object at the calling function, UNLESS
#' we set parameter \code{silent=FALSE}, in which case the function shows the name of the different
#' variables that lead to the queried object in the calling function. An example is given
#' in the Examples section
#' 
#' If the \code{obj} is given as a string, it also evaluates to the object value when an object
#' with that name exists in the given parent generation. However, the object should be passed
#' with no explicit reference to the environment where it is defined.
#' For instance we should use \code{with(env1, get_obj_value("z"))} and
#' \emph{not} \code{get_obj_value("env1$z")}, which returns simply \code{"env1$z"}.
#' 
#' @return The value of the object as described in the Details section.
#' 
#' @seealso \code{get_obj_name()} which returns the *name* of the object in the calling stack
#' leading to the queried object in the calling environment.
#' 
#' @examples
#' # Example of using get_obj_value() from within a function
#' # The value returned by get_obj_value() is compared to the values returned by eval() and evalq()
#' compareResultsOfDiferentEvaluations <- function(x) {
#'   cat("Looking at the path of variables leading to parameter 'x':\n")
#'   xval = get_obj_value(x, n=1, silent=FALSE)
#'   cat("Value of 'x' at parent generation 1 using get_obj_value():", xval, "\n")
#'   cat("Value of 'x' at parent generation 1 using eval():", eval(x, parent.frame(1)), "\n")
#'   cat("Value of 'x' at parent generation 1 using evalq():", evalq(x, parent.frame(1)), "\n")
#' }
#' g <- function(y) {
#'   x = 2
#'   compareResultsOfDiferentEvaluations(y)
#' }
#' z = 3
#' g(z) 
#'    ## Note how the result of get_obj_value() is the same as eval() (=3)
#'    ## but not the same as evalq() (=2) because the queried object (x)
#'    ## exists in the queried parent generation (g()) with value 2.
#'    ## The results of eval() and get_obj_value() are the same but
#'    ## obtained in two different ways:
#'    ## - eval() returns the value of 'x' in the calling function (even though
#'    ## the evaluation environment is parent.frame(1), because eval() first
#'    ## evaluates the object in the calling environment)
#'    ## - get_obj_value() returns the value of 'y' in the parent generation
#'    ## of the calling function (which is the execution environment of g())
#'    ## since 'y' is the variable leading to variable 'x' in the calling function.
#'    ##
#'    ## NOTE however, that using get_obj_value() does NOT provide any new
#'    ## information to the result of eval(), since the variable values are
#'    ## transmitted UNTOUCHED through the different generations in the
#'    ## function calling chain.
#'    ## FURTHERMORE, the same value is returned by simply referencing 'x'
#'    ## so we don't need neither the use of get_obj_value() nor eval().
#'    ## The only interesting result would be provided by the evalq() call
#'    ## which looks for variable 'x' at the parent generation and evaluates it.
#' 
#' # Example of calling get_obj_value() from outside a function
#' x = 3
#' v = c(4, 2)
#' get_obj_value(x)         # 3
#' get_obj_value("x")       # 3
#' get_obj_value(3)         # 3
#' get_obj_value(v[1])      # 4
#' 
get_obj_value = function(obj, n=0, silent=TRUE) {
	# Note that the evaluation in eval() is done on parent.frame(n+1), i.e. on the environment n+1 levels up
	# from this function, which is the same environment where the object's name is retrieved from
  # (by get_obj_name(obj, n=n+1)). In fact:
	# - the name of the object to be evaluated is returned by get_obj_name(obj, n=n+1)
  # (i.e. the name of the object in the environment that is n levels up from the function that called get_obj_value())
	# - this parameter needs to be evaluated in that environment (parent.frame(n+1))
	# (which is n+1 levels up from get_obj_value())
	value = try( eval(as.symbol(get_obj_name(obj, n=n+1, silent=silent)), parent.frame(n+1)), silent=TRUE )  # The silent=TRUE is for the try() call...
	if (inherits(value, "try-error")) {
		return(obj)
	} else {
		return(value)
	}
}
