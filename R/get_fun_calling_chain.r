#' Return the chain of calling functions
#' 
#' Return a data frame with the stack or chain of function calls, or optionally the information on one
#' particular function in this chain.
#' 
#' @param n non-negative integer specifying the level of interest in the function calling chain,
#' where 0 means the function calling \code{get_fun_calling_chain}.
#' It defaults to \code{NULL}, in which case the full chain is returned.
#' @param showParameters flag indicating whether the parameters of the function call should also be shown
#' in the output.
#' @param silent whether to run in silent mode. If FALSE, the calling chain is shown in an intuitive way.
#' It defaults to \code{TRUE}.
#' 
#' @return 
#' If \code{n=NULL} (the default) a data frame with the function calling chain information, 
#' with the following columns:
#' \itemize{
#' \item{\code{fun}:} the function name (including parameters if \code{showParameters=TRUE})
#' \item{\code{env}:} the function's enclosing enviroment, i.e. the environment where the function is defined
#' as returned by \code{environment(<function>)}
#' \item{\code{envfun}:} the environment where the function is defined together with the function name (and its parameters
#' if \code{showParameters=TRUE}) separated by a \code{$} sign. Ex: \code{env1$f()}
#' }
#' The rownames of the data frame are the stack level of the function calls in the chain,
#' from 0 up to the number of functions in the chain, where 0 indicates the current function
#' (i.e. the function that called \code{get_fun_calling_chain}).
#' 
#' The functions in the data frame are sorted from most recent to least recent call, much like the common
#' way of displaying the function stack in debug mode.
#' 
#' If the function is NOT called from within a function, \code{NULL} is returned.
#' 
#' If \code{n} is not NULL and is non-negative, the environment and the function name (including parameters
#' if \code{showParameters=TRUE}) separated by a \code{$} sign are returned (ex: \code{env1$f(x = 3, n = 1)}).
#' 
#' if \code{n < 0} or if \code{n} is larger than the function calling chain length, \code{NULL} is returned.
get_fun_calling_chain = function(n=NULL, showParameters=FALSE, silent=TRUE) {
  # Get the calling chain using sys.calls()
  # Note that sys.calls() returns a list where the most recent call is last in the list
  # (here I will reverse this order because I want the most recent call to be shown first)
  all_calls = sys.calls()
  ncalls = length(all_calls)

  # Extreme case: when this function is called from a non-function environment
  if (ncalls == 1 || (!is.null(n) && n < 0)) {
    # This means that the parent frame (calling environment) is the global environment
    # or that the given value of n is invalid (negative values are invalid)
    if (!is.null(n) && n < 0 && !silent) cat("Parameter n cannot be negative;", n, "was given.\n")
    return(NULL)
  }

  # Create the output data frame where the calling functions information is stored
  # This is only needed when n is NULL but we still do it when only one calling function is requested
  # to save time with IF statements checking that parameter 'n' was passed.
  fun_calling_chain = data.frame(fun=character(0), env=character(0), envfun=character(0), stringsAsFactors=FALSE)

  # Check whether n is given and has a nonnegative value
  return_just_one_function = ifelse(!is.null(n), TRUE, FALSE)

  # Define the last iteration level for the calling chain    
  if (return_just_one_function) {
    nlast = n + 1   # +1 because when n=0, it means that the user is interested in the name of the function CALLING this function.
  } else {
    nlast = ncalls - 1
  }

  # Check whether nlast is out of range. Return NA if this is the case
  if (nlast >= ncalls) {
    # This means that there is no function calling at level n
    # (where n is measured w.r.t. the function CALLING this function;
    # note that nlast is measured w.r.t. the current function and in both cases
    # the level of the reference function (i.e. the "w.r.t." function) is 0)
    return(NULL)
  }

  # Iterate on all the calls from "previous to latest" to the first one so that we go UP in the calling chain
  # Note that we exclude the latest call (we can see this because the iteration starts at level l=1 and not at l=0)
  # because the last call (i.e. level l=0) is THIS function get_fun_calling_chain(), on which we are not interested
  # in including in the function calling chain (i.e. the user doesn't care about it!)
  for (l in 1:nlast) {
    # Compute the index c in the sys.calls() output that corresponds to level l up
    # (note that index c goes in the opposite direction of the level l)
    c = ncalls - l

    # Get the function at call level c
    fun = sys.function(c)

    # Get the function name and parameters (this includes the environment where the function is defined)
    if (showParameters) {
      fun_name = deparse(all_calls[[c]])          # Use this if we want to show function name AND its parameters
    } else {
      fun_name = as.character(all_calls[[c]])[1]  # Use this if we want to show just the function name, NOT its parameters
                                                  # Note that as.character() separates each part in its input that is enclosed in quotes into a separate row
    }

    # Get the environment where the function is defined
    # NOTE: env_name is NEVER empty! (not even for user-defined environments)
    env_name = environmentName(environment(fun))
    if (env_name == "") {
      # This is the case for user-defined environments, which are unnamed
      # In this case, the environment name is part of the function name!
      # => Extract the environment part from the function name and set it as the value of env_name
      env_and_fun = extract_last_member(fun_name)
      env_name = env_and_fun$root
      fun_name = env_and_fun$name
    }
    # Convert fun_name into a single string in case it's given as an array of strings...
    # (this happens for instance when the function is cat() and the string inside cat() is constructed using different
    # arguments --as in cat("This is test number", num, "\n"), where fun_name is an array of 2 elements
    # as returned by the call to deparse(all_calls[[c]]) above (of course this is only a problem when showParameters=TRUE))
    fun_name = paste(fun_name, collapse="")

    # Update the data frame containing the functions in the calling chain
    # Note that the l-th entry contains the c-th function in the calling chain (where c = ncalls - l),
    # therefore making the first function stored in the fun_calling_chain data frame be the latest function called.
    fun_calling_chain[l,] = c(fun_name, env_name, paste(env_name, fun_name, sep="$"))

    # Debug
    #cat("level:", l, "fun:", fun_name, "memory:", address(sys.frame(-l)), "\n")
  }

  # Returned data frame (either the full chain or just one entry)
  if (return_just_one_function) {
    return(paste(env_name, fun_name, sep="$"))
  } else {
    if (!silent) {
      # Show the calling chain in an intuitive way
      # (this is done by reversing the order of the fun_calling_chain rows and
      # adding arrows to connect the different functions)
      # Note: no need to check that ncalls > 0 because no error is given when ncalls = 0
      # even if the data frame fun_calling_chain has no rows! (in fact, NA is shown in that case)
      cat("Function calling chain:\n", paste(fun_calling_chain$envfun[(ncalls-1):1], collapse=" -> "), "\n")
    }
    # Add the row names so that the innermost level is 0
    rownames(fun_calling_chain) = 1:(ncalls-1) - 1
    return(fun_calling_chain)
  }
}
