#' Return the chain of calling functions
#' 
#' Return a data frame with the calling stack of function calls, or optionally the information on one
#' particular function in this chain.
#' This function is intended to be called only within a function.
#' 
#' @param n non-negative integer specifying the number of levels to go up from the calling function
#' to get the calling function of interest in the calling chain.
#' When \code{n} is not \code{NULL} the information on the calling function \code{n} levels up
#' (from the calling function) is returned.
#' Defaults to \code{NULL}, in which case all the functions calling chain is returned.
#' 
#' @param envmap data frame containing a lookup table with name-address pairs of environment names and
#' addresses to be used when creating the function calling chain. Defaults to NULL which means that the
#' lookup table is constructed on the fly with the environments defined in the Global Environment.
#' It is useful to speed up the process of computing the fuctions calling chains if it is known that
#' the environments in the workspace will not change. 
#' See the @details for more information on its structure.
#' 
#' @param silent whether to run in silent mode. If FALSE, how the function crawls the different function
#' environments is shown. Defaults to \code{TRUE}. 
#' 
#' @return A data frame with the following columns:
#' \itemize{
#' \item{\code{level}:} the stack level going back in the function call chain, where 0 indicates the function where
#' \code{get_fun_calling_chain} was called from
#' \item{\code{fun}:} the function name
#' \item{\code{env}:} the environment where the function is defined as returned by \code{environment()}
#' \item{\code{envfun}:} the environment where the function is defined together with the function name separated by
#' a \code{$} sign. Ex: \code{env1$f}
#' }
#' 
#' @details
#' If \code{envmap} is passed it should be a data frame with at least the following columns:
#' \itemize{
#' \item{\code{address}} memory address of the environment
#' \item{\code{pathname}} environment name with the full path to it as in e.g. "env$env1$envx"
#' }
#' This table can be constructed by running get_env_names() once.
get_fun_calling_chain = function(n=NULL, envmap=NULL, silent=TRUE) {
	#------------------------------------ Parse input parameters --------------------------------
	# Setup the address-name pairs of all environments defined in the workspace
	# (we do this once so that we don't need to do it every time the environment_name() function is called
	# in the loop below!)
	if (is.null(envmap)) {
		envmap = get_env_names(envir=NULL)
	}
	#------------------------------------ Parse input parameters --------------------------------

  # Get the calling function's environment
	# Note that whenever we call a function to retrieve calling environments we use nback+1, since
	# the first calling function is the function that called get_fun_calling_chain()!
	# But whenever we display or store the number of levels corresponding to the returned function
	# we show nback, because that's what matters to the user, i.e. how many levels we are up
	# from the function that called get_fun_calling_chain().
  nback = 0											# nback measures the number of levels to go back from the *function calling get_fun_calling_chain()*
																# This means that nback has the same reference value as n (e.g. nback = 1 is equivalent to n = 1) 
  env = parent.frame(nback+1)		# This is equivalent to sys.frame(sys.parent(nback+1)) but is implemented slightly more efficient (ref: documentation)
  # Get the environment where the calling function is defined (normally the Global Environment)  
  env_enclosing = parent.env(env)

	# Create the data frame where the calling functions information is stored
	# This is only needed when n is NULL but we still do it when only one calling function is requested
	# to save time with IF statements
	fun_calling_chain = data.frame(level=numeric(0), fun=character(0), env=character(0), envfun=character(0), stringsAsFactors=FALSE)

	# Iterate on the history of calling functions up to level n if n is not NULL
  while (!identical(env, globalenv())) {
		# Get the address of the execution environment of the calling function at level n
    env_address = get_obj_address(env)
			## NOTES:
			## - No need to enclose 'env' in quote()
			## - No need to specify the environment where 'env' is defined (i.e. the enclosing environment)

		# Get the name of the enclosing environment of the calling function at level n
		# It's important that we first try to retrieve the environment name using the built-in
		# environmentName() function because the enclosing environment could be a package namespace
		# and namespaces are not part of the search() environments... so they don't show up in the environment
		# address-name pairs table created by get_env_names(). 
		# This is in fact the case when using the wrapper get_fun_calling() to call get_fun_calling_chain()
		# since the enclosing environment of get_fun_calling() is "namespace:envnames"!! 
		env_enclosing_name = environmentName(env_enclosing)
		if (env_enclosing_name == "") {
			env_enclosing_name = environment_name(env_enclosing, envmap=envmap)
			if (is.null(env_enclosing_name)) {
				# This happens when the environment is not found in the envmap table (this should not happen!)
				env_enclosing_name = "<NA>"
			}
		}

		# Get the name of the calling function at level n
		calling_fun_name = get_fun_name(nback+1)

		# Show detailed info
		if (!silent) {
			cat("\n* nback:", nback, ", env_enclosing_name", env_enclosing_name, "\n")
			cat("Address of calling environment at level", nback, ":", env_address, "\n")
			cat("Name of enclosing environment of calling function at level", nback, ":", env_enclosing_name, "\n")
			cat("Name of calling function at level", nback, ":", calling_fun_name, "\n")
		}

		# Add the information on the calling function to the output data frame
		envfun = paste(env_enclosing_name, "$", calling_fun_name, sep="")
		fun_calling_chain[nback+1,] = c(nback, calling_fun_name, env_enclosing_name, envfun)

		# Stop if we have reached level n (when n is not NULL)
		# (recall that n is the number of levels to go back from the user's function, NOT from the get_fun_calling_chain() function!)
		if (!is.null(n) && nback == n) break;

		# Get the environment of the calling function at the next level of the calling chain and its environment
    nback = nback + 1
    env = parent.frame(nback+1)
    env_enclosing = parent.env(env)
  }
	if (!silent && identical(env, globalenv())) {
  	cat("Reached global environment\n")
	}

	# Returned data frame (either the full chain or just one entry)
	if (is.null(n) || n <= 0) {
		return(fun_calling_chain)
	} else {
		# Return just the information on the calling function n levels back from the function calling get_fun_calling_chain()
		# This function may not exist if n is too large, i.e. larger than the number of levels stored in the
		# fun_calling_chain data frame.
		return(fun_calling_chain[fun_calling_chain$level==n,"envfun"])
	}
}
