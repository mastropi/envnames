#' Return the chain of calling functions
#' 
#' Returns a data frame with the chain of functions leading to the function from which
#' \code{get_fun_calling_chain} is called, or optionally it returns the information on one
#' particular function in this chain.
#' 
#' @param n positive integer specifying the number of levels that defines which calling function should
#' be returned. When n is not NULL the information on the calling function n levels back from the function
#' calling get_fun_calling_chain() is returned.
#' Defaults to NULL, in which case all the function calling chain is returned.
#' 
#' @param envmap data frame containing a lookup table with name-address pairs of environment names and
#' addresses to be used when creating the function calling chain. Defaults to NULL which means that the
#' lookup table is constructed on the fly with the environments defined in the Global Environment.
#' See the @details for more information on its structure.
#' 
#' @details
#' If \code{envmap} is passed it should be a data frame with at least the following columns:
#' \code{name} and \code{address}, where \code{name} contains the environment name and \code{address}
#' contains its memory address. 
get_fun_calling_chain = function(n=NULL, envmap=NULL) {
	#------------------------------------ Parse input parameters --------------------------------
	# Setup the address-name pairs of all environments defined in the workspace
	# (we do this once so that we don't need to do it every time the environment_name() function is called
	# in the loop below!)
	# TODO: (2016/08/13) Check if this process works as at the time of the writing get_env_names() does not crawl
	# over ALL environments defined in the workspace.
	if (is.null(envmap)) {
		envmap = get_env_names(envir=.GlobalEnv)
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
		# and namespaces are not part of the search() environments...
		# This is in fact the case when using the wrapper get_fun_calling() to call get_fun_calling_chain()
		# since the enclosing environment of get_fun_calling() is "namespace:envnames"!! 
		env_enclosing_name = environmentName(env_enclosing)
		if (env_enclosing_name == "") {
			env_enclosing_name = environment_name(env_enclosing, envmap=envmap)
		}

		# Get the name of the calling function at level n
		calling_fun_name = get_fun_name(nback+1)

		# Show detailed info
#		cat("* nback:", nback, ", env_enclosing_name", env_enclosing_name, "\n\n")
#   cat("Address of calling environment at level", nback, ":", env_address, "\n")
#   cat("Name of enclosing environment of calling function at level", nback, ":", env_enclosing_name, "\n")
#		cat("Name of calling function at level", nback, ":", calling_fun_name, "\n")

		# Add the information on the calling function to the output data frame
		envfun = paste(env_enclosing_name, "$", calling_fun_name, sep="")
		fun_calling_chain[nback+1,] = c(nback, calling_fun_name, env_enclosing_name, envfun)

		# Stop if we have reached level n (when n is not NULL) 
		if (!is.null(n) && nback == n) break;

		# Get the environment of the calling function at the next level of the calling chain and its environment
    nback = nback + 1
    env = parent.frame(nback+1)
    env_enclosing = parent.env(env)
  }
	if (identical(env, globalenv())) {
  	cat("Reached global environment\n")
	}

	# Returned data frame (either the full chain or just one entry)
	if (is.null(n) || n <= 0) {
		return(fun_calling_chain)
	} else {
		# Return just the information on the calling function n levels back from the function calling get_fun_calling_chain()
		return(fun_calling_chain[n,"envfun"])
	}
}
