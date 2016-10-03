#' Create a data frame with address-name pairs of environments
#' 
#' Return a data frame containing the address-name pairs of environments existing in the workspace
#' (including environments that are part of the search() path) or just the environments defined in
#' a given environment.
#' 
#' @param envir environment where environments are searched for. Defaults to NULL which means that all environments
#' in the whole workspace should be searched for and all packages in the \code{search()} path should be returned.
#' 
#' @return A data frame containing the following six columns:
#' \itemize{
#' \item{\code{type}} type of environment ("user" for user-defined environments or "system" for system or package environments).
#' \item{\code{location}} location of the environment (only for user-defined environments, in which case the system
#' environment or package where the environment resides is shown). Note that this may be different from the parent
#' environment if the parent environment was set during creation with the \code{parent=} option of the
#' \code{new.env()} function or using the \code{parent.env()} function.
#' \item{\code{address}} memory address of the environment. This is the key piece of information to get the
#' environment name with \code{environment_name()}.
#' \item{\code{pathname}} path to the environment an its name. This is the combination of columns
#' \code{path} and \code{name} whose values are put together separated by \code{$}.
#' \item{\code{path}} path to the environment (through e.g. different environments or packages).
#' \item{\code{name}} name of the environment.
#' }
#' The \code{type} column is used to distinguish between user-defined environments and package or system environments
#' which are also listed in the output data frame when \code{envir=NULL}.
#' 
#' The data frame is empty if no environments are found in the given environment.
#' 
#' \code{NULL} is returned when an error occurs.
#' 
#' @examples
#' # Create example of chained environments
#' env1 <- new.env()
#' with(env1, env11 <- new.env())
#' with(env1$env11, envx <- new.env())
#' 
#' # Address-name pairs of all environments defined in the workspace, including environments in the search path
#' get_env_names()  # returns a data frame with at least the following user environments: "env1", "env1$env11", "env1$env11$envx"  
#' 
#' # Address-name pairs of the environments defined in a given user-defined environment
#' get_env_names(env1)  # returns a data frame with the following user environments: "env11", "env11$envx" 
#' 
#' # Address-name pairs of the environments defined in a given package
#' get_env_names(as.environment("package:stats")) # should return an empty data frame (since the stats package does not have any environments defined) 
get_env_names = function(envir=NULL) {
  # Initialize the output lookup table to NULL in case the envir environment does not exist
	# or in case no environments are defined in the given environment
  env_table = NULL

	#-------------------------- 1. First level search of environments ---------------------------
	if (is.null(envir)) {
		# Look for environments defined in the WHOLE workspace (including packages listed in the search() path)
		# The following returns an ARRAY with the environments in all packages in the search() path
		# The names attribute of the array is the package where the environment stored in the corresponding element
		# is found.
		env_names = try( {
				# List of environments defined in each package 
				envs_list = sapply(  	search(),
															FUN=function(package) {
																# IMPORTANT: a package called "CheckExEnv" should be excluded from this analysis
																# because o.w. testing of the package during the Check process will fail with the
																# error "Error in get(x, envir = as.environment(package)) : F used instead of FALSE"
																# which is caused by an object called "F" in the CheckExEnv environment created
																# by the package Check process... (and there is another object called "T", I guess
																# an alias for TRUE). I don't know what this means...
																if (package != "CheckExEnv") {
																	Filter(function(x) "environment" %in% class(get(x, envir=as.environment(package))),
																			ls(envir=as.environment(package)))
																}
															}
													)
				envs_array = envnames:::unlist_with_names(envs_list)
				# Standardize the names of the environment so that the global and the base environments are always shown
				# the same way, regardless of how the 'envir' parameter is passed.
				names(envs_array) = sapply(names(envs_array), FUN=envnames:::standardize_env_name)
				envs_array
			},
			silent=TRUE)
	} else {
		# Look for environments present ONLY in the specified non-NULL envir environment (this can be a user-defined environment with new.env() or a package)
		# NOTE: Either of the two statements below work (one of them is commented out)
		# Note that in the first option we need to use the option envir=envir in the get() function,
		# and this is because the list of variables returned by ls(envir) reside in the envir environment.
		#env_names = try( Filter(function(x) "environment" %in% class(get(x, envir=envir)), ls(envir)), silent=TRUE )
		env_names = try( {
					envs = with(envir, Filter(function(x) "environment" %in% class(get(x)), ls()))
					envir_name = get_obj_name(envir, n=1)
					# Standardize the names of the environment so that the global and the base environments are always shown
					# the same way, regardless of how the 'envir' parameter is passed.
					envir_name = envnames:::standardize_env_name(envir_name)
					names(envs) = rep(envir_name, length(envs))
					envs
				}, silent=TRUE )
	}

	# Continue only if there was no error so far and if environments were found
  if (!inherits(env_names, "try-error")) {
		# Initialize the output table to a data frame
		env_table = data.frame(	type=character(0),
														location=character(0),
														address=character(0),
														pathname=character(0),
														path=character(0),
														name=character(0),
														stringsAsFactors=FALSE)
												
		# Continue processing if any environments were found
		if (length(env_names) > 0) {
			if (is.null(envir)) {
				# Get the address-name pairs of all system/package environments (e.g. .GlobalEnv, package:stats, package:base, etc.)
				allenvs = search()
				env_addresses_packages = vapply(search(), function(x) { get_obj_address(as.environment(x), envir=.GlobalEnv) }, FUN.VALUE=character(1))
				## NOTE: FUN.VALUE in the vapply() function is a required parameter.
				## It specifies the type and length of the value returned by the function called by vapply().
				## In this case (FUN.VALUE=character(1)) we are saying that the function should return
				## a vector of length 1 of type character.
			} else {
				# Set the addresses of packages to an array of 0 length (because this array is used below)
				env_addresses_packages = c()
			}
	
			#-------------------------- 2. Recursive search of environments ---------------------------
			# Extend the list of environments found in step 1 by recursively searching for environments within each
			# of those environments.
			# If envir=NULL, we now set it to .GlobalEnv so that the crawl function goes over all environments defined
			# in the workspace (i.e. the global environment)
			# Otherwise we recursively search just for the environments inside the given 'envir' environment. 
			if (is.null(envir)) {
				envir = .GlobalEnv
			}
			env_full_names = envnames:::unlist_with_names( tapply(	env_names,
																															INDEX=names(env_names),
																															FUN=envnames:::crawl_envs, c(), c(), envir) )
	
			# Get the environments referenced by the env_full_names names
			# NOTE that we use eval() instead of get() because eval() can evaluate environments given as e.g. env_of_envs$env11
			# but get() cannot... get() only does NOT accept the '$' sign, but accepts only calls of the form
			# get("env11", envir=env_of_envs) and creating these statements from the env_full_names content is more difficult.
			# Note also that there should not be any problem with the evaluation of env_full_names to environments because
			# these env_full_names are obtained from actually searching for environments through the crawl_envs() function above.
			#envs = lapply(env_full_names, get, envir=envir)
			envs = lapply(env_full_names, function(x) eval(parse(text=x), envir=envir))
	
			# Get the memory addresses of the environments just resolved
			env_addresses = eval( unlist( lapply(envs, get_obj_address, envir) ), envir=envir)

			# Complete with the rest of the info to store in the output data frame
			# Note: the type of address: "user" or "system". This information may be of interest when dealing
			# with environments and in particular is needed by obj_find() to know how to resolve the environment
			# where an object is found from the environment name, whether using eval() for user-defined environments
			# or as.environment() for system or package environments.
			env_types = c(rep("user", length(env_full_names)), rep("system", length(env_addresses_packages)))
			env_locations = c( names(env_full_names), rep(NA, length(env_addresses_packages)) )
			env_addresses = c(env_addresses, env_addresses_packages)
	    env_full_names = c(env_full_names, names(env_addresses_packages))
			# Separate the root and the name from env_full_names
			env_roots_and_names = sapply(env_full_names, FUN=envnames:::extract_last_member)
			env_paths = unlist( env_roots_and_names["root",] )
			env_names = unlist( env_roots_and_names["name",] )

			# Save the information in the env_table data frame
			env_table = data.frame(	type=env_types,
															location=env_locations,
															address=env_addresses,
															pathname=env_full_names,
															path=env_paths,
															name=env_names,
															stringsAsFactors=FALSE)
		} # if (length(env_names) > 0)
  } else  {
    envnames:::error_NotValidEnvironment(deparse(substitute(envir)))
  }

  return(env_table)
}
