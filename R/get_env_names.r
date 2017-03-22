#' Create a lookup table with address-name pairs of environments
#' 
#' Return a data frame containing the address-name pairs of user-defined, system, package, and namespace
#' environments in the workspace or within a given environment.
#' 
#' @param envir environment where environments are searched for. Defaults to NULL which means that all environments
#' in the whole workspace should be searched for and all packages in the \code{search()} path should be returned
#' including their namespace environments.
#' 
#' @details
#' The search for environments is recursive, meaning that a search is carried out for environments defined
#' within other environments.
#' 
#' The search within packages is always on \emph{exported objects} only.
#' 
#' If \code{envir} is the global environment ,the lookup table includes all system,
#' package, and namespace environments in the \code{search()} path, as well as all user-defined environments
#' found in the global environment (with recursive search).
#' 
#' If \code{envir=NULL} the lookup table includes all system, package, and namespace environments
#' in the \code{search()} path, as well as all user-defined environments found in \emph{any} of those
#' environments (with recursive search).
#' 
#' @return A data frame containing the following six columns:
#' \itemize{
#' \item{\code{type}} type of environment ("user" for user-defined environments "system/package"
#' for system or package environments), and "namespace" for namespace environments.
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
#' The \code{type} column is used to distinguish between user-defined environments, package or system environments,
#' and namespace environments, which are also listed in the output data frame when \code{envir=NULL}.
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
	### 1a) Look for user-defined environments either within the given 'envir' environment or
	### within the whole workspace (i.e. within all system and package environments reachable via the search() path)
	if (is.null(envir)) {
	  envir_orig = NULL
	  # If envir=NULL, we now set it to .GlobalEnv because all the process following this step should be
	  # the same as when the user passed envir=.GlobalEnv (the only different part of this process is precisely
	  # this step where we look for environments in ALL loaded packages --which note, does not use the 'envir' variable).
	  envir = .GlobalEnv

		# Look for user-defined environments in the WHOLE workspace (including packages listed in the search() path)
		# The following returns an ARRAY with the environments in all packages in the search() path
		# The names attribute of the array is the package where the environment stored in the corresponding element
		# is found.
		env_user_names = try( {
				# List of user-defined environments defined in each package 
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
	  envir_orig = envir
		# Look for environments present ONLY in the specified non-NULL envir environment (this can be a user-defined environment with new.env() or a package)
		# NOTE: Either of the two statements below work (one of them is commented out)
		# Note that in the first option we need to use the option envir=envir in the get() function,
		# and this is because the variables returned by ls(envir) reside in the envir environment.
		#env_user_names = try( Filter(function(x) "environment" %in% class(get(x, envir=envir)), ls(envir)), silent=TRUE )
		env_user_names = try( {
					envs = with(envir, Filter(function(x) "environment" %in% class(get(x)), ls()))
#					envir_name = get_obj_name(envir, n=1)		# get_obj_name() is equivalent to deparse(substitute(envir)) in this case 
					envir_name = deparse(substitute(envir))
					# Standardize the names of the environment so that the global and the base environments are always shown
					# the same way, regardless of how the 'envir' parameter is passed.
					envir_name = envnames:::standardize_env_name(envir_name)
					names(envs) = rep(envir_name, length(envs))
					envs
				}, silent=TRUE )
	}

	# Continue only if there was no error so far
  if (!inherits(env_user_names, "try-error")) {
		# Initialize the output table to a data frame
		env_table = data.frame(	type=character(0),
														location=character(0),
														address=character(0),
														pathname=character(0),
														path=character(0),
														name=character(0),
														stringsAsFactors=FALSE)
												
		# Continue processing if either:
		# - at least one environment was found or
		# - the whole workspace is searched for in which case we should include all system, package, and
		# namespace environments in the lookup table.
		# (Note that this is the case when the user passed either envir=NULL or
		# envir=.GlobalEnv, but since in the former case envir was set to .GlobalEnv (at the very top)
		# here we just test for envir=.GlobalEnv to take care of this case.)
		if (length(env_user_names) > 0 || is.null(envir_orig)) {
			if (is.null(envir_orig)) {
				### Search for environments should be carried out in all the workspace 
				### => The environments defined in the whole workspace must be retrieved 
				### 1b) Get the address-name pairs of all system/package/namespace environments (e.g. .GlobalEnv, package:stats, package:base, etc.)
				allenvs = search()

				#-- System and packages
				env_addresses_packages = vapply(search(), function(x) { get_obj_address(as.environment(x), envir=.GlobalEnv) }, FUN.VALUE=character(1))
  				## NOTE: FUN.VALUE in the vapply() function is a required parameter.
  				## It specifies the type and length of the value returned by the function called by vapply().
  				## In this case (FUN.VALUE=character(1)) we are saying that the function should return
  				## a vector of length 1 of type character.

				#-- Namespaces
				# Store the namespace environment of each package as well!
				# (for instance, the functions defined by each package are defined in their namespace environment
				# so this allows us to easily retrieve the environment of a function (e.g. "base" for the mean() function))
				env_addresses_namespaces = envnames:::get_namespace_addresses()
			} else {
				# Set the addresses of packages to an array of 0 length (because this array is used below)
				env_addresses_packages = c()
				env_addresses_namespaces = c()
			}
	
			#-------------------------- 2. Recursive search of environments ---------------------------
			# Extend the list of environments found in step 1 by recursively searching for environments within each
			# of those environments.
			# Otherwise we recursively search just for the environments inside the given 'envir' environment. 
		  if (length(env_user_names) == 0) {
				# No user-defined environments were found in step (1a) 
		    # => Initialize env_full_names and env_addresses to an empty array, which will be updated below with
		    # the respective info from system and package environments
		    env_fullnames_user = character(0)
		    env_addresses = character(0)
		  } else {
				# Search for environments within each user-defined environment found in step (1a)
		    env_fullnames_user = envnames:::unlist_with_names( tapply(env_user_names,
		                                                          INDEX=names(env_user_names),  # This is the BY group of the analysis
		                                                          FUN=envnames:::crawl_envs, c(), c(), envir) )

		    # Get the environment objects referenced by the env_full_names names
		    # NOTE that we use eval() instead of get() (in which case we would have used envs = lapply(env_full_names, get, envir=envir))
		    # because eval() can evaluate environments given as e.g. env_of_envs$env11
		    # but get() cannot... get() does NOT accept the '$' sign, but accepts only calls of the form
		    # get("env11", envir=env_of_envs) and creating these statements from the env_full_names content is more difficult.
		    # Note also that there should not be any problem with the evaluation of env_full_names to environments because
		    # these env_full_names are obtained from actually searching for environments through the crawl_envs() function above.
		    envs = lapply(env_fullnames_user, function(x) eval(parse(text=x), envir=envir))
	
  			# Get the memory addresses of the environments just retrieved
  			env_addresses = eval( unlist( lapply(envs, get_obj_address, envir) ), envir=envir)
		  }

			#--------------------- 3. Add execution environments of functions -------------------------
			# Check if get_env_names() was called from a function
			# Get all the function calls in the calling chain
			# NOTES:
			# - we CANNOT call get_fun_calling_chain() (which does precisely this job) because that function
			# calls get_env_names() and we would enter a recursive loop!
			# - sys.calls() will always return at least one element: the call to this function get_env_names()
			all_calls = sys.calls()

			# Initialize the arrays that will possibly (if there are any functions) store the information about function environments
			env_locations_function = c()
			env_addresses_function = c()
			env_fullnames_function = c()

			if (length(all_calls) > 1) {
				# This means that get_env_names() was called from within a function
				# => crawl the calling chain and retrieve the execution environment of each of the functions in the chain
				# and add it to the environment map table.

				# Iterate on all the calls from the previous to latest to first one so that we go UP in the calling chain
				# (note that we exclude the last call because it is THIS function get_env_names(), on which we are not interested)
				for (c in seq(length(all_calls)-1, 1, -1)) {
					# Compute the number of levels up in the calling chain starting from the current function (get_env_names)
					l = length(all_calls) - c
					fun_name = as.character(all_calls[[c]])[1]
					# Update the arrays with the required information to store in the environment map table
					# IMPORTANT: Note that we use sys.frame(-l) to retrieve the execution environment of the calling function
					# and NOT parent.frame(l)... This is because parent.frames may not always include internal functions in
					# the counting of parent frames (e.g. print() if we call print(get_env_names())), as explained in the Note
					# section of the documentation for sys.parent.
					env_locations_function = c(env_locations_function, environmentName(parent.env(sys.frame(-l))))	# This is where the function whose execution environment's address is of interest resides (i.e. the enclosing environment, where the function was defined)
					env_addresses_function = c(env_addresses_function, envnames:::address(sys.frame(-l)))
					env_fullnames_function = c(env_fullnames_function, fun_name)
					#cat("level:", l, "fun:", fun_name, "memory:", envnames:::address(sys.frame(-l)), "\n")
				}
			}

		  #------------------------ 4. Put together all environments found --------------------------
		  # Put together all the info gathered so far about user-defined environments and system/package
		  # environments into the output data frame.
			# Note: the type of address: "user" or "system". This information may be of interest when dealing
			# with environments and in particular is needed by obj_find() to know how to resolve the environment
			# where an object is found from the environment name, whether using eval() for user-defined environments
			# or as.environment() for system or package environments.
			env_types = c(rep("user", length(env_fullnames_user)), rep("function", length(env_fullnames_function)), rep("system/package", length(env_addresses_packages)), rep("namespace", length(env_addresses_namespaces)))
			env_locations = c( names(env_fullnames_user), env_locations_function, rep(NA, length(env_addresses_packages)), rep(NA, length(env_addresses_namespaces)) )
			env_addresses = c(env_addresses, env_addresses_function, env_addresses_packages, env_addresses_namespaces)
			env_fullnames = c(env_fullnames_user, env_fullnames_function, names(env_addresses_packages), names(env_addresses_namespaces))
			# Separate the root and the name from env_full_names
			env_roots_and_names = sapply(env_fullnames, FUN=envnames:::extract_last_member)
			env_paths = unlist( env_roots_and_names["root",] )
			env_names = unlist( env_roots_and_names["name",] )

			# Save the information in the env_table data frame
			env_table = data.frame(	type=env_types,
															location=env_locations,
															address=env_addresses,
															pathname=env_fullnames,
															path=env_paths,
															name=env_names,
															stringsAsFactors=FALSE)
		} # if (length(env_user_names) > 0)
  } else  {
    envnames:::error_NotValidEnvironment(deparse(substitute(envir)))
  }

  return(env_table)
}
