#' Create a lookup table with address-name pairs of environments
#' 
#' Return a data frame containing the address-name pairs of system, package, namespace, user-defined, and function
#' execution environments in the whole workspace or within a given environment.
#' 
#' @param envir environment where environments are searched for to construct the lookup table. Defaults to NULL
#' which means that all environments in the whole workspace should be searched for and all packages in the
#' \code{search()} path should be returned including their namespace environments.
#' 
#' @details
#' The search for environments is recursive, meaning that a search is carried out for environments defined
#' within other environments.
#' 
#' The search within packages is always on \emph{exported objects} only.
#' 
#' If \code{envir} is the global environment ,the lookup table includes all system,
#' package, and namespace environments in the \code{search()} path, as well as all user-defined and function execution
#' environments found in the global environment (with recursive search).
#' 
#' If \code{envir=NULL} the lookup table includes all system, package, and namespace environments
#' in the \code{search()} path, as well as all user-defined found in \emph{any} of those environments (with recursive search),
#' and all function execution environments.
#' 
#' @return A data frame containing the following six columns:
#' \itemize{
#' \item{\code{type}} type of environment ("user" for user-defined environments "system/package"
#' for system or package environments), and "namespace" for namespace environments.
#' \item{\code{location}} location of the environment, which is only non-\code{NA} for user-defined and function's execution
#' environments: 
#'    \itemize{
#'    \item for a user-defined environment, the location is the system environment or package where the environment resides
#' (note that this may be different from the parent environment if the parent environment was set during creation with the
#' \code{parent=} option of the \code{new.env()} function or using the \code{parent.env()} function)
#'    \item for a function's execution environment, the location is the function's enclosing environment, i.e. the environment
#'    where the function is defined.
#'    }
#' \item{\code{location}} the environment where the environment with name \code{name} and address and \code{address} is located.
#' \item{\code{locationaddress}} the address of the \code{location} environment.
#' \item{\code{address}} memory address of the environment. This is the key piece of information to get the
#' environment name with \code{environment_name()}. For functions, this is the address of the function's execution
#' environment.
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
					# Try getting the environment name of 'envir' assuming the environment name is given explicitly (as in envir=env1)
					envir_name = deparse(substitute(envir))
					if (envir_name == "envir") {
					  # This means that the environment name was NOT given explicitly (e.g. it was given as the result of a function call as in
					  # globalenv() or parent.frame() or sys.frame(sys.nframe()), etc.), since in that case deparse(substitute(envir)) resolves to "envir"
					  # => Construct the GLOBAL envmap table and retrieve the environment name from that table!
					  # Note that:
					  # - this does NOT create an infinite loop because when calling get_env_names() with envir=NULL, the function
					  # will NOT enter again this ELSE block
					  # - when calling environment_name() below with a specific envmap lookup table the get_env_names() function will NOT be called again!
					  # - the infinite loop is avoided ONLY because we FIRST create the envmap lookup table and only THEN do we call environment_name()
					  # using the envmap just constructed... if we call environment_name(evalq(envir)) WITHOUT calling get_env_names() first we would
					  # end in an infinite loop!
					  # - it is VERY important to call environment_name() on evalq(envir)) as opposed to calling it on 'envir'... i.e. we should NOT
					  # take envir as the variable 'envir' which may exist in the global environment, because in that case we would get the name
					  # of such variable, namely "envir", but may NOT be what we want (i.e. whenever 'envir' is NOT actually the environment named "envir"!)
					  # Note that this is only a problem when an environment named "envir" actually exists in the (global) workspace.
					  # - if the environment stored in 'envir' is ACTUALLY CALLED "envir", this name will be still retrieved since evalq(envir) is still 'envir'.
					  envmap_global = get_env_names(envir=NULL)
					  envir_name = environment_name(evalq(envir), envmap=envmap_global)

					  # Limit the environment name to just ONE name... it may happen that there are several environments returned
					  # when envir points to another environment.
					  # In practice, this only happens when the 'envir' environment is actually called "envir" and the environment
					  # points to another existing environment (e.g. envir was defined as envir = env11)
					  # In this situation we just keep the first environment found, and this is a convention
					  envir_name = envir_name[1]
					}
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
														locationaddress=character(0),
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
			# Check if this function get_env_names() was called from within a function
			# Get all the function calls in the calling chain
			# NOTES:
			# - we CANNOT call get_fun_calling_chain() (which does precisely this job) because that function
			# calls get_env_names() and we would enter a recursive loop!
			# - sys.calls() will always return at least one element: the call to this function get_env_names()
			all_calls = sys.calls()

			# Initialize the arrays that will possibly (if there are any functions) store the information about function environments
			env_locations_function = c()
			env_locationaddresses_function = c()
			env_addresses_function = c()
			env_fullnames_function = c()

			ncalls = length(all_calls)
			if (ncalls > 2) {
				# This means that get_env_names() was called from within a function
			  # Note that get_env_names() is part of the function calling chain, so that's why we compare > 2 and not > 1,
			  # because we are not interested in the get_env_names() function.
				# => crawl the calling chain and retrieve the execution environment of each of the functions in the chain
				# and add it to the environment map table.

				# Iterate on all the calls from the first one to the previous to latest
				# NOTES:
			  # - we exclude the last call because it is THIS function get_env_names(), on which we are not interested)
			  # - we start with the first call because we need to store the information about the execution environments in a small envmap table,
			  # so that we can retrieve the NAME of the environment should a function be defined INSIDE ANOTHER function.
			  # - IMPORTANT: it suffices to store the lookup table for the execution environments of the functions in the calling chain
			  # because it is NOT possible to call a function defined in a function that is NOT part of the calling chain, because that
			  # function will NOT be found! In addition, functions are visible if they have ALREADY been defined in the calling chain
			  # but not if they are defined in the calling chain down the line (good for us! as it makes this process easier!)
			  # So, if f1() -> g1() -> h1() where g1() is defined inside f1() and h1() is defined inside g1(), f1() and g1() are
			  # visible from h1() but h1() is NOT visible from f1() nor g1().
			  
			  # Table that will contain the name of the functions in the calling chain and the memory address of their execution environment
			  envmap_calling_chain = data.frame(matrix(nrow=ncalls-1, ncol=2))
			  names(envmap_calling_chain) = c("fun_name", "fun_exec_address")
			  for (c in seq(1, ncalls-1, 1)) {
				  level = ncalls - c
				  # Get the function name and its execution environment address
					fun_name = as.character(all_calls[[c]])[1]
					fun_exec_env = sys.frame(-level)
					fun_exec_address = envnames:::address(fun_exec_env)
					# Add the memory address to the small envmap lookup table that contains the map for the functions in the calling chain
					envmap_calling_chain[c, ] = c(fun_name, fun_exec_address)

					#-- Update the arrays with the required information to store in the environment map table
					# IMPORTANT: Note that we use sys.frame(-level) to retrieve the execution environment of the calling function
					# and NOT parent.frame(level)... This is because parent.frames may not always include internal functions in
					# the counting of parent frames (e.g. print() if we call print(get_env_names())), as explained in the Note
					# section of the documentation for sys.parent.

					# Check if fun_name contains the $ operator (e.g. env_of_envs$env1$f)
					if ( length(grep("\\$", fun_name)) > 0 ) {
					  # The function is defined in a user-defined environment (i.e. its enclosing environment is a user-defined environment)
					  # (note that sys.calls() includes these environments as part of the function name!)
					  # => Define the location environment (location_env) as the parent environment of the first environment in the path
					  # because that's where the function sits.
					  # (e.g. the parent environment of "env_of_envs" when the path is "env_of_envs$env1") so that we can apply the environmentName()
					  # function below that updates the env_locations_function array to such location environment (location_env)
					  members = envnames:::extract_last_member(fun_name)
					  path_to_function = members$root
					  envs_to_function = unlist(strsplit(path_to_function, "\\$"))
					  parent_env = parent.env(fun_exec_env)   # Start, as parent environment with the parent environment of the function's execution environment (i.e. the environment where the function is defined)
					  # TODO: (2017/09/26) Add a new column in the output lookup table containing the address of the path (e.g. of env_of_envs$env1)
					  # This would be then used by the look_for_environment() function defined in obj_find.r where we
					  # make reference to this column...! Currently there is no UNIT test that shows the use of this column... (still to be implemented)
					  #pathaddress = get_obj_address(parent_env)
					  for (i in seq_along(envs_to_function)) {
					    # Go to the parent environment of parent_env and update parent_env
					    parent_env = parent.env(parent_env)
					  }
					  # Set the location environment to the final parent_env environment obtained in the above loop
					  location_env = parent_env
					} else {
					  # The function is NOT defined in a user-defined environment
					  # => Define the location environment (location_env) as the environment where the function is defined.
					  # Note that this is NOT the parent.env() because the parent.env() is the environment from which the function was called
					  # but we are not interested in this, we are interested in the environment where the function is defined.
					  # That is we should NOT define location_env as parent.env(sys.frame(-level))

					  # First convert the function name to actually a function
					  # Note that we evaluate the expression obtained from the function name in its parent environment because
					  # there the function will exist for sure.
					  call_as_function = try( eval(parse(text=fun_name), envir=parent.env(fun_exec_env)), silent=TRUE ) 
					  if (!inherits(call_as_function, "try-error")) {
					    location_env = environment(call_as_function)
					  }
					}
					location_env_address = envnames:::address(location_env)
					location_env_name = environmentName(location_env)
					if (location_env_name == "") {
					  # The environment where the function is defined is function execution environment
					  # => Retrieve the function's name from the small envmap lookup table created in this loop
					  ind = which( envmap_calling_chain[, "fun_exec_address"] == location_env_address )
					  if (length(ind) > 0) {
					    # length(ind) should be exactly 1, i.e. only one function should match the memory address of location_env
					    location_env_name = envmap_calling_chain[ind, "fun_name"]
					  }
					}

					env_locations_function = c(env_locations_function, location_env_name)
					env_locationaddresses_function = c(env_locationaddresses_function, location_env_address)
					env_addresses_function = c(env_addresses_function, fun_exec_address)
					env_fullnames_function = c(env_fullnames_function, fun_name)
					#cat("level:", level, ", fun:", fun_name, ", memory:", fun_exec_address, ", location:", location_env_name, "\n")
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
			env_locationaddresses = c( rep(NA, length(env_fullnames_user)), env_locationaddresses_function, rep(NA, length(env_addresses_packages)), rep(NA, length(env_addresses_namespaces)) )
			env_addresses = c(env_addresses, env_addresses_function, env_addresses_packages, env_addresses_namespaces)
			env_fullnames = c(env_fullnames_user, env_fullnames_function, names(env_addresses_packages), names(env_addresses_namespaces))
			# Separate the root and the name from env_full_names
			env_roots_and_names = sapply(env_fullnames, FUN=envnames:::extract_last_member)
			env_paths = unlist( env_roots_and_names["root",] )
			env_names = unlist( env_roots_and_names["name",] )

			# Save the information in the env_table data frame
			env_table = data.frame(	type=env_types,
															location=env_locations,
															locationaddress=env_locationaddresses,
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
