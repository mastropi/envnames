#' Create a lookup table with address-name pairs of environments
#' 
#' Return a data frame containing the address-name pairs of system, package, namespace, user-defined,
#' and function execution environments in the whole workspace or within a given environment.
#' 
#' The table includes the empty environment as well when the address-name pairs map is constructed
#' on the whole workspace.
#' 
#' @param envir environment where environments are searched for to construct the lookup table.
#' It defaults to \code{NULL} which means that all environments in the whole workspace should be searched for
#' and all packages in the \code{search()} path should be returned including their namespace environments.
#' @param include_functions flag indicating whether to include in the returned data frame 
#' user-defined environments defined inside function execution environments.
#' 
#' @details
#' The search for environments is recursive, meaning that a search is carried out for environments defined
#' within other user-defined environments and, when \code{include_functions=TRUE} within function execution
#' environments.
#' 
#' The search within packages is always on \emph{exported objects} only.
#' 
#' If \code{envir=NULL} the lookup table includes all system, package, and namespace environments
#' in the \code{search()} path, as well as all user-defined found in \emph{any} of those environments
#' (with recursive search), and all function execution environments.
#'
#' If \code{envir} is not \code{NULL} the lookup table includes just the user-defined and
#' function execution environments found inside the given environment (with recursive search).
#' 
#' @return A data frame containing the following seven columns:
#' \itemize{
#' \item{\code{type}} type of environment ("user" for user-defined environments,
#' "function" for function execution environments, "system/package" for system or package environments,
#' "namespace" for namespace environments, and "empty" for empty environments such as emptyenv()).
#' \item{\code{location}} location of the environment, which is only non-\code{NA} for user-defined
#' and function execution environments: 
#'    \itemize{
#'    \item for a user-defined environment, the location is the system environment or package where
#'    the environment resides (note that this may be different from the parent environment if the
#'    parent environment was set during creation with the \code{parent=} option of the \code{new.env()}
#'    function or using the \code{parent.env()} function)
#'    \item for a function execution environment, the location is the function's enclosing environment, i.e. the environment
#'    where the function is defined.
#'    }
#' \item{\code{locationaddress}} the memory address of the \code{location} environment.
#' \item{\code{address}} memory address of the environment. This is the key piece of information used
#' by the package to retrieve the environment name with the \code{environment_name()} function.
#' For functions, this is the address of the function's execution environment.
#' \item{\code{pathname}} path to the environment and its name. This is the combination of columns
#' \code{path} and \code{name} whose values are put together separated by \code{$}.
#' \item{\code{path}} path to the environment (i.e. all environments that need to be traversed in order
#' to reach the environment).
#' \item{\code{name}} name of the environment.
#' }
#' The \code{type} column is used to distinguish between user-defined environments, function execution
#' environments, package or system environments, namespace environments, and empty environments.
#' 
#' The data frame is empty if no environments are found in the given \code{envir} environment.
#' 
#' \code{NULL} is returned when an error occurs.
#' 
#' @examples
#' # Create example of chained environments
#' env1 <- new.env()
#' with(env1, env11 <- new.env())
#' with(env1$env11, envx <- new.env())
#' 
#' # Address-name pairs of all environments defined in the workspace,
#' # including environments in the search path
#' get_env_names()  # returns a data frame with at least the following user environments:
#'                  # "env1", "env1$env11", "env1$env11$envx"  
#' 
#' # Address-name pairs of the environments defined in a given user-defined environment
#' get_env_names(env1)  # returns a data frame with the following user environments:
#'                      # "env11", "env11$envx"
#' 
#' # Address-name pairs of the environments defined in a given package
#' get_env_names(as.environment("package:stats")) # should return an empty data frame
#'                                                # (since the stats package does not
#'                                                # have any environments defined)
get_env_names = function(envir=NULL, include_functions=FALSE) {

  #-------------------------------- Auxiliary functions ---------------------------------------
  get_informative_environment_name = function(envir) {
    # The informative environment name is one that gives information to the user.
    # Therefore, we need to distinguish whether get_env_names() was called from
    # the outside world or from another function in the envnames package such as
    # environment_name().
    # In fact:
    # - when calling get_env_names() from the outside world, we simply deparse
    # substitute parameter 'envir' in the calling environment => we use parent.frame(n=1)
    # - when calling get_env_names() from another function in the envnames package
    # (such as environment_name()), we need to deparse substitute parameter 'envir'
    # in the environment calling e.g. environment_name() => we use parent.frame(n=2)
    # (if we used parent.frame(n=1) as above the name of 'envir' resolves to "envir"
    # and this is uninformative to the user)
    # IMPORTANT: using parent.frame(n=2) in the second case ASSUMES that there is only
    # one function in package envnames calling get_env_names(), as opposed to calling
    # get_env_names() through a chain such as, obj_find() calls environment_name()
    # which in turn calls get_env_names()...
    fun_calling = get_fun_calling(n=2)
    fun_calling_location_and_function = extract_root_and_last_member(fun_calling)
    if (!is.null(fun_calling_location_and_function) &&
        fun_calling_location_and_function$root == "envnames" &&
        fun_calling_location_and_function$name != get_fun_name(n=1)) { # get_fun_name(n=1) should refer to get_env_names()
      envir_name = deparse(substitute(envir, parent.frame(n=2)))
    } else {
      envir_name = deparse(substitute(envir, parent.frame(n=1)))
    }    
  }
  #-------------------------------- Auxiliary functions ---------------------------------------

  # Parse input parameters
  if (!is.null(envir) && !is.environment(envir)) {
    error_NotValidEnvironment(deparse(substitute(envir)))
    return(NULL)
  }

  # Initialize the output table to a data frame
  env_table = data.frame(	type=character(0),
                          location=character(0),
                          locationaddress=character(0),
                          address=character(0),
                          pathname=character(0),
                          path=character(0),
                          name=character(0),
                          stringsAsFactors=FALSE)

	#-------------------------- 1. First level search of environments ---------------------------
	### 1a) Look for user-defined environments either within the given 'envir' environment or
	### within the whole workspace (i.e. within all system and package environments reachable via the search() path)
	if (is.null(envir)) {
		# Look for user-defined environments in the WHOLE workspace (i.e. in the system/packages in the search() path)
		env_user_names = get_user_environment_names_in_search_path()
	} else {
	  env_user_names = get_user_environment_names_in_env(envir)

	  # Assign a names attribute because this is used by the process that looks for environments inside 'envir' below
	  # in order to define the LOCATION of the environments found in that search and its address, which will then
	  # fill the 'location' and 'locationaddress' columns of the output envmap data frame.
    envir_name = get_informative_environment_name(envir)
	  names(env_user_names) = rep(envir_name, length(env_user_names))
	}

	# Continue processing if either:
	# - at least one environment was found or
	# - the whole workspace is searched for in which case we should include all system, package, and
	# namespace environments in the lookup table. Note that this is the case when the user passed envir=NULL.
	if (length(env_user_names) > 0 || is.null(envir)) {
		if (is.null(envir)) {
			### Search for environments should be carried out in all the workspace 
			### => The environments defined in the whole workspace must be retrieved 
			### 1b) Get the address-name pairs of all non-empty system/package/namespace environments
		  ### (e.g. .GlobalEnv, package:stats, package:base, etc.) (the empty environment
		  ### is added below to the lookup table)

			#-- System and packages
			env_addresses_system_and_packages = get_searchpath_environment_addresses()
			
			#-- Namespaces
			# Store the namespace environment of each package as well!
			# (for instance, the functions defined by each package are defined in their namespace environment
			# so this allows us to easily retrieve the environment of a function (e.g. "base" for the mean() function))
			env_addresses_namespaces = get_namespace_addresses()
		} else {
			# Set the addresses of packages to an array of 0 length (because this array is used below)
			env_addresses_system_and_packages = character(0)
			env_addresses_namespaces = character(0)
		}

	  #-------------------------- 2. Recursive search of environments ---------------------------
	  # Recursively search for user-defined environments inside user-defined environments found in step (1)
	  env_fullnames_user_recursive = character(0)
	  env_addresses_user_recursive = character(0)
	  env_locationaddresses_user_recursive = character(0)
	  if (length(env_user_names) > 0) {
	    # Search for environments within each user-defined environment found in step (1a)
	    # These evironments are ADDED to the already existing ones meaning that the output of this process
	    # includes the user-defined environments found in step (1a).
	    user_environments_recursive = get_user_environments_in_user_envs_recursively(env_user_names, envir)
	  }
	  # Create a small lookup table that contains the name of user-defined environments and their memory address
	  # so that we can retrieve the environment name in case one of the functions analyzed
	  # in the next step below is defined precisely there but such environment is not mentioned
	  # in the function call (as in e.g. env1$f), making the result of sys.calls() not contain
	  # the piece of the call 'env1', and therefore making it impossible to find the environment name
	  # (unless we store it here in this small lookup table, together with its memory address)
	  envmap_env_user = data.frame(env_name=user_environments_recursive$fullnames,
	                               env_address=user_environments_recursive$addresses,
	                               stringsAsFactors=FALSE)

		#--------------------- 3. Add execution environments of functions -------------------------
		# Check if this function get_env_names() was called from within a function
		# Get all the function calls in the calling chain
		# NOTES:
		# - we CANNOT call get_fun_calling_chain() (which does precisely this job) because that function
		# calls get_env_names() and we would enter a recursive loop!
		# - sys.calls() will always return at least one element: the call to this function get_env_names()
		all_calls = sys.calls()

		# Initialize the arrays that will possibly (if there are any functions)
		# store the information about function environments and about user-defined
		# environments defined inside those function environments.
		env_locations_function = character(0)               # Locations of function execution environments
		env_locationaddresses_function = character(0)       # Memory addresses of the location of the function execution environment (e.g. address of env1 if we talk about env1$f)
		env_addresses_function = character(0)               # Memory addresses of the function execution environments
		env_fullnames_function = character(0)               # Names of function execution environments with their full path (e.g. env1$f)
		env_execenv_fun = c()                               # Function execution environments
		env_fullnames_user_inside_function = character(0)   # Full names of user-defined environments inside function environments
		env_addresses_user_inside_function = character(0)   # Addresses of user-defined environments inside function environments
		env_locationaddresses_user_inside_function = character(0) # Addresses of the function inside which user-defined environments are found

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
				fun_exec_address = address(fun_exec_env)
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
				  # because that's where the function sits (this is e.g. the parent environment of "env_of_envs" when the path is
				  # "env_of_envs$env1$f") so that we can use the environmentName() to get the name of such location environment (location_env)
				  members = extract_root_and_last_member(fun_name)
				  path_to_function = members$root
				  envs_to_function = unlist(strsplit(path_to_function, "\\$"))
				  parent_env = parent.env(fun_exec_env)   # Start, as parent environment with the parent environment of the function's execution environment (i.e. the environment where the function is defined)
				  # TODO: (2017/09/26) Add a new column in the output lookup table containing the address of the path (e.g. of env_of_envs$env1)
				  # This would be then used by the look_for_environment() function defined in obj_find.r where we
				  # make reference to this column(!) in order to retrieve the location of an environment
				  # (e.g. the environment of a function execution environment).
				  # Currently there is no UNIT test that shows the use of this column... (still to be implemented)
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
				  # NOTE: Another way of obtaining the same thing would be to call eval(as.name(fun_name), envir=parent.env(fun_exec_env))
				  # which in fact I am using below before looking for user-defined environments inside the function execution environment 
				  # and it seems to work.
				  call_as_function = try( eval(parse(text=fun_name), envir=parent.env(fun_exec_env)), silent=TRUE ) 
				  if (!inherits(call_as_function, "try-error")) {
				    location_env = environment(call_as_function)
				  }
				}
				location_env_address = address(location_env)
				location_env_name = environmentName(location_env)
				if (location_env_name == "") {
				  # The environment where the function is defined is either:
				  # - a function execution environment
				  # - a user-defined environment
				  # => Look for the environment address to retrieve its name
				  # in the two small envmap lookup tables created above, one for the
				  # function execution environments, and one for the user-defined environments

				  # Start with the function execution environments
				  ind = which( envmap_calling_chain[, "fun_exec_address"] == location_env_address )
				  if (length(ind) > 0) {
				    # length(ind) should be exactly 1, i.e. only one function should match the memory address of location_env
				    location_env_name = envmap_calling_chain[ind, "fun_name"]
				  } else {
				    # Look for the environment in the small envmap of user-defined environments created above
				    ind = which( envmap_env_user[, "env_address"] == location_env_address )
				    if (length(ind) > 0) {
				      # length(ind) MAY be more than 1, because there could be several user-defined environment
				      # pointing to the same environment and therefore having the same memory address.
				      # We pick the first matching environment based on alphabetical order of their name.
				      # NOTE that we COULD return ALL the matching environments (e.g. in a string separated by
				      # semicolon, or by a comma), but we choose to keep it simple and allow ONLY 1 location
				      # for a given user-defined environment. I.e. maybe the user wants to use the location
				      # information for some particular task and they are expecting there to have only one name.
				      ord = order(envmap_env_user[ind, "env_name"])
				      location_env_name = envmap_env_user[ind[ ord[1] ], "env_name"]
				    }
				  }
				}

				# Update the arrays that contain different type of information about the function execution environments
				env_locations_function = c(env_locations_function, location_env_name)
				env_locationaddresses_function = c(env_locationaddresses_function, location_env_address)
				env_addresses_function = c(env_addresses_function, fun_exec_address)
				env_fullnames_function = c(env_fullnames_function, fun_name)
				env_execenv_fun = c(env_execenv_fun, fun_exec_env)
				#cat("level:", level, ", fun:", fun_name, ", memory:", fun_exec_address, ", location:", location_env_name, "\n")
				
				# Recursively look for user-defined environents defined inside the function execution environment
				# (whenever the function is NOT "environment_name", which is the function calling this function where
				# the environment passed as parameter 'env' will also match the environment whose name we want to retrieve)
				# done-2017/10/15: (2017/10/15) We should make sure that the function environmnt_name() is the function INSIDE
				# the envnames package and NOT a function defined by the user... (in which case we should process it!)
				if (include_functions && !identical( environment(eval(as.name(fun_name), envir=parent.env(fun_exec_env))), asNamespace("envnames") )) {
				  env_user_names_in_fun = crawl_envs_in_env(fun_exec_env)
				  if (length(env_user_names_in_fun) > 0) {
				    # Use the location of the function as names attribute of the array
				    # containing the user-defined environment names found inside the function
				    # execution environment, since these are used as location when constructing
				    # the envmap table below.
				    names(env_user_names_in_fun) = rep(fun_name, length(env_user_names_in_fun))
				    # Get the environment addresses
				    env_user_addresses_in_fun = get_obj_addresses_from_obj_names(env_user_names_in_fun, envir=fun_exec_env)

				    # Add the user-defined environments found to the arrays collecting their information
				    env_fullnames_user_inside_function = c(env_fullnames_user_inside_function, env_user_names_in_fun)
				    env_addresses_user_inside_function = c(env_addresses_user_inside_function, env_user_addresses_in_fun)
				    env_locationaddresses_user_inside_function = c(env_locationaddresses_user_inside_function, rep(fun_exec_address, length(env_user_names_in_fun)))
				  }
				}
			}
		}

	  #------------------------ 4. Put together all environments found --------------------------
	  # Put together all the info gathered so far about user-defined environments and system/package
	  # environments into the output data frame.
		# Note: the type of address: "user", "function", or "system/package".
		# This information may be of interest when dealing with environments and in particular is needed by obj_find()
		# to know how to resolve the environment where an object is found from the environment name,
		# whether using eval() for user-defined environments or as.environment() for system or package environments.
		
		# First put together all the user-defined environments found, namely:
		# a) the user-defined environments found inside system environments
		# b) the user-defined environments found recursively inside other user-defined environments
		# c) the user-defined environments found recursively inside function execution environments
		# (a) and (b) are already together in the same array
		# (c) is stored in a separate array
		env_fullnames_user = c(user_environments_recursive$fullnames, env_fullnames_user_inside_function)
		env_addresses_user = c(user_environments_recursive$addresses, env_addresses_user_inside_function)
		env_locationaddresses_user = c(user_environments_recursive$locationaddresses, env_locationaddresses_user_inside_function)

		# Now, if the map is constructed on the global workspace,
		# define the set of empty environments (which are not allowed to contain any objects)
		# (normally this is just the empty environment in R but just in case we make this list extendable)
		# (note that the empty environment is NOT part of the search list returned by search(), so it's not
		# considered a system or package environment)
		if (is.null(envir)) {
		  env_addresses_empty = address(emptyenv())
		  names(env_addresses_empty) = "R_EmptyEnv"
		} else {
		  env_addresses_empty = character(0)
		}

		n_user_environments = length(env_fullnames_user)
		n_functions = length(env_fullnames_function)
		n_system_and_packages = length(env_addresses_system_and_packages)
		n_namespaces = length(env_addresses_namespaces)
		n_empty = length(env_addresses_empty)
		env_types = c(rep("user", n_user_environments),
		              rep("function", n_functions),
		              rep("system/package", n_system_and_packages),
		              rep("namespace", n_namespaces),
		              rep("empty", n_empty))
		env_locations = c(names(env_fullnames_user),
		                  env_locations_function,
		                  rep(NA, n_system_and_packages),
		                  rep(NA, n_namespaces),
		                  rep(NA, n_empty))
		env_locationaddresses = c(env_locationaddresses_user,
		                          env_locationaddresses_function,
		                          rep(NA, n_system_and_packages),
		                          rep(NA, n_namespaces),
		                          rep(NA, n_empty))
		env_addresses = c(env_addresses_user,
		                  env_addresses_function,
		                  env_addresses_system_and_packages,
		                  env_addresses_namespaces,
		                  env_addresses_empty)
		env_fullnames = c(env_fullnames_user,
		                  env_fullnames_function,
		                  names(env_addresses_system_and_packages),
		                  names(env_addresses_namespaces),
		                  names(env_addresses_empty))
		# Separate the root and the name from env_full_names
		env_roots_and_names = sapply(env_fullnames, FUN=extract_root_and_last_member)
		env_paths = unlist( env_roots_and_names["root",] )
		env_names = unlist( env_roots_and_names["name",] )

		# Save the information in the env_table data frame
		# NOTE: It's important to define row.names= if we don't want
		# this to fail when there is only one row with the error message
		# "row names contain missing values"!
		env_table = data.frame(	row.names=1:length(env_types),
		                        type=env_types,
														location=env_locations,
														locationaddress=env_locationaddresses,
														address=env_addresses,
														pathname=env_fullnames,
														path=env_paths,
														name=env_names,
														stringsAsFactors=FALSE)
	} # if (length(env_user_names) > 0)

  return(env_table)
}
