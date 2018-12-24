#' Retrieve the name of an environment
#' 
#' Retrieve the name of an environment as the \code{\link{environmentName}} function of the base package does,
#' but extending its functionality to retrieving the names of user-defined environments and function
#' execution environments.
#' 
#' @param env environment whose name is of interest.
#' It can be given as an object of class environment, as a string with the name of the environment,
#' or as a string with the memory address of the environment.
#' The latter is useful to find out if a given memory address is the reference of an environment object.
#' Note that the variable passed may or may \emph{not} exist in the calling environment, as the purpose
#' of this function is also to search for it (and return its name if it is an environment).
#' It defaults to parent.frame(), meaning that the name of the environment that calls this function is retrieved.
#' @param envir environment where \code{env} should be searched for. When \code{NULL}, \code{env} is searched in
#' the whole workspace, including packages and user-defined environments, recursively.
#' @param envmap data frame containing a lookup table with name-address pairs of environment names and
#' addresses to be used when searching for environment \code{env}. It defaults to \code{NULL} which means that the
#' lookup table is constructed on the fly with the environments defined in the \code{envir} environment
#' --if not \code{NULL}--, or in the whole workspace if \code{envir=NULL}.
#' See the details section for more information on its structure.
#' @param matchname flag indicating whether the match for \code{env} is based on its name or on its
#' memory address. In the latter case all environments sharing the same memory address of the given
#' environment are returned. Such scenario happens when, for instance, different
#' environment objects have been defined equal to another environment (as in \code{env1 <- env}).
#' It defaults to \code{FALSE}.
#' @param ignore one or more environment names to ignore if found during the search. These environments
#' are removed from the output. It should be given as a character array if more than one environments
#' should be ignored. See the details section for more information.
#' @param include_functions flag indicating whether to look for user-defined environments inside function
#' execution environments. This should be used with care because in a complicated function chain, some function
#' execution environments may contain environments that point to other environments (e.g. the 'envclos' environment
#' in the \code{eval()} function when running tests using the \code{test_that} package).
#' 
#' @details
#' If \code{env} is an environment it is searched for in the \code{envir} environment using its memory address.
#' If \code{env} is a string containing a valid 16-digit memory address (enclosed in < >), the memory address
#' itself is searched for among the defined environments in the \code{envir} environment.
#' In both cases, if \code{envir=NULL} the search is carried out in the whole workspace. 
#'  
#' It may happen that more than one environment exist with the same memory address (for instance
#' if an environment is a copy of another environment). In such case, if \code{matchname=FALSE},
#' the names of ALL the environments matching \code{env}'s memory address are returned. Otherwise,
#' only the environments matching the given name are returned.
#' 
#' If \code{envmap} is passed it should be a data frame providing an address-name pair lookup table
#' of environments and should contain at least the following columns:
#' \itemize{
#' \item{\code{location}} for user-defined environments, the name of the environment where the environment
#' is located; otherwise \code{NA}.
#' \item{\code{pathname}} the full \emph{environment path} to reach the environment separated by \code{$}
#' (e.g. \code{"env1$env$envx"})
#' \item{\code{address}} an 8-digit (32-bit architectures) thru 16-digit (64-bit architectures) memory address
#' of the environment given in \code{pathname} enclosed in < > (e.g. \code{"<0000000007DCFB38>"}
#' (64-bit architectures))
#' Be ware that Linux Debian distributions may have a 12-digit memory address representation.
#' So the best way to know is to check a memory address by calling e.g. `address("x")`.
#' }
#' Passing an \code{envmap} lookup table is useful for speedup purposes, in case several calls to this
#' function will be performed in the context of an unchanged set of defined environments.
#' Such \code{envmap} data frame can be created by calling \link{get_env_names}.
#' Use this parameter with care, as the matrix passed may not correspond to the actual mapping of existing
#' environments to their addresses and in that case results may be different from those expected.
#' 
#' The following example illustrates the use of the \code{ignore} parameter:
#' 
#' \code{for (e in c(globalenv(), baseenv())) { print(environment_name(e, ignore="e")) }}
#' 
#' That is, we iterate on a set of environments and we don't want the loop variable (an environment itself)
#' to show up as part of the output generated by the call to \code{environment_name()}.
#' 
#' @return
#' If \code{matchname=FALSE} (the default), an array containing the names of all the environments
#' (defined in the \code{envir} environment if \code{envir} is not \code{NULL}) having the same memory address
#' as the \code{env} environment.
#' 
#' If \code{matchname=TRUE}, the environment name contained in \code{env} is used in addition to the memory
#' address to check the matched environments (potentially many if they have the same memory address)
#' so that only the environments having the same name and address as the \code{env} environment are returned.
#' Note that several environments may be found if environments with the same name are defined in
#' different environments.
#' WARNING: in this case, the name is matched exactly as the expression given in \code{env}. So for instance,
#' if \code{env=globalenv()$env1} the name \code{"globalenv()$env1"} is checked and this will not return any
#' environments since no environment can be called like that. For such scenario call the function with
#' parameter \code{env=env1} instead, or optionally with \code{env=env1} and \code{envir=globalenv()}
#' if the \code{env1} environment should be searched for just in the global environment.
#' 
#' If \code{env} is not found or it is not an environment, \code{NULL} is returned.
#' 
#' @aliases get_env_name
#' 
#' @examples
#' # Retrieve name of a user-defined environment
#' env1 <- new.env()
#' environment_name(env1)                   		# "env1"
#' 
#' # Retrieve the name of an environment given as a memory address
#' env1_address = get_obj_address(env1)
#' environment_name(env1_address)           		# "env1"
#' 
#' # Create a copy of the above environment
#' env1_copy <- env1
#' environment_name(env1)                   		# "env1" "env1_copy"
#' # Retrieve just the env1 environment name
#' environment_name(env1, matchname=TRUE)   		# "env1"
#' 
#' # Retrieve the name of an environment defined within another environment
#' with(env1, envx <- new.env())
#' environment_name(env1$envx)              		# "env1$envx" "env1_copy$envx"
#' environment_name(env1$envx, matchname=TRUE)
#'   ## NULL, because the environment name is "envx", NOT "env1$envx"
#' 
#' # Get a function's execution environment name
#' with(env1, f <- function() { cat("We are inside function", environment_name()) })  
#'     ## "We are inside function env1$f"
environment_name <- function(env=parent.frame(), envir=NULL, envmap=NULL, matchname=FALSE, ignore=NULL, include_functions=FALSE) {
  # Output variable
  env_names = NULL

  # Flavor of the environment functions... (for reference)
  #env_current = environment()
  #env_parent = parent.env(env_current)   # this is the enclosure of the current execution environment which may be different from parent.frame()! In fact, in general parent.env() of the execution environment is the global environment...
  #env_calling = parent.frame()

  # Setup the address-name pairs of the environments defined in envir if the envmap variable is not passed
	if (is.null(envmap)) {
  	envmap = get_env_names(envir=envir, include_functions=include_functions)
	}

  if (!is.null(envmap)) { # This means that parameter 'envir' is a valid environment
		# Variable used to store the indices of the envmap lookup table where the environment is found
		indfound = numeric(0)

		# Look for 'env' as a potential environment object in the address-name lookup table
		# only when it is NOT given as a memory address (i.e. a string as in "<0x00000000119dba68>")
		# If it's given as a memory address search directly for that memory address in the lookup table
		# This special case is needed because get_obj_address() will return the memory address of variable
		# 'env' and this is *not* what we want when 'env' is directly a memory address; we just want to
		# search for the given memory address in the envmap lookup table... so no need to call get_obj_address()
		# in that case.
		if (!is_memory_address(env)) {
			# We now call get_obj_address() to first look for 'env' in the given 'envir' environment --if not NULL--
			# or in the whole workspace when envir=NULL, and then retrieve its memory address (if 'env' is found).
			# NOTE that we don't simply call address() to get the memory address of 'env' because:
			# - 'env' can be given as a string (i.e. a string containing the environment name)
			# - even if 'env' is given as an environment object, it could exist in different environments
			# and we would like to retrieve ALL of them. This is also the reason why we are calling the variable
			# where the returned value is stored as a plural name ("addresses" instead of "address").
			env_addresses = get_obj_address(env, envir=envir, envmap=envmap, n=1, include_functions=TRUE)

			# Now look for the addresses returned in the envmap lookup table created above
			# But first check if the match should be done by ADDRESS or instead by the NAME of the environment
			# (the latter means that only the environments matching both the memory address AND the name
			# of the 'env' environment are returned)
			# This is important because several environments may exist that point to the same memory address
			# (e.g. by doing e = globalenv(), e will have the same memory address as the global environment)
			if (!matchname) {
				# Look just for the address (the name of the environment doesn't matter)
				# Note that this search can return more than one match even if there is only one environment with
				# the name of the 'env' environment, and the reason is that there may be several environments
				# pointing to the same memory address, as explained above.
				indfound = which(envmap[,"address"] %in% env_addresses)
			} else {
				# Look for the address AND the name of the environment in the envmap data frame
				# Note that this search can ALSO return more than one match since an environment with the same
				# name may be present in different environments or packages.

				# Get the name of the environment given in 'env'
				env_name = get_obj_name(env, n=1, silent=TRUE)

				indfound = which(envmap[,"address"] %in% env_addresses & envmap[,"name"] == env_name)
			}
		} else {
			# 'env' is given as a memory address
			# => search for this memory address in the envmap table
			# Note that in this case we don't care about the value of parameter 'matchname' because
			# there is no name associated to 'env' so the match will for sure be by address only!!

		  # First parse the memory address string
		  env = parse_memory_address(env)
			indfound = which(toupper(envmap[,"address"]) == toupper(env))
		}

		# Clean up the matched environments: in the case both "function" and "proper" environments matched, keep just the "proper" environments
		indfound = clean_up_matching_environments(envmap, indfound)

		# If any environments are found construct the output variable
    if (length(indfound) > 0) {
      # Get the locations and names of the environments found
			env_locations = ifelse( envmap[indfound,"path"] == "",
															as.character(envmap[indfound,"location"]),
															ifelse(envmap[indfound,"location"] == "R_GlobalEnv", 
															       as.character(envmap[indfound,"path"]),
															       paste(as.character(envmap[indfound,"location"]), as.character(envmap[indfound,"path"]), sep="$")
															      )
														)
      env_names = as.character(envmap[indfound,"name"])
			# Standardize the environment names (in case the global environment or the base environment are present)
			env_names = sapply(env_names, FUN=standardize_env_name, USE.NAMES=FALSE)

      # Check if the user asked to ignore some of the environments found and if so remove them from the output
			# This is useful when we are calling environment_name() from e.g. a loop on a set of environments
			# and we want to ignore from the list the loop variable which simply references an environmenet, as in:
			# for (e in c(globalenv(), baseenv())) { print(environment_name(e, ignore="e")) }
			if (!is.null(ignore)) {
				# Standardize ignore in case it's the global environment or the base environment
				ignore = sapply(ignore, FUN=standardize_env_name)
				indkeep = which( !(env_names %in% ignore) )
				env_names = env_names[indkeep]
			}

			# Store the location of the environments found as the names attribute of array env_name when:
			# - envir=NULL (i.e. the search for the environment is in the whole workspace) AND
			# - more than one environment was found (so that we use the names of the env_names object to distinguish where
			# each environment was found)
      # In fact, envir=NULL means that the user doesn't know where the environment is located
      # (i.e. in which package) and may want to know where it is.
			# In addition, there may be several environments matching 'env' if different variables point to the
			# the same environment (that's why we also add the names when length(env_names) > 1).
      # This is the same logic implemented in function get_obj_address() when envir=NULL (although implemented
			# differently there because of the different information available in that case)
			#
			# If envir=NULL and just one environment has been found, the location is prefixed to the environment name,
			# (as long as the location is NOT the global environment --which is assumed to be the default location of environments)
			# so that the user receives just a character output (i.e. NOT a named character array on which
			# Gabor Grothendieck complained)
      #
			# If envir is not NULL, the location information is not returned as part of the result because the
			# user already gave the location in the envir= parameter!
      if (is.null(envir)) {
        if (length(env_names) > 1) {
          names(env_names) = env_locations
        } else if (!is.na(env_locations) &&                         # env_locations = NA for system and package enviroments
                    env_locations != "R_GlobalEnv") {               # We choose NOT to add R_GlobalEnv to the environment name because this is the DEFAULT location of environments!
          env_names = paste(env_locations, env_names, sep="$")
        }
      }
		}
  }

	# When the env_names array has a names attribute (containing the locations of the environments found),
	# sort the array by the names attribute (so that locations appear alphabetically sorted)
	if (!is.null(names(env_names))) {
		ord = order(names(env_names))
		env_names = env_names[ord]
	}

  return(env_names)
}

get_env_name <- environment_name