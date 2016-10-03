#' Retrieve the name of an environment
#' 
#' Retrieve the name of an environment as the \code{\link{environmentName}} function of the base package does,
#' but extending its functionality to retrieving the names of user-defined environments and function
#' environments as well.
#' 
#' @param env environment whose name is requested. It can be given as an object of class environment or as
#' a string with the memory address of the environment. The latter is useful to find out if a particular
#' memory address is the reference of an environment object.
#' Note that the variable passed may or may \emph{not} exist in the calling environment, as the purpose is
#' to search for it! --and return its name in case it is an environment.
#' @param envir environment where \code{env} should be searched for. If NULL, \code{env} is searched in
#' the whole workspace.
#' @param envmap data frame containing a lookup table with name-address pairs of environment names and
#' addresses to be used when searching for environment \code{env}. Defaults to NULL which means that the
#' lookup table is constructed on the fly with the environments defined in the \code{envir} environment.
#' See the details section for more information on its structure.
#' @param byaddress flag indicating whether the names of \emph{all} the environments sharing the same memory
#' address as that of \code{env} should be returned. Defaults to \code{TRUE}.
#' @param ignore one or more environment names to ignore if found during the search. These environments
#' are removed from the output. See the details section.
#' 
#' @details
#' If \code{env} is an environment it is searched in the \code{envir} environment using its memory address.
#' If \code{env} is a memory address, the memory address itself is searched among the defined environments 
#' in the \code{envir} environment.
#' In both cases, if \code{envir=NULL} the search is carried out in the whole workspace. 
#'  
#' It may happen that there exist more than one environment with the same memory address (for instance
#' if an environment is a copy of another environment). In such case, if \code{byaddress=TRUE},
#' the names of ALL the environments matching \code{env}'s memory address are returned.
#' 
#' If \code{envmap} is passed it should be a data frame providing an address-name pair lookup for environments
#' with at least the following columns:
#' \itemize{
#' \item{\code{pathname}} the environment name with the full path to it separated by \code{$}
#' (e.g. env1$env)
#' \item{\code{address}} the memory address of the environment
#' }
#' This is useful for speedup purposes, in case several calls to this function will be done
#' under the same environment space.
#' Such \code{envmap} data frame can be created by calling \link{get_env_names}.
#' 
#' The \code{ignore} parameter is useful for example when iterating on a set of environments
#' and the loop variable itself is not of interest, therefore we may want to remove it from the output, as in:
#' \code{for (e in c(globalenv(), baseenv())) { print(environment_name(e, ignore="e")) }}
#' 
#' @return
#' If \code{byaddress=TRUE} (the default) an array containing the names of all the environments
#' (defined in the \code{envir} environment if \code{envir} is not NULL) having the same memory address
#' as the \code{env} environment.
#' 
#' If \code{byaddress=FALSE} the environment name given in \code{env} is used as well to check the matched
#' environments (potentially many if they have the same memory address) so that only the environments having
#' the same name as the \code{env} environment are returned. Note that several environments may be found
#' if environments with the same name are defined in different environments.
#' WARNING: in this case, the name is matched exactly as the expression given in \code{env}. So for instance,
#' if \code{env=globalenv()$env1} the name \code{"globalenv()$env1"} is checked and this will not return any
#' environments since no environment can be called like that. For such scenario use \code{env=env1} instead or
#' optionally \code{env=env1} and \code{envir=globalenv()} if the \code{env1} environment in the global environment
#' is wished.  
#' 
#' NULL is returned if \code{env} is not found or if it is not an environment (in which case an appropriate
#' message is shown).
#' 
#' @aliases get_env_name
environment_name <- function(env, envir=NULL, envmap=NULL, byaddress=TRUE, ignore=NULL) {
# todo:
# 1) [DONE-2016/08/13] (2016/03/30) Add the functionality of receiving a memory address in the env parameter and retrieving
#    the environment name associated to the address (of course if the associated variable exists and is
#    an environment!). This would be useful because some functions in R return the memory address of
#    the environment (for instance when retrieving the environment where a function is defined, whenever
#    the function is defined within a user-defined environment (as in with(env1, f <- function(x) { })))
# 	 or when running e.g. options() we get a list of defined functions with the address of 
# 	 the environment where they are defined at the end of the function definition and we may want to know
#		 in which environment (name) the function is defined.
#    UPDATE: (2016/03/30) note that the returned value of e.g. environment(env1$f) in the example just given
#		 correctly returns the environment 'env1' (i.e. environment_name(environment(env1$f)) returns "env1")  

  # Output variable
  env_full_names = NULL

	# Flavor of the environment functions... (for reference)
  #env_current = environment()
  #env_parent = parent.env(env_current)
  #env_calling = parent.frame()

  # Setup the address-name pairs of the environments defined in envir if the envmap variable is not passed
	if (is.null(envmap)) {
  	envmap = get_env_names(envir=envir)
	}

  if (!is.null(envmap)) { # This means that parameter 'envir' is a valid environment
		# Variable used to store the indices of the envmap lookup table where the environment is found
		indfound = numeric(0)

		# Look for 'env' as an potential environment object in the address-name lookup table
		# only when it is NOT given as a memory address
		# (i.e. a string as in "<0x00000000119dba68>")
		# If it's given as a memory address search directly for that memory address in the lookup table
		# This special case is needed because get_obj_address() will return the memory address of variable
		# 'env' and this is *not* what we want when 'env' is directly a memory address; we just want to
		# search for the given memory address in the envmap lookup table... so no need to call get_obj_address()
		# in that case.
		if (!envnames:::is_memory_address(env)) {
			env_addresses = get_obj_address(env, envir=envir, n=1)

			# Now look for the addresses returned in the envmap lookup table created above
			# But first check if the match should be done by ADDRESS or instead by the NAME of the environment
			# (the latter means that only the environments matching both the memory address AND the name
			# of the 'env' environment are returned)
			# This is important because several environments may exist that point to the same memory address
			# (e.g. by doing e = globalenv(), e will have the same memory address as the global environment)
			if (byaddress) {
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
			# Note that in this case we don't care about the value of parameter 'byaddress' because
			# there is no name associated to 'env' so the match will for sure be by address only!!
			indfound = which(envmap[,"address"] == env)
		}

		# If any environments are found construct the output variable
    if (length(indfound) > 0) {
      # Location (used when envir=NULL) and full name of the matching environments
      # (as.character() is used to remove any factor attribute)
      env_locations = as.character(envmap[indfound,"location"])
      env_full_names = as.character(envmap[indfound,"pathname"])
			# Standardize the environment names (in case the global environment or the base environment are present)
			env_full_names = sapply(env_full_names, FUN=envnames:::standardize_env_name, USE.NAMES=FALSE)

      # Check if the user asked to ignore some of the environments found and if so remove them from the output
			# This is useful when we are calling environment_name() from e.g. a loop on a set of environments
			# and we want to ignore from the list the loop variable which simply references an environmenet, as in:
			# for (e in c(globalenv(), baseenv())) { print(environment_name(e, ignore="e")) }
			if (!is.null(ignore)) {
				# Standardize ignore in case it's the global environment or the base environment
				ignore = sapply(ignore, FUN=envnames:::standardize_env_name)
				indkeep = which( !(env_full_names %in% ignore) )
				env_full_names = env_full_names[indkeep]
			}

			# Store the location of the environments found as the names attribute of array env_name when:
			# - envir=NULL and
			# - it's not the case that there is only one environment found and the environment is a system
			# or package environment. 
      # In fact, envir=NULL means that the user doesn't know where the environment is located
      # (i.e. in which package) and may want to know where it is.
			# In addition, there may be several environments matching 'env' if different variables point to the
			# the same environment (that's why we also add the names when length(env_full_names) > 1).
			# Finally, when the environment whose name was requested is one of the system or package environments
			# and that is the only environment found, then simply show its name.
      # This is the same logic implemented in function get_obj_address() when envir=NULL (although implemented
			# differently there because of the different information available in that case)
      if (is.null(envir) &&
					!(length(env_full_names) == 1 && sapply(env_full_names, envnames:::destandardize_env_name) %in% search())) {
        names(env_full_names) = env_locations
      }
		}
  }

  return(env_full_names)
}

get_env_name <- environment_name