#' Return the memory address of an object
#' 
#' @param obj object whose memory address is requested. It can be given as a variable name, a string with
#' a variable name, or an expression.
#' @param envir environment where the object should be searched for. All parent environments of
#' \code{envir} are searched as well.
#' @param n number of levels to go up from the calling function environment to resolve the name
#' of \code{obj}. It defaults to 0 which implies the calling environment.
#'
#' @details
#' The environment name should \emph{not} be part of the object name \code{obj}
#' (e.g. something like \code{env$x} is \emph{not} valid).
#' The call \code{get_obj_address(x, envir=env)} should be used instead.
#' 
#' If \code{obj} is already a memory address, it is returned unchanged.
#' 
#' @return The memory address of the input object given as a string enclosed in <>
#' (e.g. \code{"<0000000005E90988>"}), or NULL under any of the following situations:
#' \itemize{
#' \item the object does not exist in the given environment
#' \item the object is an expression that cannot be evaluated in the given environment
#' \item the object is a non-character constant (e.g. TRUE, 3, etc.)
#' \item the object refers to an unnamed list (e.g. '[[1]] 3', where '[[1]]' is the unnamed list)
#' \item the object is a character constant which however does not resolve to an existing variable in the
#' given environment. That is, the result of running e.g. \code{eval(as.name("aaa"))} is an error.
#' }
#' 
#' Note that for the last three cases, although the cited elements (constants, unnamed lists) have a memory
#' address, this address is meaningless as it changes with every invocation of the function. For instance, running
#' envnames:::address(3) several times will show a different memory address each time, and that is why
#' we chose to return NULL in those cases.
#' 
#' When \code{envir=NULL} (the default) or when an object exists in several environments,
#' the memory address is returned for all of them where the object is found. In that case, the addresses are
#' stored in an array whose names attribute contains the environment names where the object is found.
#' 
#' @examples
#' env1 = new.env()
#' env1$x = 3                       # x defined in environment 'env1'
#' x = 4                            # x defined in the Global Environment  
#' get_obj_address(x, envir=env1)   # Searches for object 'x' in the 'env1' environment
#' get_obj_address("x", envir=env1) # Searches for object 'x' in the 'env1' environment
#' get_obj_address(x)               # Searches for 'x' everywhere in the workspace and
#'                                  # returns an array with the memory address of all its occurrences. 
#' 
#' # Memory addresses of objects whose names are stored in an array and retrieved using sapply()
#' env1$y <- 2;
#' objects <- c("x", "y")
#' sapply(objects, FUN=get_obj_address, envir=env1)	# Note that the address of object "x" is the same as the one returned above by get_obj_address(x, envir=env1)
get_obj_address = function(obj, envir=NULL, n=0) {
# NOTE: (2016/07/26) This function is needed to avoid an error when requesting the address() of an object that
# does not exist. This function returns NULL in such case instead of an error message!

  # Function that returns the object address in a GIVEN environment
	get_obj_address0 = function(obj, envir, n=0) {
			# First check if envir is a valid environment
			if(!is.environment(envir)) {
				envnames:::error_NotValidEnvironment(envir)
				return(NULL)
			}

			#------------------ 1. Try to retrieve the object address using 'obj' as is -----------------
			# Get the name of the object stored in 'obj'. That is if obj=x, this returns "x".
			obj_name = get_obj_name(obj, n=n, silent=TRUE)
			if (!is.null(obj_name) && obj_name != "" && exists(obj_name, envir=envir, inherits=FALSE)) {
				## NOTE: The conditions !is.null() and != "" are in place because these are not valid arguments for the
				## exists() function. All the other names including invalid names such as "<a" are valid arguments for exists(). 
				obj_address = envnames:::address(eval(as.name(obj_name), envir=envir))
				## NOTE: note we are using eval(as.name(obj_name)) instead of eval(obj).
				## It's strange that the latter does not work because eval() first evaluates obj in the calling environment
				## (e.g. it would give 'env9' if obj=env9) and then evaluates the object to which 'obj' resolved to
				## in the environment specified in envir. But, despite this I get the error message that 'env9'
				## is not found when envir is not the Global Environment!
				## We can neither use evalq() because evalq() prevents 'obj' from being evaluated in the
				## current execution environment and in that case the address() function would be returning
				## the memory address of the local variable 'obj', not of the variable referenced by 'obj'!  
				
				## NOTE also that eval() presumably looks for the object recursively in all parent environments
				## if it is not found in the 'envir' environment, so in principle it does the same thing that
				## exists() does when inherits=TRUE, and we should be ok here in that eval() should not give an
				## error if exists() returned TRUE.
			} else {
				obj_address = NULL
			}
			
			if (is.null(obj_address)) {
			  #---- 2.- Check if 'obj' was given as an expression that includes the environment path -----
			  # This is the case when e.g. obj = env1$x, where env1 is an environment
			  # To check this we first check if the path to the object name is an environment and if so
			  # simply try to get the object's address directly
			  # NOTE that we need to test that the path is actually an environment because it may be another
			  # object (e.g. a list) and in that case the address of object is meaningless because it is
			  # different for each run of the get_obj_address() function! (e.g. if we call get_obj_address(alist$x))
			  obj_name_check = check_object_with_path(obj_name, checkenv=TRUE)
			  if (obj_name_check$ok && obj_name_check$env_found) {
          # Check if the object exists in the 'envir_actual' environment or any parent environment
			    check_obj_exist = check_object_exists(obj, envir)
			    if (check_obj_exist$found) {
			      # Get the address of the object found
			      obj_address = check_obj_exist$address
			    }
        }

			  if (is.null(obj_address)) {
			    # Still the object's address could not be retrieved...

			    #------------ 3. Try to retrieve the object's address after evaluating the object --------
			    # This is the case when e.g. obj is an expression as in 'objects[1]'
			    obj_eval <- try(eval(obj, envir=envir), silent=TRUE)
			    if (inherits(obj_eval, "try-error")) {
			      # Note that we do NOT show any messages when the object is not found, because the object name cannot
			      # always be obtained by deparse(substitute(obj)). In order to get the actual name we need to go back
			      # to the n-th calling function and this is a little long to do --see how I did it in get_obj_name()
			      # (using a WHILE loop)
			      #cat("Object '", deparse(substitute(obj)), "' does not exist in environment '", deparse(substitute(envir)), "'\n", sep="")
			      obj_address = NULL
			    } else {
			      #------------- 3a.- Check if the evaluated object above is an environment ---------------
			      if (is.environment(obj_eval)) {
			        # When the evaluated object is an evironment we should get the address directly
			        # (i.e. without getting the evaluated object's name first, because get_obj_name() on an environment object
			        # returns "environment" which is not useful for the environment address retrieval
			        obj_address = envnames:::address(obj_eval)
			      } else {
			        #----------- 2b.- Do the same as step 1 but now for the evaluated object --------------
			        # This case is for instance when obj = alist$z whose value obj_eval = "x"
			        # Since "x" may be the name of an object in the given environment, we search for "x" now.
			        # Do the same we tried in step 1 but now for the evaluated object
			        obj_name = get_obj_name(obj_eval, n=n, silent=TRUE)
			        if (!is.null(obj_name) && obj_name != "" && exists(obj_name, envir=envir, inherits=FALSE)) {
			          obj_address = envnames:::address(eval(as.name(obj_name), envir=envir))
			        } else {
			          obj_address = NULL
			        }
			      }
			    }
			  }
			}

			return(obj_address)
	}

	# Look for the object inside the environment specified in envir or in the whole workspace when envir=NULL
	if (is.null(envir)) {
		# Look for the object in the whole workspace (globalsearch=TRUE) and
	  # get the name of the environments where the object is found
		envir_names = obj_find(obj, globalsearch=TRUE, n=n+1)	# n+1 means that 'obj' should be resolved 1 level up from the n levels
																				                  # passed to get_obj_address(), since we are now going into one level deeper
																				                  # (by calling obj_find())

		if (is.null(envir_names)) {
			# => The object was not found anywhere
			# BUT check now if obj has already a valid and sensible memory address.
			# This is the case when e.g. obj = globalenv()$env1$x
			# which is not considered a valid object name (by get_obj_name() --which is called by obj_find() above--
		  # due to the requirements by parse()).
			# NOTE HOWEVER that if, in the above example, 'globalenv()$env1$x' exists and is a scalar and 'obj' is
			# 'globalenv()$env1$x[2]' (i.e. an out of range reference to x which yields NA) WE STILL GET A MEMORY ADDRESS
			# (the memory address allocated to NA I presume) but this memory address is meaningless, as it has nothing
			# to do with x... however, I think this is still ok, because it is the user's responsibility to pass an
			# object that makes sense...
		
			# Now try to evaluate the object... this is to test if its evaluation gives something sensible
			# before making us retrieve its memory address.
		  # --> Note that I set the warning level option "warn" to -1 so that no warning message is shown when the
		  # object is not found. Otherwise we would get the message "restarted interrupted promise evaluation"
		  # which has to do with the program having tried already to evaluate the object before (namely
		  # in get_obj_name() through the obj_find() call above) unsuccesfully.
		  # Ref: http://stackoverflow.com/questions/20596902/r-avoiding-restarting-interrupted-promise-evaluation-warning
		  option_warn = options("warn")$warn
		  options(warn=-1)
			obj_eval = try( eval(obj, envir=.GlobalEnv), silent=TRUE )
			options(warn=option_warn)
			# Now check that the evaluated result is not the same as the object itself meaning that the object
			# is not just a simple string like "3" or "3aaddd", or a memory address given as a string, etc.
			# in which case we do NOT want to retrieve the memory address because it is meaningless.
			# We would fall into this situation for example when 'obj' is v[1], i.e. an expression that gives another
			# object (in this case the content of element 1 of array v)
			# (this check is done by comparing deparse(substitute(obj)) with obj_eval whose values are different
			# --after stripping any additional quotes from deparse(substitute()) as done by the gsub() function--
			# only in non-trivial cases (where "trivial" means the cases that just mentioned above "3", "3aaddd", etc.))
			if (!inherits(obj_eval, "try-error") &&
					!is.null(obj_eval) &&
					!is.environment(obj_eval) &&
					gsub("\"", "", deparse(substitute(obj))) != obj_eval) {
				# This means there is a chance that the object is not simply a string like "x" or a number like 3, etc.
				obj_addresses = try( envnames:::address(obj), silent=TRUE )
				if (inherits(obj_addresses, "try-error")) {
					obj_addresses = NULL
				}
			} else {
				obj_addresses = NULL
			}
		} else {
			# Iterate on the environments found
			obj_addresses = character(0)
			for (envir_name in envir_names) {
				# Convert envir_name to an environment as follows:
				# a) check that envir_name is not NA (which happens when the object is a system or package environment,
				# because their path information is missing and their location is set to NA in the envmap table
				# created by get_env_names())
				# => get their address directly by calling get_obj_address0()
				# Note that in this case we could still have other elements returned in envir_names above since
				# additional environments may exist that point to that system or package environment (as in e = globalenv())
				#
				# b) de-standardize its name (e.g. "R_GlobalEnv" -> ".GlobalEnv" so that it is accepted by as.environment())
				#
				# c) check if the environment is a named environment or a user-defined environment
				# Note that we use eval() and not get() because the environment is given with its full path (e.g. env1$env)
				# and get() does not accept the '$' notation.
				# Note also that this eval() call cannot give an error because the environments listed in envir_names
				# do exist since they were returned by the call to obj_find() above.
				#
				if (is.na(envir_name)) {
					# This means that 'obj' is a system or package environment
					# Note that:
				  # - we don't simply call envnames:::address(obj) because obj may be given as an expression (e.g. globalenv())
					# - system and package environments are accessible through the search() path and that's why we use
					# envir=.GlobalEnv in this case.
				  # - setting envir=.GlobalEnv is not a problem of overriding the 'envir' parameter because
				  # we don't even get to this point when the envir parameter is set to a particular environment
				  # since these statements are part of the block 'if (is.null(envir))'. 
					obj_addresses = c(obj_addresses, get_obj_address0(obj, envir=.GlobalEnv, n=n+1))
				} else {
					# Check whether the environment is user-defined or a named environment (system or package)
				  # First try to see if we can resolve it as a system or package environment with as.environment()...
					e = try( as.environment(envnames:::destandardize_env_name(envir_name)), silent=TRUE )
					if (inherits(e, "try-error")) {
						# It's a user-defined environment
						e = eval(parse(text=envir_name))
					}
					obj_addresses = c(obj_addresses, get_obj_address0(obj, envir=e, n=n+1))	# n+1 means that we want to resolve 'obj' 1 level up from the n levels passed to get_obj_address(), since we are now going into one level deeper (by calling get_obj_address0())
				}
			}
			# Add the environment names where the object is found to the names attribute of the array,
			# UNLESS the object was found in only one environment and the object is a system or package environment
			# (which is identified by the fact that envir_names == NA).
			if ( !(length(envir_names) == 1 && is.na(envir_names)) ) {
				names(obj_addresses) = envir_names
			}
		}
	} else {
		obj_addresses = get_obj_address0(obj, envir=envir, n=n+1)	# n+1 means that we want to resolve 'obj' 1 level up from the n levels passed to get_obj_address(), since we are now going into one level deeper (by calling get_obj_address0())
	}

	return(obj_addresses)
}
