#' Return the memory address of an object
#' 
#' @param obj object whose memory address is requested. It can be given as a variable name or an expression.
#' Strings representing object names are not interpreted and return NULL.
#' @param envir environment where the object should be searched for. All parent environments of
#' \code{envir} are searched as well. Defaults to \code{NULL} which means that it should be searched in the
#' whole workspace (including packages and user-defined environments).
#' @param n number of levels to go up from the calling function environment to resolve the name
#' of \code{obj}. It defaults to 0 which implies the calling environment.
#'
#' @details
#' Strings return \code{NULL} but strings can be the result of an expression passed as argument to this function.
#' In that case, the string is interpreted as an object and its memory address is returned if the object exists.
#' 
#' @return The 16-digit memory address of the input object given as a string enclosed in <>
#' (e.g. \code{"<0000000005E90988>"}), or NULL under any of the following situations:
#' \itemize{
#' \item the object is \code{NULL}, \code{NA}, or a string
#' \item the object is a constant (e.g. \code{TRUE}, \code{3}, etc.)
#' \item the object does not exist in the given environment
#' \item the object is an expression that cannot be evaluated in the given environment
#' }
#' 
#' Note that for the last case, although constants have a memory address, this address is meaningless as
#' it changes with every invocation of the function. For instance, running
#' envnames:::address(3) several times will show a different memory address each time, and that is why
#' \code{get_obj_address} returns NULL in those cases.
#' 
#' When \code{envir=NULL} (the default) or when an object exists in several environments,
#' the memory address is returned for all of the environments where the object is found. In that case, the addresses are
#' stored in an array whose names attribute shows the environments where the object is found.
#' 
#' @examples
#' env1 = new.env()
#' env1$x = 3                       # x defined in environment 'env1'
#' x = 4                            # x defined in the Global Environment
#' get_obj_address(env1$x)          # returns the memory address of the object 'x' defined in the 'env1' environment
#' get_obj_address(x, envir=env1)   # same as above
#' get_obj_address(x)               # Searches for object 'x' everywhere in the workspace and
#'                                  # returns a named array with the memory address of all its occurrences,
#'                                  # where the names are the names of the environments where x was found.
#' 
#' # Memory addresses of objects whose names are stored in an array and retrieved using sapply()
#' env1$y <- 2;
#' objects <- c("x", "y")
#' sapply(objects, FUN=get_obj_address, envir=env1)	# Note that the address of object "x" is the same as the one returned above by get_obj_address(x, envir=env1)
#' 
#' # Memory address of elements of a list
#' alist <- list("x")
#' get_obj_address(alist[[1]])      # memory address of object 'x'
#' get_obj_address(alist[1])        # same result
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
			  obj_name_check = envnames:::check_object_with_path(obj_name, checkenv=TRUE)
			  if (obj_name_check$ok && obj_name_check$env_found) {
          # Check if the object exists in the 'envir_actual' environment or any parent environment
			    check_obj_exist = envnames:::check_object_exists(obj, envir)
			    if (check_obj_exist$found) {
			      # Get the address of the object found
			      obj_address = check_obj_exist$address
			    }
       }

			 if (is.null(obj_address)) {
			    # Still the object's address could not be retrieved...
			    
			    #------------ 3. Try to retrieve the object's address after evaluating the object --------
			    # This is the case when e.g. obj is an expression as in 'objects[1]'
			    # Note that we set the warn option to -1 (i.e. remove warnings) in order to
			    # avoid the warning message "restarting interrupted promise evaluation"
			    # when the obj object does not exist. This happens when the program already
			    # tried to evaluate the object unsuccessfully.
			    # See for more info: http://stackoverflow.com/questions/20596902/r-avoiding-restarting-interrupted-promise-evaluation-warning
			    option_warn = options("warn")$warn
			    options(warn=-1)
			    obj_eval <- try(eval(obj, envir=envir), silent=TRUE)
			    options(warn=option_warn)
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
			      if (is.null(obj_address)) {
			        # Still the object's address could not be retrieved
			        # Last chance, we check whether 'obj' is a name or symbol and in that case we get its address
			        # This is the case when e.g. obj = as.name("x"), i.e. obj = x and therefore we simply need to get the address of x.
			        # Note that in this case obj_name above is "as.name(\"x\")" and that's why we were not able to retrieve
			        # the object address so far (although obj was found when computing obj_eval!)
			        if (is.symbol(obj)) {
			          obj_address = try( eval( parse(text=paste("envnames:::address(", deparse(obj), ")")), envir=envir ), silent=TRUE )
			        }
			      }
			    }
			  }
			}

			return(obj_address)
	}

	# Initialize the output variable
	obj_addresses = NULL
	
	# First check whether the object is NULL, NA o a string, in which case we return NULL
	# In fact, we don't want to retrieve the address of a string, because this changes for every call to the function!
	# (note: although this worked correctly for get_obj_address("x"), it failed for get_obj_address("env1$x") (it gave an incorrect
	# memory address) and fixing it to make it work properly was too complicated and not really necessary)
	# Note that we need to set the warning option to -1 to avoid a warning when calling is.na(obj) on an environment object...
	# Also note that before checking if obj is NA we check that it is not an environment and not a symbol because is.na()
	# gives a warning in those cases!
	# I also set the warn option to -1 because sometimes we get the warning regarding "promised evaluation".
	option_warn = options("warn")$warn
	options(warn=-1)
	is_obj_null_na_string = try( is.null(obj) || (!is.environment(obj) && !is.symbol(obj) && is.na(obj)) || envnames:::is_string(obj), silent=TRUE )
	options(warn=option_warn)
	if (!inherits(is_obj_null_na_string, "try-error") && is_obj_null_na_string) {
	  return(NULL)
	}

	# Look for the object inside the environment specified in envir or in the whole workspace when envir=NULL
	if (is.null(envir)) {
		# Look for the object in the whole workspace (globalsearch=TRUE) and
	  # get the name of the environments where the object is found
		envir_names = obj_find(obj, globalsearch=TRUE, n=n+1)	# n+1 means that 'obj' should be resolved 1 level up from the n levels
																				                  # passed to get_obj_address(), since we are now going into one level deeper
																				                  # (by calling obj_find())

		if (!is.null(envir_names)) {
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
				  e <- try( as.environment(envnames:::destandardize_env_name(envir_name)), silent=TRUE )
					if (inherits(e, "try-error")) {
						# It's a user-defined environment
					  # Note that we evaluate the expression in envir=parent.frame(). This is important to void name collision
					  # when envir_name happens to be "e" (i.e. the user has defined an environment with name "e"), whose name
					  # is the same as the local variable 'e' we have just created above!
					  # If we do not use envir=parent.frame(), the LOCAL 'e' variable is evaluated, and since 'e' is of class
					  # try-error, we will not store what we want to store in 'e' (i.e. the environment associated to envir_name)
					  e = eval(parse(text=envir_name), envir=parent.frame())
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
