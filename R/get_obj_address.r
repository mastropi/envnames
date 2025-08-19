#' Return the memory address of an object
#'
#' Return the memory address of an object after recursively searching for the object in all the environments defined
#' in a specified environment or in all the environments defined in the whole workspace.
#'
#' @param obj object whose memory address is requested. It can be given as a variable name or an expression.
#' Strings representing object names are not interpreted and return \code{NULL}.
#' @param envir environment where the object should be searched for. All parent environments of
#' \code{envir} are searched as well. It defaults to \code{NULL} which means that it should be searched in the
#' whole workspace (including packages, namespaces, and user-defined environments).
#' @param envmap data frame containing a lookup table with name-address pairs of environment names and
#' addresses to be used when searching for environment \code{env}. It defaults to \code{NULL} which means that the
#' lookup table is constructed on the fly with the environments defined in the \code{envir} environment
#' --if not \code{NULL}--, or in the whole workspace if \code{envir=sNULL}.
#' See the details section for more information on its structure.
#' @param n number of levels to go up from the calling function environment to resolve the name
#' of \code{obj}. It defaults to 0 which implies the calling environment.
#' @param include_functions whether to include funtion execution environments as environments where the object
#' is searched for. Set this flag to \code{TRUE} with caution because there may be several functions where the
#' same object is defined, for instance functions that are called as part of the object searching process!
#'
#' @details
#' The object is first searched recursively in all environments defined in the specified environment (if any),
#' by calling \code{obj_find}.
#' If no environment is specified, the object is searched recursively in the whole workspace.
#'
#' The memory address is then retrieved for every object found in those environments having the same name
#' as the given object \code{obj}.
#'
#' Strings return \code{NULL} but strings can be the result of an expression passed as argument to this function.
#' In that case, the string is interpreted as an object and its memory address is returned as long as
#' the object exists.
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
#' @return
#' The 8-digit (32-bit architectures) thru 16-digit (64-bit architectures) memory address of
#' the input object given as a string enclosed in <>  (e.g. \code{"<0000000005E90988>"})
#' (note that Ubuntu Debian may use 12-digit memory addresses),
#' or \code{NULL} under any of the following situations:
#' \itemize{
#' \item the object is \code{NULL}, \code{NA}, or a string, or any other object whose memory address changes every
#' time the object is referred to (for instance for \code{alist[1]} --as opposed to \code{alist[[1]]}--
#' where \code{alist} is a list.
#' \item the object is a constant (e.g. \code{TRUE}, \code{3}, etc.)
#' \item the object does not exist in the given environment.
#' \item the object is an expression that cannot be evaluated in the given environment.
#' }
#'
#' Note that for the last case, although constants have a memory address, this address is meaningless as
#' it changes with every invocation of the function. For instance, running
#' \code{address(3)} several times will show a different memory address each time, and that is why
#' \code{get_obj_address} returns \code{NULL} in those cases.
#'
#' When \code{envir=NULL} (the default) or when an object exists in several environments,
#' the memory address is returned for all of the environments where the object is found. In that case, the addresses are
#' stored in an array whose names attribute shows the environments where the object is found.
#'
#' @examples
#' env1 = new.env()
#' env1$x = 3                       # x defined in environment 'env1'
#' x = 4                            # x defined in the Global Environment
#' get_obj_address(env1$x)          # returns the memory address of the object 'x'
#'                                  # defined in the 'env1' environment
#' get_obj_address(x, envir=env1)   # same as above
#' get_obj_address(x)               # Searches for object 'x' everywhere in the workspace and
#'                                  # returns a named array with the memory address of all its
#'                                  # occurrences, where the names are the names of the
#'                                  # environments where x was found.
#'
#' # Memory addresses of objects whose names are stored in an array and retrieved using sapply()
#' env1$y <- 2;
#' objects <- c("x", "y")
#' sapply(objects, FUN=get_obj_address, envir=env1)	# Note that the address of object "x"
#'                                                  # is the same as the one returned above
#'                                                  # by get_obj_address(x, envir=env1)
#'
#' # Memory address of elements of a list
#' alist <- list("x")
#' get_obj_address(alist[[1]])      # memory address of object 'x'
#' get_obj_address(alist[1])        # NULL because alist[1] has a memory address
#'                                  # that changes every time alist[1] is referred to.
get_obj_address = function(obj, envir=NULL, envmap=NULL, n=0, include_functions=FALSE) {
# NOTE: (2016/07/26) This function is needed to avoid an error when requesting the address() of an object that
# does not exist. This function returns NULL in such case instead of an error message!

  # Function that returns the object address in a GIVEN environment
	get_obj_address0 = function(obj, envir, n=0) {
			# First check if envir is a valid environment
			if(!is.environment(envir)) {
				error_NotValidEnvironment(envir)
				return(NULL)
			}
			#------------------ 1. Try to retrieve the object address using 'obj' as is -----------------
			# Get the name of the object in the calling function (i.e. the object stored in 'obj'. That is if obj=x, this returns "x")
			obj_name = get_obj_name(obj, n=n+1, silent=TRUE)
			if (!is.null(obj_name) && obj_name != "" && exists(obj_name, envir=envir, inherits=FALSE)) {
				## NOTE: The conditions !is.null() and != "" are in place because these are not valid arguments for the
				## exists() function. All the other names including invalid names such as "<a" are valid arguments for exists().
				obj_address = address(eval(as.name(obj_name), envir=envir))  # This eval() does NOT need to be enclosed in try() because the object exists() in 'envir'
				## NOTE: we are using eval(as.name(obj_name)) instead of eval(obj).
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
			    # Note that we set the warn option to "no warning" in order to
			    # avoid the warning message "restarting interrupted promise evaluation"
			    # when the obj object does not exist. This happens when the program already
			    # tried to evaluate the object unsuccessfully.
			    # See for more info: http://stackoverflow.com/questions/20596902/r-avoiding-restarting-interrupted-promise-evaluation-warning
			    set_option_warn_to_nowarning()
			    obj_eval <- try(eval(obj, envir=envir), silent=TRUE)
			    reset_option_warn()
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
			        obj_address = address(obj_eval)
			      } else {
			        #----------- 2b.- Do the same as step 1 but now for the evaluated object --------------
			        # This case is for instance when obj = alist$z whose value obj_eval = "x"
			        # Since "x" may be the name of an object in the given environment, we search for "x" now.
			        # Do the same we tried in step 1 but now for the evaluated object
			        obj_name = get_obj_name(obj_eval, n=n+1, silent=TRUE)
			        if (!is.null(obj_name) && obj_name != "" && exists(obj_name, envir=envir, inherits=FALSE)) {
			          obj_address = address(eval(as.name(obj_name), envir=envir))  # This eval() does NOT need to be enclosed in try() because the object exists() in 'envir'
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
			          obj_address = try( eval( parse(text=paste("address(", deparse(obj), ")")), envir=envir ), silent=TRUE )
			        }
			      }
			    }
			  }
			}

			return(obj_address)
	}

	# Initialize the output variable
	obj_addresses = NULL

	# First check whether the object is NULL, NA or a string, in which case we return NULL
	# In fact, we don't want to retrieve the address of a string, because the memory address changes for every call to the function!
	# (note: although this worked correctly for get_obj_address("x"), it failed for get_obj_address("env1$x") (it gave an incorrect
	# memory address) and fixing it to make it work properly was too complicated and not really necessary)
	# Note that we need to set the warning option to "no warning" to avoid the warning about "promised evaluation"
	# (not sure when it happens)
	set_option_warn_to_nowarning()
	is_obj_null_na_string = try( is_null_or_na(obj) || is_string(obj), silent=TRUE )
	reset_option_warn()
	# is_obj_null_na_string can evaluate to NA in some cases, NA is treated as false here.
	if (!inherits(is_obj_null_na_string, "try-error") && isTRUE(is_obj_null_na_string)) {
	  return(NULL)
	}

	# Look for the object inside the environment specified in envir or in the whole workspace when envir=NULL
	if (is.null(envir)) {
		# Look for the object in the whole workspace (globalsearch=TRUE) and
	  # get the name of ALL the environments where the object is found
	  objects_found = obj_find(obj, envmap=envmap, globalsearch=TRUE, n=n+1, return_address=TRUE, include_functions=include_functions)
		  ## NOTE that:
		  ## - n+1 means that 'obj' should be resolved 1 level up from the n levels passed to get_obj_address(),
		  ## since we are now going into one level deeper (by calling obj_find())
		  ## - we need the address of the environments found in order to correctly treat function execution environments,
		  ## i.e. treat them as execution environments and NOT objects of class functions (see below the section we deal
		  ## with user-defined or execution environments)

		if (!is.null(objects_found)) {
			# Iterate on the environments found
			obj_addresses = NULL
			envir_names_final = objects_found$env_full_names   # This variable is used to store the final list of environments found by obj_find() above
			                                                   # because some may not exist if the envmap table is inconsistent with the current context

			# First check if objects_found$env_full_names is not NULL
			# (if this is the case length(objects_found$env_full_names) is 0 and the FOR loop still iterates!!)
			if (!is.null(objects_found$env_full_names)) {
      	for (i in 1:length(objects_found$env_full_names)) {
  			  envir_name = objects_found$env_full_names[i]
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
  				# c) check if the environment is a named environment, or a user-defined or function environment
  				#
  				if (is.na(envir_name)) {
  					# This means that 'obj' is a system or package environment
  					# Note that:
  				  # - we don't simply call address(obj) because obj may be given as an expression (e.g. globalenv())
  					# - system and package environments are accessible through the search() path and that's why we use
  					# envir=.GlobalEnv in this case.
  				  # - setting envir=.GlobalEnv is not a problem of overriding the 'envir' parameter because
  				  # we don't even get to this point when the envir parameter is set to a particular environment
  				  # since these statements are part of the block 'if (is.null(envir))'.
  					obj_addresses = c(obj_addresses, get_obj_address0(obj, envir=.GlobalEnv, n=n+1))
  				} else {
  					# Check whether the environment whose name is 'envir_name' is a "user-defined environment or a function
  		      # execution environment" OR a named environment (system or package)
  				  # First try to see if we can resolve it as a system or package environment with as.environment()...
  				  e = try( as.environment(destandardize_env_name(envir_name)), silent=TRUE )
  					if (inherits(e, "try-error")) {
  						# It's a user-defined or function environment
  					  # => Get the environment object associated to 'envir_name' by evaluating envir_name as an expression
  					  # (i.e. using parse() to convert the string in 'envir_name' to an expression)

  					  # Notes:
  					  # - we evaluate the expression in envir=parent.frame(). This is important to avoid name collision
  					  # when envir_name happens to be "e" (i.e. the user has defined an environment with name "e"), whose name
  					  # is the same as the local variable 'e' we have just created above!
  					  # If we do not use envir=parent.frame(), the LOCAL 'e' variable is evaluated, and since 'e' is of class
  					  # try-error, we will not store what we want to store in 'e' (i.e. the environment associated to envir_name)
  					  # - we use eval() and not get() because the environment is given with its full path (e.g. env1$env)
  					  # and get() does not accept the '$' notation.
  					  # - we still need to try() the eval() because we don't know if the environment exists...
  					  # In fact, it may well be the case that the envmap map passed to this function is an INCORRECT name-address
  					  # pairs map! (the user could pass anything... even this happened in Sep-2017 when testing the package
  					  # where a completely controlled environment generated an inconsistent envmap table --recall this case with
  					  # the "object 'doTryCatch' not found" error triggered from test-get_obj_value.r)

  					  # First get the environment as if it were an user-defined environment (as opposed to a function execution environment)
  					  # Note that we DESTANDARDIZE the environment name 'envir_name' in case the user-defined environment
  					  # is given as in e.g. "R_GlobalEnv$env1", in which case the eval-parse-ing of it will not find env1
  					  # but the eval-parse-ing of ".GlobalEnv$env1", i.e. of the result of the destandardization, will))
  					  e = try( eval(parse(text=destandardize_env_name(envir_name)), envir=parent.frame()), silent=TRUE )
  					  # Check if the environment is actually a function execution environment
  					  if (class(e) == "function" && include_functions) {
  					    # Get the function's execution environment using its name and address
  					    envir_address = objects_found$env_addresses[i]
  					    e = get_fun_env(envir_address)
  					  }
  					}
  				  if (inherits(e, "try-error") || is.null(e)) {
  				    # The environment actually does NOT exist (this happens when the envmap table is inconsistent with the current context)
  				    # => remove the envir_name from the envir_names array so that there is no error when assigning envir_names
  				    # as names to the obj_addresses array below
  				    envir_names_final = envir_names_final[envir_names_final!=envir_name]
  				  }
  				  else {
  				    # Get the address of the object in the environment where it was found (e)
  				    obj_addresses = c(obj_addresses, get_obj_address0(obj, envir=e, n=n+1))	# n+1 means that we want to resolve 'obj' 1 level up from the n levels passed to get_obj_address(), since we are now going into one level deeper (by calling get_obj_address0())
  				  }
  				}
  			}
  			# Add the environment names where the object is found to the names attribute of the array,
  			# UNLESS no address was returned for the object (because the object has a memory address that changes
  			# every time it's invoked (this is checked by obj_addresses != NULL and it happens for instance if
  			# the object is 'alist[1]' --as opposed to e.g. alist[[1]] (with two brackets), because alist[1] doesn't
  			# have a fixed memory address)
  			# or if "the object was found in only one environment and this environment is a system or package environment"
  			# (the latter condition of system or package environment is identified by the fact that envir_names_final == NA).
  			if ( !(is.null(obj_addresses) ||
  			       length(envir_names_final) == 1 && is.na(envir_names_final)) ) {
  			  names(obj_addresses) = envir_names_final
  			}
		  }
		} # if (!is.null(objects_found))
	} else {
	  # 'envir' is not NULL
	  # => get the object address by simply calling get_obj_address0() which retrieves the address of an object given its environment
		obj_addresses = get_obj_address0(obj, envir=envir, n=n+1)	# n+1 means that we want to resolve 'obj' 1 level up from the n levels passed to get_obj_address(), since we are now going into one level deeper (by calling get_obj_address0())
	}

	return(obj_addresses)
}
