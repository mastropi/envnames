#' Find an object in the workspace including user-defined environments
#' 
#' Look for an object in the whole workspace including all environments defined within it
#' (possibly recursively) and return ALL the environment(s) where the object is found.
#' User-defined environments are also searched.
#' Note that both the "recursive search" and the "user-defined environments search" makes this function
#' quite different from functions \link{find} and \link{exists} of the base package.
#' 
#' Optionally, the search can be limited to a specified environment, as opposed to carrying it out in the whole workspace.
#' Still, all user-defined environments defined inside the specified environment are searched.
#' 
#' @param obj object to be searched given as the object itself or as a character string. If given as an object,
#' expressions are accepted (see details on how expressions are considered).
#' @param envir environment where the search for \code{obj} should be carried out.
#' Defaults to \code{NULL} which means \code{obj} is searched in the calling environment (i.e. in the environment
#' calling this function), unless \code{globalsearch=TRUE} in which case it is searched in the whole workspace.
#' @param envmap data frame containing a lookup table with name-address pairs of environment names and
#' addresses to be used when searching for object \code{obj}. Defaults to NULL which means that the
#' lookup table is constructed on the fly with the environments defined in the \code{envir} environment
#' --if not NULL--, or in the whole workspace if \code{envir=NULL}.
#' See the details section for more information on its structure.
#' @param globalsearch when \code{envir=NULL} it specifies whether the search for \code{obj} should be done
#' globally, i.e. in the whole workspace, or just within the calling environment.
#' @param n non-negative integer indicating the number of levels to go up from the calling function environment
#' to evaluate \code{obj}. It defaults to 0 which implies that \code{obj} is evaluated in the environment
#' of the calling function (i.e. the function that calls \code{obj_find()}).
#' @param return_address whether to return the address of the environments where the object is found in addition
#' to their names.
#' @param include_functions whether to include funtion execution environments as environments where the object
#' is searched for. Set this flag to \code{TRUE} with caution because there may be several functions where the
#' same object is defined, for instance functions that are called as part of the object searching process!
#' @param silent run in silent mode? If not, the search history is shown,
#' listing all the environments that are searched for object \code{obj}. Defaults to TRUE.
#' 
#' @details
#' An object is found in an environment if it is reachable from within that environment. An object is considered
#' reachable in an environment if either one of the following occurs:
#' \itemize{
#' \item it exists in the given environment
#' \item it exists in a user-defined environment defined inside the given environment or in any environment
#' recursively defined inside them
#' }
#' 
#' Note that \code{obj_find} differs from base functions \code{find} and \code{exists} in that \code{obj_find}
#' searches for the object inside user-defined environments within any given environment in a recursive way.
#' 
#' In particular, compared to:
#' \itemize{
#' \item{\code{find}:} \code{obj_find} searches for objects inside user-defined environments while \code{find} is not
#' able to do so (see examples).
#' \item{\code{exists}:} \code{obj_find} \emph{never} searches for objects in the parent environment of \code{envir}
#' when \code{envir} is not \code{NULL}, as does \code{exists} when its \code{inherits} parameter is set to \code{TRUE},
#' the default. If it is wished to search for objects in parent environments, simply set \code{envir} to \code{NULL}
#' and \code{globalsearch} to \code{TRUE}, in which case the object will be searched in the whole workspace
#' and the environments where it is found will be returned.
#' }
#' 
#' When the object is found, an array containing the names of all the environments where the object is found is
#' returned. These names may include the names of user-defined environments.
#' 
#' When \code{envir} is not \code{NULL} attached packages are not included in the search for \code{obj},
#' unless of course \code{envir} is itself a package environment.
#' 
#' When given as an object, \code{obj} can be an expression in which case the following rules apply:
#' \itemize{
#' \item if the expression defines the full path to the object (as in \code{env1$env2$x} where \code{env1} and
#' \code{env2} are existing environments, object \code{x} is found if it is reachable from the calling environment
#' and all its parent environments.
#' \item if the expression is an attribute of a list, data frame, or array, the object contained in this
#' attribute is searched for. Ex: if \code{alist$var = "x"} then object \code{x} is searched. 
#' }  
#' 
#' If \code{envmap} is passed it should be a data frame providing an address-name pair lookup table
#' of environments and should contain at least the following columns:
#' \itemize{
#' \item{\code{location}} for user-defined environments, the name of the environment where the environment
#' is located; otherwise \code{NA}.
#' \item{\code{pathname}} the full \emph{environment path} to reach the environment separated by \code{$}
#' (e.g. \code{"env1$env$envx"})
#' \item{\code{address}} the 16-digit memory address of the environment given in \code{pathname} enclosed
#' in < > (e.g. \code{"<0000000007DCFB38>"})
#' }
#' This is useful for speedup purposes, in case several calls to this function will be done
#' under the same environment space.
#' Such \code{envmap} data frame can be created by calling \link{get_env_names}.
#' 
#' @return The return value depends on the value of parameter \code{return_address}: when \code{FALSE}
#' (the default) it returns an array containing the names of the environments where the object \code{obj}
#' is found; when \code{TRUE} it returns a list with two attributes: \code{"env_full_names"} and
#' \code{"env_addresses"} with respectively the environment names and addresses where the object is found.
#' 
#' @examples 
#' # Define a variable in the global environment
#' x = 4
#' # Create new environments, some nested
#' env1 = new.env()
#' with(env1, envx <- new.env())
#' env1$x = 3
#' env1$envx$x = 2
#' env1$y = 5
#' 
#' # Look for objects (crawling environments recursively)
#' obj_find(x)                  # "env1" "env1$envx" "R_GlobalEnv"
#' obj_find("x")                # "env1" "env1$envx" "R_GlobalEnv"
#' obj_find("x", envir=env1)    # "env1" "envx" (as the search is limited to the env1 environment)
#' obj_find("y")                # "env1"
#' obj_find(nonexistent)        # NULL (note that NO error is raised even if the object does not exist)
obj_find = function(obj, envir=NULL, envmap=NULL, globalsearch=TRUE, n=0, return_address=FALSE, include_functions=FALSE, silent=TRUE) {

	# Function that searches for an object among the environments included in the 'envmap' lookup table.
  # The name of the object to search for is computed by calling get_obj_name() of 'obj' and requesting
  # to evaluate 'obj' n levels up from the calling environment.
  #
  # The 'envir' variable defines an optional environment where the environments listed in the 'envmap'
  # lookup table should be evaluated in order to get to the actual environment variable (e.g. in order to
  # get from the string "env1$env2$env" to the actual environment variable referenced by this string
  # using eval(parse(text=env_full_name), envir=envir)). When this is given, the envmap lookup table
  # is assumed to have been created in the 'envir' environment!
  #
  # On the contrary, if 'envir' is NULL, the environment is evaluated using eval() with its default behaviour,
  # i.e. the environment name is first evaluated in the parent environment (parent frame) and then searched
  # for in all parent environments until it is found. When envir=NULL it is assumed that the 'envmap' lookup
  # table was created on the whole workspace!
  #
  # IMPORTANT: Whatever the 'envir' value, every environment in the 'envmap' lookup table is expected to be
  # found when running eval()!! It's either a user-defined environment or a named environment (system, package,
  # namespace).
  #
	# Parameters env_full_names and env_addresses are both input and output parameters that are updated with the
  # respective information about the new environments found with this search.
	look_for = function(obj, envmap, env_full_names, env_addresses, n, envir=NULL, include_functions=FALSE, silent=TRUE) {
	  # Get the name of obj in the calling environment of the function calling this function + n levels up
	  # (where n is the parameter passed to obj_find())
	  # (this is the same as running get_obj_name(obj, n=n+1) in the calling function environment, i.e.
	  # in the main body of obj_find(), where in fact we have already run that, but here we are constructing
	  # obj_name again as a *local* variable --and this is crucial because there are two places in the main
	  # body of obj_find() where this function can be called: in one place it's called with 'obj' as parameter
	  # and in the other place it's called with 'obj_eval' as parameter => obj_name needs to be computed again!)
	  obj_name = get_obj_name(obj, n=n+2, silent=TRUE)

	  # Look for this object in the environments defined in the 'envir' environment
		i = 0
		nenvs = nrow(envmap)
		for (address in envmap[,"address"]) {
			i = i + 1
			env_type = as.character(envmap[i,"type"])
			env_full_name = as.character(envmap[i,"pathname"])

			# ONLY check if the object exists in the env_full_name environment if the environment
			# has not already been checked (repetition happens e.g. for package and namespace environments
			# which have the same name (e.g. both the base package and its namespace environment
			# are called "package:base")
			if (!env_full_name %in% env_full_names) {
			  if (!silent)
			    cat(i, "of", nenvs, ": Inspecting environment", env_full_name, "...\n")
			  
			  # Get the environment from the currently analyzed envmap entry
			  # Need to check if the current entry corresponds to an unnamed environment (user-defined) or
			  # to a named environment (system, package, namespace)
			  if (env_type == "user") {
			    # Case for unnamed environments (e.g. those created with new.env())
			    if (is.null(envir)) {
			      env = try( eval(parse(text=env_full_name)), silent=TRUE )
			    } else {
			      env = try( eval(parse(text=env_full_name), envir=envir), silent=TRUE )
			    }
			  } else if (env_type == "function") {
          if (include_functions) {
  			    # Set the environment to search for 'obj_name' as the function's execution environment
	  		    # (i.e. this allows to search objects defined in fuction execution environments!)
		  	    env = get_fun_env(env_full_name, address)
			    } else {
			      env = NULL
			    }
				} else {
			    # Case for named environments (mostly packages) (e.g. .GlobalEnv, package:stats, etc.)
			    env = as.environment(env_full_name)
			  }
			  
			  # Check whether the object exists in the currently analyzed environment
			  # and if so add the analyzed environment to the list of environments where the object is found
			  if (!inherits(env, "try-error") && !is.null(env) &&		## env could be NULL if env_type = "function" and the function's execution environment could not be retrieved
			      !is.null(obj_name) && obj_name != "" &&        	## This is checked to avoid an error in exists() which does not accept a NULL or empty argument
			      exists(obj_name, envir=env, inherits=FALSE)) { 	## inherits=FALSE avoids searching on the enclosing (i.e. parent) environments
			    env_full_names = c(env_full_names, env_full_name)
			    env_addresses = c(env_addresses, address)
			  }
			}
		}

		return(list(env_full_names=env_full_names, env_addresses=env_addresses))
	} # look_for()

	# Store the original environment passed in 'envir' in case we need to call obj_find() recursively
	# (in order to search for the *evaluated* 'obj' (obj_eval) as done at the very end of this process),
	# and set envir to the parent environment when it is NULL so that searches for the object take place
	# in the calling environment (e.g. if we do 'with(env1, obj_find(x))') or if we call obj_find()
	# from within a function.
	if (is.null(envir)) {
	  envir_orig = NULL
	  if (globalsearch) {
	    envir = .GlobalEnv
	  } else {
	    envir = parent.frame()
	  }
	} else {
	  envir_orig = envir
	}

	# Get the name of the envir environment to be used in messages
	# Note that this returns "<environment>" when envir is a system environment (e.g. .GlobalEnv, etc.)
	# => 'envir_name' is only meaningful when envir is a user-defined environment in which case it contains
	# the name of the environment (e.g. "env1")
	envir_name = deparse(substitute(envir))

	# Initialize the output variable containing the list of environments
	# (fully specified, i.e. with their paths as well, as in env1$env)
  env_full_names = NULL
  env_addresses = NULL
  found = FALSE

	# Check that the value of parameter 'envir' is an environment
  error = FALSE
  tryCatch(
    if (class(envir) != "environment") {
			envnames:::error_NotValidEnvironment(envir_name)
      error = TRUE
    },
    error=function(e) {
						envnames:::error_NotValidEnvironment(envir_name); assign("error", TRUE, inherits=TRUE)
            ## Note the use of the inherits=TRUE parameter which means: search for the variable to be assigned in parent environments and assign the value to the first one found.
          },
		silent=TRUE
  )
  if (error) return(invisible(NULL))

  # Extract the name of the object in the calling function
	# (i.e. the string of the object passed in obj when obj is NOT a string --e.g. when obj = x => obj_name = "x")
	obj_name = get_obj_name(obj, n=n+1, silent=TRUE)
	# Check if obj_name is not NULL and it's not an empty string, o.w. the exists() function below gives an error
	# (the gsub() function removes blanks in the value of obj_name so that if the user passes "   ",
	# nchar() still returns 0, meaning that the name of the object is an empty string)
	if (!is.null(obj_name) && nchar(gsub(" ", "", obj_name)) > 0) {

		### 1.- First check if the object exists in the root of the 'envir' environment
		# In fact the object may exist in the given environment, without need to further search in
		# environments defined within that environment. Of course we also search those below.
	  # NOTE that we only check this if 'envir' was not originally NULL (envir_orig != NULL)
	  # or, despite envir_orig being NULL, the search is supposed to be done locally (globalsearch = FALSE)
	  # --i.e. in the parent environment, which is the value of 'envir' when globalsearch=FALSE and envir_orig=NULL-- 
	  # If neither of this is the case (i.e. if the search is to be done globally) a get_env_names() call
	  # will be done below that will trigger the process of searching for the object in ALL environments
	  # defined in the workspace (including packages).
	  # But more importantly, this check should NOT be done when envir was originally NULL
	  # because the name of the 'envir' environment here is obtained by calling deparse(substitute(envir))
	  # but if the value of envir was originally NULL this will return "<environment>", which doesn't make sense.
	  # Although we could get the correct name of the environment by calling environment_name() I don't want
	  # to call this function here because this implies running a get_env_names(), which is already going to be
	  # run below.
	  # On the contrary, even if envir was originally NULL, we should still search for the object in the
	  # parent environment when globalsearch=FALSE because this will NOT be done below. In this case, we also
	  # have the problem just mentioned about the output of deparse(substitute()) and in this case the problem
	  # will in fact be solved by calling environment_name(), but this is ok, because as I just said
	  # no call to get_env_names() will be done below when globalsearch=FALSE!
	  #
		# IMPORTANT: here we should use 'envir' and NOT a variable that is set to be equal to 'envir'
	  # (as my former envir_actual = envir case) because under those circumstances the result of
	  # deparse(substitute(envir_actual)) will give "<environment>" and not the name
	  # of the environment stored in 'envir' (which is what we really want). This happens because
	  # envir_actual stores a local variable ('envir') containing the environment, while 'envir'
	  # stores the actual environment passed by the user! So, the "substitution" of 'envir_actual'
	  # with deparse(substitute()) contains the value of 'envir_actual' as text, i.e. the value of
	  # 'envir' as text, and this results in "<environment>". Instead when the substitution is applied
	  # to 'envir' it returns e.g. "env1", i.e. the name of the object passed as parameter.
		if ((!is.null(envir_orig) || globalsearch == FALSE) &&
	      exists(obj_name, envir=envir, inherits=FALSE)) { # inherits=FALSE avoids searching on the enclosing (i.e. parent) environments
		  # Check whether we are in a recursive call of obj_find(). This is important because that defines
		  # in which environment is 'envir' evaluated, either in the current execution environment
		  # (when there is no recursion involved) or in the parent environment (when recursion is involved)
		  # We check if we are on a recursive call by checking the calling function name.
		  # We then compute the number of levels to go up to evaluate 'envir' by comparing the value of n
		  # with the value of n at the calling environment (n_parent) and compute the different n - n_parent.
		  # Note that normally 'envir' needs to be evaluated in the parent environment (parent.frame(1)),
		  # and not further back, since we expect to have only one recursive call to obj_find() coming from
		  # the case where we check whether we can find the *evaluated* value of 'obj' as done below,
		  # towards the end.
		  fun_calling = get_fun_calling()
		  if (length( grep("obj_find", fun_calling) ) > 0) {
		    n_parent = eval.parent(quote(n))
  		    ## Note the need to use quote() because o.w. n is first evaluated in the current environment!
	  	    ## (the other option would be to use parse(text="n")))
	      env_full_names = deparse(substitute(envir, parent.frame(abs(n - n_parent))))
		  }
		  else {
		    env_full_names = deparse(substitute(envir))
		  }

		  # Check if the name of the environment makes sense
		  if (env_full_names == "<environment>") {
		    # This means that the 'envir' environment could not be resolved to a name because for example
		    # is equal to the calling environment (which happens when envir=NULL and globalsearch=FALSE)
		    # and the calling environment doesn't have a name, e.g. when obj_find() is called as:
		    # 	with(env1, obj_find(x))
		    # => Get the name of the environment by calling environment_name() with envir set to the parent frame
		    # of the calling environment (this was proved to work on a simulation test!)
		    env_full_names = environment_name(envir, envir=parent.env(envir))
		  }
		  # Store the address of the environment whose name is stored in env_full_names
		  # NOTE that:
		  # - only one environment is listed in env_full_names. (because env_full_names contains the name of the environment
		  # referenced by 'envir', which is only ONE)
		  # - this should be done BEFORE standardizing the environment name! (as e.g. "R_GlobalEnv" because the parse() function
		  # gives the error that no object called "R_GlobalEnv" is found)
		  env_addresses = try( eval(parse(text=env_full_names)) )
		  if (inherits(env_addresses, "try-error")) {
		    env_addresses = NULL
		  }
		  
		  # Standardize the environment names (this is important because a further search for the object
		  # may be carried out below --by calling look_for() when globalsearch=TRUE-- and the environment name should be already
		  # standardized if we don't want the global environment or the base environment to appear twice)
		  env_full_names = sapply(env_full_names, envnames:::standardize_env_name, USE.NAMES=FALSE)
		}

		### 2.- Look for the object inside any environments defined within 'envir' (recursively)
	  # When envir_orig=NULL, how this search is done depends on parameter globalsearch:
	  # - if globalsearch = TRUE  => the search is done on the whole workspace (envir=NULL)
	  # - if globalsearch = FALSE => the search is done solely on the environments found
	  # inside the 'envir' environment (which is the parent environment (a.k.a. calling environment))
		# Recall that if the original parameter envir is NULL, it has already been set to .GlobalEnv,
	  # and this allows to search for the object in the whole workspace if globalsearch=TRUE
	  # (simply by constructing the 'envmap' lookup table on the whole workspace (i.e. using envir=NULL
	  # in the call to get_env_names()).
	  # Still if globalsearch=FALSE and the original value of envir is NULL, envir could have been
	  # set to .GlobalEnv (if the calling environment is the global environment), and in this case the
	  # object is searched for only within the global enviroment (i.e. it is not searched within packages
	  # or namespaces and the environments therein).
	  # 
	  # SO HERE we go over all the environments stored in envmap and check if the object is there
	  # Note that the first parameter in the call to look_for() must be 'obj_name' and NOT 'obj'
	  # because 'obj' will return the 'value* of the variable passed NOT its *name* (which is what we need here)
	  if (is.null(envir_orig) && globalsearch) {
			if (is.null(envmap)) envmap = get_env_names(envir=NULL)
	    env_found = look_for(obj_name, envmap, env_full_names, env_addresses, n, envir=NULL, include_functions=include_functions, silent=silent)
	  } else {
	    if (is.null(envmap)) envmap = get_env_names(envir=envir)
	    env_found = look_for(obj_name, envmap, env_full_names, env_addresses, n, envir=envir, include_functions=include_functions, silent=silent)
	  }
	  env_full_names = env_found$env_full_names # This may return a SET of environments (when the object is found in several environments)
	  env_addresses = env_found$env_addresses

		if (is.null(env_full_names)) {
		  # If still the object was not found...
		  ### 3.- Check if 'obj' was given as an expression that includes the environment path to the object
		  ### as in 'env1$x' or 'globalenv()$env1$x'
		  # (Note that this step must come BEFORE checking if the object can be evaluated (step 4 below)
		  # because the expression that would pass the test we do now (e.g. env1$y) could evaluate to the name
		  # of an object (e.g. "x"), but in this case we assume that the user is not interested in finding
		  # the object given by the value of env1$y but rather in finding whether object env1$y exists)

		  # Check if the environment path is actually an environment, if not, do not look for obj_name because
		  # it wouldn't mean anything. In fact for instance...
		  # - if obj = alist$v and alist is not an environment then we wouldn't want to look for object "v"
		  # because 'v' is just part of a list, not of an environment (and perhaps an object called "v" even
		  # exists somewhere and we would be looking for it whereas that has nothing to do with the original request!)
		  # - however, if obj = alist$v and alist$v resolves to a variable name, say "x", then we would like to
		  # look for the object called "x". This is done in step 4 below.
		  obj_with_path = envnames:::check_object_with_path(obj_name, envir, checkenv=TRUE)
		  if (obj_with_path$ok && obj_with_path$env_found) {
		    # Check if the object can be resolved in the 'envir' environment (where the search for the object
		    # is being carried out) or in any parent environment.
		    # Looking for parent environments is important because the user may have called obj_find as:
		    #   with(env1, obj_find(globalenv()$env2$x))
		    # in which case the object ('globalenv()$env2$x') should be found in the global environment
		    # even if we are calling obj_find() from within the 'env1' environment...
		    # In fact, if we run with(env1, globalenv()$env2$x), we will get the value
		    # of x inside the globalenv()$env2 environment.
		    # TODO: (2016/10/09) Should this search be with inherits=FALSE when globalsearch=FALSE?
		    # (even taking into account what I just wrote about searching for 'globalenv()$env2$x')
				# In that case, inside check_object_exists(), instead of calling eval() on 'obj' we would need
				# to call exists() on the object name (i.e. obj_with_path$name) on the environment path made up of
				# 'envir' -> obj_with_path$env_name using the option inherits=FALSE. Note that this can be done
				# by calling exists() as e.g.:
				#		exists(obj_name, envir=eval(parse(text=env_name)))
				# where e.g. obj_name = "x" and env_name = "env_of_envs$env1" (note that the $ is accepted by eval()!)
		    check_obj_exist = envnames:::check_object_exists(obj, envir)
		    if (check_obj_exist$found) {
		      # Assign the environment extracted from the object name (i.e. the full environment path)
					# as the environment name to return by the function (env_full_names)
					# TODO: (2016/10/09) When globalsearch=TRUE, try to solve the issue arising when the object exists
		      # in an environment different from 'envir'
					# and still is found (because the eval() call inside check_object_exists() is performed on all
					# parent environments from 'envir') but the name stored in env_full_names in that case is the name
					# of the environment path indicated in 'obj' (e.g. "env_of_envs$env1"). This would suggest that
					# such environment exists in the 'envir' environment but this is not true in this case, as it
					# exists in ANOTHER environment, whose name we are not giving. So the solution would be to add the
					# environment where the object is found to the environment path and assign e.g. the following to
					# env_full_names: "envwhereitwasfound$env_of_envs$env1".
					# Perhaps this could be done by calling get_env_names() on the whole workspace and then calling
					# look_for('obj') on the constructed envmap lookup table and using envir=NULL. 
		      env_full_names = obj_with_path$env_name
		    }
		  }

		  if (is.null(env_full_names)) {
		    # If still the object was not found... give it a last chance!
		    ### 4.- Try to see if obj is an expression whose *evaluation* exists (see more on the next line)
		    # Note that the object is evaluated in the environment n levels up from the current environment
		    # (this is the meaning of 'n', i.e. how many levels up should 'obj' be evaluated)
		    # or in any parent environment until it is found.
		    # Note also that we set the warn option to -1 (i.e. remove warnings) in order to
		    # avoid the warning message "restarting interrupted promise evaluation"
		    # when the obj object does not exist. This happens when the program already
		    # tried to evaluate the object unsuccessfully.
		    # We re-establish the original warn value further down after different try() calls
		    # have been carried out.
		    # See more at: http://stackoverflow.com/questions/20596902/r-avoiding-restarting-interrupted-promise-evaluation-warning
		    option_warn = options("warn")$warn
		    options(warn=-1)
		    obj_eval <- try(eval(obj, envir=parent.frame(n+1)), silent=TRUE)

		    #--- Check first if obj_eval yields an error or resolves to a value but obj is still a valid object to search for 
		    # This check is two-fold:
		    # - on one side we check whether the evaluated object obj_eval and the original object obj are different
		    # - on the other side we check whether the try( eval() ) call above yielded an error.
		    # If any of these is the case, obj_eval is replaced with obj, because obj may contain the object
		    # we are looking for.
		    # This happens for instance when:
		    # - obj = as.name(y) where y = "x", i.e. a string we are treating as a symbol (because of as.name())
		    # - obj = v[[1]] where v is a list of symbols, as in v = c(quote(x), quote(y)) (note that is a 
		    # list in this case NOT an array! --R behaviour, presumably because each element of the "array" is
		    # storing a complicated object)
		    # In these examples, x is the object we are looking for and two situations may happen,
		    # which derive respectively into the two checks mentioned above:
		    # - either x exist in the calling environment n levels up (therefore obj_eval contains
		    # the evaluation of x --e.g. 3)
		    #   --> in this case obj_eval != obj so we should assign obj to obj_eval so that we can
		    #       look for object x (not for the value 3 as an object!)
		    # - or x does not exist in the calling environment n levels up 
		    #   --> in this case the try(eval()) yields an error so we should assign obj to obj_eval
		    #       so that we can look for object x.
		    # Note that this does not happen if obj = v[1] in the above example (i.e. only one pair of brackets,
		    # not two as in [[1]]), because in that case v[1] is a list and the above try(eval()) does not give an error.
		    is_obj_eval_different_from_obj <- try(obj_eval != obj, silent=TRUE)
		    if (envnames:::is_logical(is_obj_eval_different_from_obj) &&
  		        ## Need to first check if the result of the above comparison is a valid logical value
  		        ## (e.g. has length > 0 or is not NA) because if obj_eval or obj are NULL or NA
  		        ## the comparison will yield logical(0) or NA and an error will be raised.
		        ( inherits(obj_eval, "try-error") ||
		          inherits(is_obj_eval_different_from_obj, "try-error") ||  # If there was an error in 'obj_eval != obj' then it means that the result of the comparison is TRUE
		          !inherits(is_obj_eval_different_from_obj, "try-error") && is_obj_eval_different_from_obj) ) {
		      # => Assign the searched object to obj_eval
		      # This is the case when e.g. obj = v[[1]], i.e. an element of a list
		      # containing a name (e.g. 'x') that does not exist in the calling environment 
		      # n levels up (meaning that the try(eval(obj)) above returns an error)
		      # But if this is the case, we still want to search for symbol 'x'.
		      obj_eval <- try(obj, silent=TRUE)
		    }
		    options(warn=option_warn)

		    #--- Now process obj_eval
		    if (!inherits(obj_eval, "try-error")) {
		      if (is.list(obj_eval) && length(obj_eval) == 1) {
		        # Extract the single element of the list as obj_eval (this is the case for example when
		        # obj = v[1] and v is a vector that contains e.g. environments or functions... in that case
		        # the evaluation of v[1] returns a list with a single element whose value is the environment
		        # or function contained in v[1])
		        obj_eval = obj_eval[[1]]
		      }
		      #--------------- 4a. Check if the evaluated object is an environment ------------------
		      if (is.environment(obj_eval)) {
		        # When the evaluated object is an evironment or function, first get the address and then
		        # look for this address in the environment map table created abov
		        # (this is the only way we can get to the name of an environment!!
		        # --i.e. through the address-name lookup table)
		        # (in fact running get_obj_name() on an environment object returns "<environment>"
		        # which is not useful at all!
		        obj_address = envnames:::address(obj_eval)

		        # Look for this address in the envmap table created above and
		        # return either the "path" or the "location" value (the latter is returned if "path" is empty)
		        # In fact, we should not return the "pathname" as we do for regular (non-environment) objects,
		        # since for environment objects the "pathname" will coincide with the environment name!
		        # and what we need to know is the **environment where the object is found**... 
		        ind = which(envmap[,"address"] == obj_address)

						# Clean up the matched environments: in the case both "function" and "proper" environments matched, keep just the "proper" environments
						ind = envnames:::clean_up_matching_environments(envmap, ind)

		        if (length(ind) > 0) {
		          # Construct the environment where the object (also an environment) is found by using either
							# the value of 'path' or of 'location': we use simply 'path' when 'path' is not empty, o.w.
							# we use 'location', but it's important to note that we do NOT use location$path when path
							# is not empty, because we don't want to show to the user for instance "R_GlobalEnv$env_of_envs"
							# because by showing simply "env_of_envs" we are implying that env_of_envs is located in the
							# global environment (o.w. the location would be indicated before it as e.g. envx$env_of_envs
							# should env_of_envs be defined inside environment envx)
							env_full_names = ifelse(envmap[ind,"path"] != "",
																			as.character(envmap[ind,"path"]),
																			as.character(envmap[ind,"location"]))
		          ## NOTE: as.character() removes any factor levels that may be present in the envmap columns
		          ## I define the envmap data frame with the stringsAsFactors=FALSE option but the user may provide
		          ## an envmap data frame that was not created with this option... 
		        }
		      } else if (is.function(obj_eval)) {
		        # The environment of functions is retrieved by the environment() function, which is the environment
		        # where the function is defined.
		        env = environment(obj_eval)
		        env_full_names = environment_name(env, envir=envir, envmap=envmap, matchname=FALSE)
          }

		      if (is.null(env_full_names)) {
		        #----- 4b. Recurse and check if we can find the evaluated object in 'envir_orig' ------
		        # Compare the object evaluation with the object name in order to avoid entering an infinite loop
		        # (i.e. here we rule out the case when e.g. obj_name = "3" and obj_eval = 3 in which case we should
		        # stop looking for the object '3'!)
		        # Note that we enclose the comparison of obj_eval with obj_name in a try block because obj_eval
		        # can be any kind of object and that object may not accept a comparison with a string (for instance
		        # if the obj_eval is a function like e.g. 'mean')
		        is_obj_eval_different_from_obj_name = try( obj_eval != obj_name, silent=TRUE )
		        if (envnames:::is_logical(is_obj_eval_different_from_obj_name) &&
  		            ## Need to first check if the result of the above comparison is a valid logical value
		              ## (e.g. has length > 0 or is not NA) because if obj_eval or obj are NULL or NA
  		            ## the comparison will yield logical(0) or NA and an error will be raised.
		            ( !is.null(obj_eval) &&
		              (inherits(is_obj_eval_different_from_obj_name, "try-error") || # If there was an error in 'obj_eval != obj_name' then it means that the result of the comparison is TRUE => we can recurse to look for obj_eval)
		               !inherits(is_obj_eval_different_from_obj_name, "try-error") && is_obj_eval_different_from_obj_name)) ) {
		          # Repeat the obj_find() process on the EVALUATED object
		          # This is important when e.g. alist$v = "env1$x" and object 'x' exists in environment "env1"
		          # Note that we use envir=envir_orig, in order to repeat exactly the original call to obj_find()
		          # where any envir=NULL value has not been replaced with envir=.GlobalEnv.
		          obj_found = obj_find(obj_eval, envir=envir_orig, globalsearch=globalsearch, n=n+1, return_address=return_address, include_functions=include_functions, silent=silent)
		          if (return_address) {
		            env_full_names = obj_found$env_full_names
		            env_addresses = obj_found$env_addresses
		          } else {
		            env_full_names = obj_found
		          }
		        }
		      } else {
		        # Assign the addresses to each environment in env_full_names
		        # NOTE that:
		        # - at this stage there may be several environments stored in env_full_names and therefore we need to call the sapply() function
		        # - we need to call the destandardize() function because environments stored as e.g. R_GlobalEnv should be converted to their
		        # "original" names, i.e. the names that are understood by R (e.g. .GlobalEnv)), o.w. the parse() function gives the error that
		        # object e.g. "R_GlobalEnv" is not found.
		        # - the eval(parse()) expression used below simply requests the address of the environment whose name is the one given in text=.
		        # Note that we DO NOT need to enclose this evaluation in a try() block because the environment names stored in
		        # env_full_names correspond to valid environments as the object was found there!
		        env_addresses = sapply(env_full_names, function(x) { envnames:::address(eval(parse(text=envnames:::destandardize_env_name(x)))) })
		      }
		    }
		  }
		}

		if (!is.null(env_full_names)) {
			# Standardize the names of the environment so that the global and the base environments are always shown
			# the same way, regardless of how the 'envir' parameter is passed.
			env_full_names = sapply(env_full_names, FUN=envnames:::standardize_env_name, USE.NAMES=FALSE)
			found = TRUE
		}
	}
		
  if (!silent) {
    if (found) {
      cat("Object", obj_name, "found in the following environments:\n")
      print(env_full_names)
    } else {
      cat("The object was not found in any environment\n");
    }
    if (return_address) {
      return(invisible(list(env_full_names=env_full_names, env_addresses=env_addresses)))  # Return invisible() because we already printed the environments where the object was found
    } else {
      return(invisible(env_full_names))  # Return invisible() because we already printed the environments where the object was found
    }
  } else {
    if (return_address) {
      return(list(env_full_names=env_full_names, env_addresses=env_addresses))             # Return non-invisible, because we want to show the environments where the object was found
    } else {
      return(env_full_names)             # Return non-invisible, because we want to show the environments where the object was found
    }
  }
}
