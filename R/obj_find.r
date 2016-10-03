#' Find an object in a given environment
#' 
#' Check if an object is reachable from the given environment \code{envir} and return all the environments where
#' it is found.
#' 
#' @param obj object to be searched in the \code{envir} environment, given as the object itself or as a character string.
#' @param envir environment used as starting point for the search for object \code{obj}, as explained in the description.
#' It defaults to NULL which means \code{obj} is searched in the whole workspace, including attached packages. 
#' @param n non-negative integer indicating the number of levels to go up from the calling function environment
#' to evaluate \code{obj}. It defaults to 0 which implies that \code{obj} should be evaluated in the environment
#' of the calling function (i.e. the function that calls \code{obj_find}).
#' @param silent run in silent mode? Use \code{FALSE} to show the search history,
#' listing the environments that are searched for object \code{obj}. Defaults to TRUE.
#' 
#' @details
#' An object is considered reachable in an environment either because:
#' \itemize{
#' \item it exists in the given environment
#' \item it exists in an environment defined inside the given environment (the search for environments is recursive)
#' \item it is reachable via the \code{search()} path when \code{envir=NULL}
#' }
#' 
#' Note that \code{obj_find} differs from \code{exists} in that the object is \emph{not} searched in parent environments,
#' since all checks of object existence call \code{exists} using \code{inherits=FALSE}. This is so because the
#' function is aimed at reporting the names of the environment where the object is found, something that is not
#' readily given by the \code{exists} function.
#' On the contrary, \code{obj_find} looks for the object in all environments defined \bold{inside} the given environment.
#' 
#' When the object is found, a vector containing the names of all the environments where the object is found is
#' returned, including the names of user-defined environments.
#' 
#' When \code{envir} is \emph{not} \code{NULL} attached packages are not included in the search for \code{obj}.
#' 
#' @return A vector containing the names of the environments where the object \code{obj} is found.
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
#' # Look for objects
#' obj_find(x)                  # "env1" "env1$envx" ".GlobalEnv"
#' obj_find("x")                # "env1" "env1$envx" ".GlobalEnv"
#' obj_find("x", envir=env1)    # "env1" "envx" (as the search is limited to the env1 environment)
#' obj_find("y")                # "env1"
#' obj_find(nonexistentObject)  # NULL (note that NO error is raised even if the object does not exist)
obj_find = function(obj, envir=NULL, n=0, silent=TRUE) {

	# Function that searches for an object in the given enviromnent using the envmap lookup table
	# env_full_names is both an input and output variable that is updated with the new environments
	# that are found with this search.
	look_for = function(obj, envir, envmap, env_full_names, silent=TRUE) {
		i = 0
		n = nrow(envmap)
		for (address in envmap[,"address"]) {
			i = i + 1
			env_type = as.character(envmap[i,"type"])
			env_full_name = as.character(envmap[i,"pathname"])
			if (!silent)
				cat(i, "of", n, ": Inspecting environment", env_full_name, "...\n")

			# Get the environment from the currently analyzed envmap entry
			# Need to check if the current entry corresponds to an unnamed environment (user-defined) or to a named environment (packages)
			if (env_type == "user") {
				# Case for unnamed environments (e.g. those created with new.env())
				#env = get(env_name, envir=envir_actual)
				env = try( eval(parse(text=env_full_name), envir=envir) )
			} else {
				# Case for named environments (mostly packages) (e.g. .GlobalEnv, package:stats, etc.)
				env = as.environment(env_full_name)
			}

			# Check whether the object exists in the currently analyzed environment
			# and if so add the analyzed environment to the list of environments where the object is found
			if (!inherits(env, "try-error") &&
					exists(obj_name, envir=env, inherits=FALSE)) { ## inherits=FALSE avoids searching on the enclosing (i.e. parent) environments
				env_full_names = c(env_full_names, env_full_name)
			}
		}

		return(env_full_names)
	}

	# Set the actual search environment to be used in calls to exists() and eval()
	# which do not accept NULL as the envir= parameter value.
	if (is.null(envir)) {
		envir_actual = .GlobalEnv
	} else {
		envir_actual = envir
	}

	# Get the name of the envir_actual environment to be used in messages
	# Note that this returns "environment" when envir_actual is a system environment (e.g. .GlobalEnv, etc.)
	# => it has only a meaningful value when envir_actual is user-defined environment in which case it contains
	# the name of the environment (e.g. "env1")
	envir_name = deparse(substitute(envir_actual))

	# Initialize the output variable containing the list of environments
	# (fully specified, i.e. with their paths as well, as in env1$env)
  env_full_names = NULL
  found = FALSE
  
  error = FALSE
  tryCatch(
    if (class(envir_actual) != "environment") {
			envnames:::error_NotValidEnvironment(envir_name)
      error = TRUE
    },
    error=function(e) {
						envnames:::error_NotValidEnvironment(envir_name); assign("error", TRUE, inherits=TRUE)
            ## Note the use of the inherits=TRUE parameter which means: search for the variable to be assigned in parent environments and assign the value to the first one found.
          }
  )
  if (error) return(invisible(NULL))

	# Extract the name of the object
	# (i.e. the string of the object passed in obj when obj is NOT a string --e.g. when obj = x => obj_name = "x")
	obj_name = get_obj_name(obj, n=n, silent=TRUE)
	# Check if obj_name is not an empty string, o.w. exists() function below gives an error
	# (the gsub() function removes blanks in the value of obj_name so that if the user passes "   ",
	# nchar() still returns 0, meaning that the name of the object is an empty string)
	if (nchar(gsub(" ", "", obj_name)) > 0) {

		### 1.- First check if the object exists in the root of envir_actual environment
		# In fact the object may exist in the given environment, without need to further search in
		# environments defined within that environment. Of course we also search those below.
		# NOTE that we only check this if envir is not NULL, o.w. the get_env_names() call below
		# will search in ALL environments defined in the workspace (including packages).
		# (if we don't use the condition !is.null(envir) and use envir_actual instead of envir inside
		# the IF block, the first element of env_full_names wil be "<environment>", which is not what we want)
		if (!is.null(envir) && exists(obj_name, envir=envir, inherits=FALSE)) { # inherits=FALSE avoids searching on the enclosing (i.e. parent) environments
			env_full_names = deparse(substitute(envir))
			found = TRUE
		}
		
		### 2.- Look for the object inside environments defined within 'envir'
		# Retrieve all environments existing in the envir environment
		# (note that if envir=NULL, the search is done ovar the WHOLE workspace!)
		envmap = get_env_names(envir=envir)
		
		# Go over all the environments stored in envmap and check if the object is there
		env_full_names = look_for(obj_name, envir, envmap, env_full_names, silent=silent)

		if (is.null(env_full_names)) {
			# If still the object was not found...
			### 3.- Try to see if the object given is an expression whose *evaluation* can be found
			obj_eval <- try(eval(obj, envir=envir_actual), silent=TRUE)
			if (!inherits(obj_eval, "try-error")) {
				if (is.environment(obj_eval)) {
					# When the evaluated object is an evironment, first get the address and then look for this address
					# in the environment map table created above (this is the only way we can get to the name
					# of an environment!! --i.e. through the address-name lookup table)
					# (in fact running get_obj_name() on an environment object returns "<environment>" which is not useful at all! 
					obj_address = envnames:::address(obj_eval)

					# Look for this address in the envmap table created above and
					# return either the "path" or the "location" value (the latter is returned if "path" is empty)
					# In fact, we should not return the "pathname" as for regular (non-environment) objects,
					# since for environment objects the "pathname" will coincide with the environment name!
					# and what we need to know is the **environment where the object is found**... 
					ind = envmap[,"address"] == obj_address
					env_full_names = ifelse(envmap[ind,"path"]!="", as.character(envmap[ind,"path"]), as.character(envmap[ind,"location"]))
						## NOTE: as.character() removes any factor levels that may be present in the envmap columns
						## I define the envmap data frame with the stringsAsFactors=FALSE option but the user may provide
						## an envmap data frame that was not created with this option... 
				}	else {
					# Look for an object that is NOT  an environment in the environments listed in the envmap table created above 
					env_full_names = look_for(obj_name, envir, envmap, env_full_names, silent=silent)
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
    return(invisible(env_full_names))  # Return invisible() because we already printed the environments where the object was found
  } else {
    return(env_full_names)             # Return non-invisible, because we want to show the environments where the object was found
  }
}
