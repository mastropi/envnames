#' Find an object in a given environment
#' 
#' Check if an object is reachable from the given environment (parameter \code{envir}) either because:
#' \itemize{
#' \item it exists in the given environment
#' \item it exists in an environment defined inside the given environment
#' \item it is reachable via the search() path
#' }
#' Note that \code{obj_find} differs from \code{exists} in that the object is NOT searched in parent environments,
#' since all checks of object existence call \code{exists} using \code{inherits=FALSE}. This is so because the
#' function is aimed at reporting the names of the environment where the object is found, something that is not
#' readily given by the \code{exists} function.
#' On the contrary, \code{obj_find} looks for the object in all the environments defined \bold{inside} the given environment.
#' \cr
#' \cr
#' When the object is found, a vector containing the names of all the environments where the object was found is
#' returned, including the names of user-defined environments.
#' 
#' @param obj object to be searched in the \code{envir} environment, given as the object itself or as a character string.
#' @param envir environment used as basis for the search for object \code{obj} as explained in the description.
#' @param silent run in silent mode? Use \code{FALSE} to show the search history, which lists
#' the environments that are searched for object \code{obj}.
#' @return A vector containing the names of the environments where the object is found.
#' @examples 
#' # Define a variable in the global environment
#' x = 4
#' # Create a new environment
#' env1 = new.env()
#' env1$x = 3
#' env1$y = 5
#' 
#' # Look for object x in the global environment
#' obj_find(x)   # "env1" ".GlobalEnv"
#' obj_find("x") # "env1" ".GlobalEnv"
#' obj_find("x", envir=env1)  # "env1" ".GlobalEnv" (as .GlobalEnv is in the search path)
#' obj_find("y") # "env1"
#' obj_find(nonexistentObject)  # NULL (note that NO error is raised even if the object does not exist)
obj_find = function(obj, envir=.GlobalEnv, silent=TRUE)
{
  # Extract the name of the object
  # (i.e. the string of the object passed in obj when obj is NOT a string --e.g. when obj = x => obj_name = "x")
	obj_name = get_obj_name(obj)

  # Get the name of the envir environment
  envir_name = deparse(substitute(envir))

  # Initialize the output variable
  env_names = NULL
  found = FALSE
  
  error = FALSE
  tryCatch(
    if (class(envir) != "environment") {
			envnames:::error_NotValidEnvironment(envir_name)
      error = TRUE
    },
    error=function(e) {
            envnames:::error_NotValidEnvironment(envir_name); assign("error", TRUE, inherits=TRUE)
            ## Note the use of the inherits=TRUE parameter which means: search for the variable to be assigned in parent environments and assign the value to the first one found.
          }
  )
  if (error) return(invisible(NULL))

  # Check if obj_name is not an empty string, o.w. exists() function below gives an error
  # (the gsub() function removes blanks in the value of obj_name so that if the user passes "   ",
  # nchar() still returns 0, i.e. the name of the object is an empty string)
  if (nchar(gsub(" ", "", obj_name)) > 0) {

    ### 1.- First check if the object exists in the envir environment
    # Note that this is only performed when the envir environment is NOT the .GlobalEnv environment
    # since in that case, the global environment will still be added in step (2) as .GlobalEnv is part
    # of the search() path.
    if (envir_name != ".GlobalEnv" && exists(obj_name, envir=envir, inherits=FALSE)) { ## inherits=FALSE avoids searching on the enclosing (i.e. parent) environments
      env_names = deparse(substitute(envir))
      found = TRUE
    }

    ### 2.- Check if the object is defined in any environment defined inside the envir environment or if it can be reached by the search() path.
    # Retrieve all environments existing in the envir environment
    envmap = get_env_names(envir=envir)

    # Go over all the environments stored in envmap and check if the object is there
    i = 0
    n = nrow(envmap)
    for (address in envmap[,"address"]) {
      i = i + 1
			env_type = as.character(envmap[i,"type"])
      env_address = as.character(envmap[i,"address"])
      env_name = as.character(envmap[i,"name"])
      if (!silent)
        cat(i, "of", n, ": Inspecting environment", env_name, "...\n")
  
      # Get the environment from the currently analyzed envmap entry
      # Need to check if the current entry corresponds to an unnamed environment or to a named environment
      if (env_type == "user") {
        # Case for unnamed environments (e.g. those created with new.env())
        env = get(env_name, envir=envir)
      } else {
        # Case for named environments (mostly packages) (e.g. .GlobalEnv, package:stats, etc.)
        env = as.environment(env_name)
      }

      # Check whether the object exists in the currently analyzed environment
      if (exists(obj_name, envir=env, inherits=FALSE)) { ## inherits=FALSE avoids searching on the enclosing (i.e. parent) environments
          env_names = c(env_names, env_name)
          found = TRUE
      }
    }
  }

  if (!silent) {
    if (found) {
      cat("Object", obj_name, "found in the following environments:\n")
      print(env_names)
    } else {
      cat("The object was not found in any environment\n");
    }
    
    return(invisible(env_names))  # Return invisible() because we already printed the environments where the object was found
  } else {
    return(env_names)             # Return non-invisible, because we want to show the environments where the object was found
  }
}
