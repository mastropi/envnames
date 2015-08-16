#' Find an object in a given environment.
#' 
#' @param obj object to be searched in the \code{envir} environment, given as the object itself or as a character string.
#' A character string should be given if the object is not defined in the calling environment,
#' o.w. an object-not-found error will be raised.
#' @param envir environment where object \code{obj}.
#' @param silent run in silent mode? Use \code{TRUE} to hide the search history, which lists
#' the environments that are searched for object \code{obj}.
#' @return The name of the environment to which the object belongs.
#' 
obj_find = function(obj, envir=.GlobalEnv, silent=FALSE)
## obj is the name of the object to look for (NOT enclosed in parenthesis)
{
  # Extract the name (i.e. string of the object passed in obj when obj is NOT a string --e.g. obj=x => obj_name = "x")
  if (is.character(obj)) {
    obj_name = obj
  } else {
    obj_name = deparse(substitute(obj))
  }

  # Retrieve all environments existing in the envir environment
  envmap = get_env_names(envir=envir)

  # Go over all the environments stored in envmap and check if the object is there
  env_names = NULL
  i = 0
  n = nrow(envmap)
  found = FALSE
  for (address in envmap[,1]) {
    i = i + 1
    env_address = as.character(envmap[i,1])
    env_name = as.character(envmap[i,2])
    if (!silent) {
      cat(i, "of", n, ": Inspecting environment", env_name, "...\n")
    }
    
    # Get the environment from the currently analyzed envmap entry
    # Need to check if the current entry corresponds to an unnamed environment or to a named environment
    if (substr(env_address, 1, 1) == "<") {
      # Case for unnamed environments (e.g. those created with new.env())
      env = get(env_name, envir=envir)
    } else {
      # Case for named environments (e.g. .GlobalEnv, package:stats, etc.)
      env = as.environment(env_name)
    }
    
    # Check whether the object exists in the currently analyzed environment
    if (exists(obj_name, envir=env, inherits=FALSE)) { # inherits=FALSE avoids searching on the enclosing (i.e. parent) environments
      env_names = c(env_names, env_name)  # Remove any factor attribute with as.character()
      found = TRUE
    }
  }
  
  if (!silent) {
    if (found) {
      cat("Object", obj_name, "found in the following environments:\n")
      print(env_names)
    } else {
      cat("The object was not found in any environment\n");
    }
    
    return(invisible(env_names))
    
  } else {
    return(env_names)
  }
}
