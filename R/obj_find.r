# Returns the name of the environment to which a particular object belongs
obj_find = function(obj, envir=.GlobalEnv, silent=FALSE)
## obj is the name of the object to look for (given as a character string)
{  
  # Retrieve all environments existing in the envir environment
  envmap = setup_env_table(envir=envir)
  
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
    if (exists(obj, envir=env, inherits=FALSE)) { # inherits=FALSE avoids searching on the enclosing (i.e. parent) environments
      env_names = c(env_names, env_name)  # Remove any factor attribute with as.character()
      found = TRUE
    }
  }
  
  if (!silent) {
    if (found) {
      cat("Object", obj, "found in the following environments:\n")
      print(env_names)
    } else {
      cat("The object was not found in any environment\n");
    }
    
    return(invisible(env_names))
    
  } else {
    return(env_names)
  }
}
