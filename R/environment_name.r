# TODO: (2014/10/18)
# 1) Write a function called setup_env() to be called at the beginning of every function that:
#    adds the address-name pair of the execution environment to the global lookup table containing
#    the execution environments information (lut)
# 2) Write a function called get_env_name_calling(n=1) that retrieves the name of the environment of
#    any calling function.
#


# Function that retrieves the name of an environment existing in environment 'envir' using the address-name lookup table 'envmap'
environment_name = function(env, envmap, type="variable", envir=.GlobalEnv)
## For now env should be passed using quote() --e.g. quote(env11)-- unless envir is the global environment
## This requirement comes from the requirement by get_env_address()
## See the description of parameter 'type' in function get_env_address().
{
  #env_current = environment()
  #env_parent = parent.env(env_current)
  #env_calling = parent.frame()
  
  # Get the address of the env environment to look for in the address-names lookup table
  address_match = get_env_address(env, type=type, envir=envir)

  # Look for address_match in the address-names lookup table envmap
  env_name = NULL
  found = FALSE
  if (!is.null(address_match)) {
    #cat("Matched address:", address_match, "\n")
    
    # Go over the addresses stored in the first column of envmap and check whether the object passed is found there
    i = 0
    for (address in envmap[,1]) {
      i = i + 1
      if (address == address_match) {
        env_name = as.character(envmap[i,2])  # Remove any factor attribute with as.character()
        found = TRUE
        break
      }
    }
  }
  
  #if (found) {
  #  cat("The environment name is:", env_name, "\n")
  #} else {
  #  cat("The environment was not found.\n");
  #}
  
  return(invisible(env_name))
}


