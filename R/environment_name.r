# TODO: (2014/10/18)
# 1) [DONE-2014/10/20] Write a function called setup_env() to be called at the beginning of every function that:
#    adds the address-name pair of the execution environment to the global lookup table containing
#    the execution environments information (lut)
# 2) [DONE-2014/10/20] Write a function called get_env_calling(n=1) that retrieves the name of the environment of
#    any calling function (to be used inside another function for... debugging purposes?)
# 3) Update the setup_env_table() function so that the table includes environments defined in all
# existing environments accessible from the envir environment passed as parameter.

#' Retrieve the name of an environment
#' 
#' Retrieve the name of an environment as \code{\link{environmentName}} in the base package does,
#' but extends its functionality by also retrieving the names of user-defined environments and function
#' environments.
#' 
#' @aliases environment_name
#' @details Environment \code{env} is searched for in environment \code{envir} using the address-name
#' lookup table \code{envmap} defined inside \code{envir}.
get_env_name <- environment_name <- function(env, envmap=.envmap, type="variable", envir=.GlobalEnv)
## For now env should be passed using quote() --e.g. quote(env11)-- unless envir is the global environment,
## in which case it can be also passed without quote().
## This requirement comes from the requirement by get_env_address() --> see the description of parameter 'type' in function get_env_address().
## Note that parameter 'env' can either be an object of the environment class or be a text string containing
## a memory address enclosed in < >. Ex: "<0x0000000008e36338>". In this case specifying the environment is
## NOT necessary because we are already giving the memory address, so R can find it directly!
{
  #env_current = environment()
  #env_parent = parent.env(env_current)
  #env_calling = parent.frame()

  # Get the address of the env environment to look for in the address-names lookup table
  address_match = get_env_address(env, type=type, envir=envir)

  # Look for address_match in the address-names lookup table envmap defined in the envir environment
  envmap = eval(substitute(envmap), envir=envir)
    ## Here is the explanation of how the above evaluation works:
    ## - substitute() evaluates envmap on the function environment and returns .envmap (assuming envmap=.envmap, the default)
    ## - eval() evaluates variable .envmap in the envir environment
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

  return(env_name)
}


