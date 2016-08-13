# TODO: (2014/10/18)
# 1) [DONE-2014/10/20] Write a function called setup_env() to be called at the beginning of every function that:
#    adds the address-name pair of the execution environment to the global lookup table containing
#    the execution environments information (lut)
# 2) [DONE-2014/10/20] Write a function called get_env_calling(n=1) that retrieves the name of the environment of
#    any calling function (to be used inside another function for... debugging purposes?)
# 3) Update the setup_env_table() function so that the table includes environments defined in all
# existing environments accessible from the envir environment passed as parameter.
# 4) (2016/03/30) Add the functionality of searching the environment on ALL existing environments, i.e.
#    existing in the global environment and within any defined environment therein.
#    The function should return the name of the environment including the environment where it was found
#    as in e.g. env1$env12
# 5) [DONE-2016/08/13] (2016/03/30) Add the functionality of receiving a memory address in the env parameter and retrieving
#    the environment name associated to the address (of course if the associated variable exists and is
#    an environment!). This would be useful because some functions in R return the memory address of
#    the environment (for instance when retrieving the environment where a function is defined, whenever
#    the function is defined within a user-defined environment (as in with(env1, f <- function(x) { })))
# 	 or when running e.g. options() we get a list of defined functions with the address of 
# 	 the environment where they are defined at the end of the function definition and we may want to know
#		 in which environment (name) the function is defined.
#    UPDATE: (2016/03/30) note that the returned value of e.g. environment(env1$f) in the example just given
#		 correctly returns the environment 'env1' (i.e. environment_name(environment(env1$f)) returns "env1")  

#' Retrieve the name of an environment
#' 
#' Retrieve the name of an environment as \code{\link{environmentName}} in the base package does,
#' but extends its functionality by also retrieving the names of user-defined environments and function
#' environments.
#' 
#' @param env environment variable whose name is requested. See details for different types of input
#' variables.
#' @param envir environment where \code{env} should be searched for.
#' @param envmap data frame containing a lookup table with name-address pairs of environment names and
#' addresses to be used when searching for environment \code{env}. Defaults to NULL which means that the
#' lookup table is constructed on the fly with the environments defined in the \code{envir} environment.
#' See the @details for more information on its structure.
#' 
#' @details
#' The \code{env} parameter should be a variable of class \code{environment}. It can be given either
#' as the variable name or in the form of <environment: [memory-address]> as in e.g.
#' <environment: 0x0000000009030df8>. The latter form is for instance returned by the function
#' environment() called on a function, which returns the environment of definition of the function.
#' Environment \code{env} is searched for in environment \code{envir} using its memory address.
#' It may happen that there exist more than one environment with the same memory address (for instance
#' if an environment is a copy of another environment). In such case, the names of all the environments
#' matching the same memory address as the one given in \code{env} are returned.
#' 
#' If \code{envmap} is passed it should be a data frame with at least the following columns:
#' \code{name} and \code{address}, where \code{name} contains the environment name and \code{address}
#' contains its memory address. 
#' 
#' @return A string containing the name of the environments defined in the \code{envir} environment
#' whose memory address matches that of the \code{env} environment given, or NULL if the environment is
#' not found in the \code{envir} environment or in the global environment.
#' @aliases get_env_name
environment_name <- function(env, envir=.GlobalEnv, envmap=NULL)
{
  # Output variable
  env_name = NULL

  #env_current = environment()
  #env_parent = parent.env(env_current)
  #env_calling = parent.frame()

  # Setup the address-name pairs of the environments defined in envir if the envmap variable is not passed
	if (is.null(envmap)) {
  	envmap = get_env_names(envir=envir)
	}

  if (!is.null(envmap)) { # This means that parameter 'envir' is a valid environment
    # Get the address of the env environment to look for in the address-names lookup table just created
    env_address = get_obj_address(env, envir=envir, n=2)
    
    # Look for env_address in the address-name lookup table envmap created above
    if (!is.null(env_address)) {
      # Look for the address in the first column of envmap
      ind = which(envmap[,1] == env_address)
      if (length(ind) > 0)
        env_name = as.character(envmap[ind,2])  # Remove any factor attribute with as.character()
    }
  }

  return(env_name)
}

get_env_name <- environment_name