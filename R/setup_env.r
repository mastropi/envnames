#' Set up the execution environment
#' 
#' Add the execution environment (of the calling function) to the global address-name lookup table .envmap
#' defined in the environment where the calling function is defined.
#' 
#' @return It returns (invisibly) the memory address of the execution environment (i.e. of the function calling
#' \code{setup_env}) as a character string.
#' @note It is assumed that the environment of the calling function has NOT been changed after its creation
#' using e.g. the \code{environment} function. If this is the case, \code{setup_env} will not correctly
#' return the memory address of the execution environment.
#' @examples 
#' f <- function() {
#'  f_env_address = setup_env()
#'  f_env_name = get_env_name(environment())
#'  cat("Address and name of the function's execution environment:", f_env_address, ",", f_env_name, "\n")
#' }
#' f()   # Address and name of the function's execution environment: <...> , .GlobalEnv:f
#' 
#' # Define the same function f in a user-defined environment
#' env1 = new.env()
#' with(env1, f <- .GlobalEnv$f)
#' # Change the environment of env1$f (o.w. the environment is set to .GlobalEnv)
#' environment(env1$f) = env1
#' env1$f()  # environment name is env1:f
setup_env = function()
{
  # Change the warning level to avoid a warning message when trying to convert a memory address below with as.numeric()
  op = options("warn"); options(warn=-1); on.exit(options(warn=op$warn))
  
  # Define the environment of the envnames package, which is where the lookup table .envmap is created
  #envir_pkg = as.environment("package:envnames")
  envir_pkg = .GlobalEnv
  
  # Setup: Prepare lookup table to hold the address of the execution environment (of the calling function, here called env_current)
  env_current = parent.frame(n=1)       # Execution environment of the calling function
  fname = get_fun_name(n=1)             # Name of the calling function
  env_parent = parent.env(env_current)  # Parent environment, i.e. the environment where the calling function resides
                                        # In other words, this is the environment where the execution environment of the calling function belongs to.
                                        # IT IS ASSUMED that the environment of the function was NOT changed using the environment() function.
                                        # If this is the case, this will fail, because env_parent will NOT contain
                                        # the environment where the function is defined! (which is what we are interested in here)
  if (environmentName(env_parent) == environmentName(globalenv())) {
    # If env_parent is .GlobalEnv do not go further up (because the parent of the .GlobalEnv is a package!
    # and env_parent2 should point to the environment where env_parent is defined. As far as I know
    # .GlobalEnv is not defined within another package!!
    env_parent2 = .GlobalEnv
  } else {
    env_parent2 = environment(env_parent) # Environment of the env_parent environment.
      ## This is needed to retrieve the name of the environment via a lookup table of address-name pairs.
      ## NOTE that we first try to retrieve the environment through the environment() function
      ## and NOT through the parent.env() function, because sometimes we may define
      ## environments by explicitly specifying its parent environment with the parent= option
      ## of the new.env() function.
      ## If this is the case, it is assumed that the user stored the environment
      ## where it was defined under the environment() property of the environment
      ## (by doing e.g. environment(env1) = globalenv())
      ## In most practical cases, environment(env_parent) will return NULL and
      ## in this case we compute env_parent2 as the parent environment of env_parent (below).
    if(is.null(env_parent2)) {
      # If environment(env_parent) returns NULL, compute the parent environment (which may be manipulated by the user when creating the environment)
      env_parent2 = parent.env(env_parent)
    }    
  }

  # Create the environment names table of the environments existing in the env_parent2 environment.
  # We need this because the env_parent2 environment is where we need to look for the name of the
  # env_parent environment.
  # Note that the lookup table 'env_table' is created in the env_parent2 environment
  # (this is what the assign() function is doing) because below we retrieve the parent environment
  # name by calling environment_name() with envir=env_parent2 => the env_table is evaluated in the
  # env_parent2 environment.
  assign("env_table", get_env_names(envir=env_parent2), envir=env_parent2)
  
  # Check if env_parent has a name or is just a memory address in order to define the type of address
  # to look for with the environment_name() function (either type="package" or type="variable", respectively)
  env_parent_address = get_env_address(env_parent, envir=env_parent2)
  try_asnumeric = try( as.numeric( substr(env_parent_address, 2, nchar(env_parent_address)-1) ), silent=TRUE )
  if (!is.na(try_asnumeric)) {
    type = "variable"
  } else {
    type = "package"
  }
  
  # Get the name of the parent environment (of the calling function execution environment)
  env_parent_name = environment_name(env_parent, env_table, type=type, envir=env_parent2)

  # Get the address of the execution environment of the calling function
  env_address = get_env_address(env_current, envir=env_parent)
    ## IMPORTANT: env_current should NOT be quoted with quote(), otherwise env_current is not found
    ## in the parent environment, because env_current does not have a name, it's directly the memory
    ## address of the environment.

  # Add the entry to the .envmap table (and create it if it does not exist)
  if (!exists(".envmap", envir=envir_pkg)) {
    # Create an empty .envmap data frame
    assign(".envmap", as.data.frame( matrix(nrow=0, ncol=2) ), envir=envir_pkg)
    names(.envmap) = c("address", "name")
  }
  assign(".envmap",  rbind(.envmap, data.frame(address=env_address, name=paste(env_parent_name, fname, sep=":"))), envir=envir_pkg)
  #cat("Following pair added to the lookup table .envmap:\n")
  #print(.envmap[nrow(.envmap),])
  
  return(invisible(env_address))
}
