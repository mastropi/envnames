#' Create a data frame with address-name pairs of environments
#' 
#' Return a data frame containing the address-name pairs of environments existing in the given environment
#' and all environments that are part of the search() path.
#' 
#' @param envir environment where environments are searched for to construct the data frame with address-name pairs.
#' @return A data frame containing two columns: \code{address} and \code{name}, respectively listing the
#' memory address of the environments defined in the \code{envir} environment and their corresponding names.
#' It also lists the memory address and names of packages that are part of the search path.
#' @examples
#' # Address-name pairs of environments and search paths in the global environment
#' get_env_names()
#' 
#' # Address-name pairs of environments and search paths in a user-defined environment
#' env1 = new.env()
#' env11 = with(env1, new.env())
#' get_env_names(env1)
get_env_names = function(envir=.GlobalEnv) {
  # Initialize the output lookup table to NULL in case the envir environment does not exist
  env_table = NULL
  
  # Names of the currently defined environments in the envir environment as they are given
  # at the time of their creation with new.env() (e.g. "env1")
  # NOTE: Either of the two statements below work (one of them is commented out)
  # Note in the first option the need to use the option envir=envir in the get() function,
  # and this is because the list of variables returned by ls(envir) reside in the envir environment.
  #env_names = try( Filter(function(x) "environment" %in% class(get(x, envir=envir)), ls(envir)), silent=TRUE )
  env_names = try( with(envir, Filter(function(x) "environment" %in% class(get(x)), ls())), silent=TRUE )
  if (!inherits(env_names, "try-error")) {
    # Store the way R shows an environment (e.g. <environment: 0x00000000107eb718>, where the number is the memory address) 
    env_resolves = lapply(env_names, get, envir=envir) # NOTE: The envir=envir parameter is used by get(). Note that using (with(envir, lapply(env_names, get)) does NOT work, because get() in that case runs on the global environment...)
    # Extract the memory address from the above (this is the difficult part!)
    env_addresses = eval( unlist( lapply(env_resolves, get_obj_address) ), envir=envir)

    # Now get the address-name pairs of existing environments (e.g. .GlobalEnv, package:stats, package:base, etc.)
    # that can be reached from the envir environment
    allenvs = search()
    env_addresses_packages = vapply(search(), function(x) get_obj_address(as.environment(x)), FUN.VALUE=character(1))
              ## NOTE: FUN.VALUE in the vapply() function is a required parameter.
              ## It is used to specify the type and length of the value returned by the function called by vapply().
              ## In this case (FUN.VALUE=character(1)) we are saying that the function should return
              ## a vector of length 1 of type character.
    env_addresses = c(env_addresses, env_addresses_packages)
    env_names = c(env_names, names(env_addresses_packages))

    env_table = data.frame(address=env_addresses, name=env_names)
  } else {
    envnames:::error_NotValidEnvironment(deparse(substitute(envir)))
  }

  return(env_table)
}
