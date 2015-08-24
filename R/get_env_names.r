# Returns a data frame containing the address-name pairs of environments existing in the envir environment
# AND all environments returned via search()
get_env_names = function(envir=.GlobalEnv) {
  # Initialize the output lookup table to NULL in case the envir environment does not exist
  env_table = NULL

  # Names of the currently defined environments in the envir environment as they are given
  # at the time of their creation with new.env() (e.g. "env1")
  env_names = try( with(envir, Filter(function(x) "environment" %in% class(get(x)), ls())), silent=TRUE )
  if (!inherits(env_names, "try-error")) {
    # Store the way R shows an environment (e.g. <environment: 0x00000000107eb718>, where the number is the memory address) 
    env_resolves = lapply(env_names, get, envir=envir) # NOTE: The envir=envir parameter is used by get(). Note that using (with(envir, lapply()) does NOT work, because get() applies on the global environment...)
    # Extract the memory address from the above (this is the difficult part!)
    env_addresses = eval( unlist( lapply(env_resolves, get_env_address) ), envir=envir)

    # Now get the address-name pairs of existing environments (e.g. .GlobalEnv, package:stats, package:base, etc.)
    # that can be reached from the envir environment
    allenvs = search()
    env_addresses_packages = vapply(search(), function(x) get_env_address(as.environment(x), type="package"), FUN.VALUE=character(1))
    env_addresses = c(env_addresses, env_addresses_packages)
    env_names = c(env_names, names(env_addresses_packages))

    env_table = data.frame(address=env_addresses, name=env_names)
  } else {
    error_NotValidEnvironment(deparse(substitute(envir)))
  }

  return(env_table)
}
