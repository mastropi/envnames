get_calling_chain = function() {
  # Generate an address-name pairs table of all the environment and functions in the calling chain
  # NOT NEEDED BECAUSE environment_name() BELOW ALREADY GENERATES THIS MAPPING TABLE!
#  env_table = get_env_names(envir=globalenv())

  # Get the calling function's environment
  n = 1
  env = sys.frame(sys.parent(n))
  # Get the environment where the calling function is defined (normally the Global Environment)  
  enclosing_env = parent.env(env)

  while (!identical(env, globalenv())) {
    env_address = get_env_address(env)
      ## NOTES:
      ## - No need to enclose 'env' in quote()
      ## - No need to specify the environment where 'env' is defined (i.e. the enclosing environment)
    cat("Address of calling environment at level", n, ":", env_address, "\n")
    cat("Name of enclosing environment of calling function at level", n, ":", environment_name(enclosing_env), "\n")

    # Get the environment of the calling function at the next level of the calling chain and its environment
    n = n + 1
    env = sys.frame(sys.parent(n))
    enclosing_env = parent.env(env)
  }
  cat("Reached global environment\n")
}
