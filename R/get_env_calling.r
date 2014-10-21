# Returns the environment name of the function being called n+1 levels counted back from the present function environment
get_env_calling = function(n=1)
# When n=0 (default) the function returns the environment name of the calling function.
{
  # Setup: get information about the calling environment at level n+1 of the chain
  # that is, the environment of the function n levels back from the calling function
  env_calling = parent.frame(n=1+n)
  #cat("Calling environment", n, "levels back:\n")
  #print(env_calling)
  
  # Memory address of environment env_calling as text
  # Note that we can find environment 'env_calling' inside the current environment
  # (i.e. the currently executing environment) because we have just defined env_calling
  # in the execution environment!)
  calling_address = get_env_address(env_calling, envir=environment())
    ## Note: do NOT use quote() to enclose env_calling, because it is in the form of an address.
    ## This is so because env_calling corresponds to the execution environment of the calling function
    ## (which does not have a name, just an address).
  #cat("Calling address:\n")
  #print(calling_address)
  calling_name = as.character( .lut[ .lut[,1]==calling_address, 2 ] )
  
  return(calling_name)
}