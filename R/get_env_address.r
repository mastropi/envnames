# Function that retrieves the memory address of an environment existing in the envir environment as text
get_env_address = function(env, type="variable", envir=.GlobalEnv)
  ## For now env should be passed using quote() --e.g. quote(env11)-- unless envir is the global environment, in which
  ## case it can also be passed without quote()
  ## Note that parameter 'env' can either be an object of the environment class or be a text string containing
  ## a memory address enclosed in < >. Ex: "<0x0000000008e36338>". In this case specifying the environment is
  ## NOT necessary because we are already giving the memory address, so R can find it directly!
  ## (Of course this is not useful inside the function called get_env_address, but still it's useful to note)
  ## Parameter 'type' can be "variable" or "package" and affects where in the inspected environment the memory
  ## address is retrieved from:
  ## - when type="variable" it is retrieved from the end of the string (e.g. "<0x0000000008e36338>"),
  ## - when type="packages" it is retrieved from the beginning (e.g. "@0x0000000008e36338")
  ## Usually the memory address for environment "variables" also appear at the beginning of the inspected output
  ## as is the case for environment "packages". But this is NOT always the case, for example for the addresses
  ## of execution enviroments, which show up ONLY at the end of the second line of the inspected object.
{
  # Inspect object 'env'
  env.inspect = obj_inspect(env, envir=envir)
  
  # Extract the memory address from the inspection
  # THE FOLLOWING IF COMES BECAUSE OF A VERY STRANGE BEHAVIOUR OF THE inspect() FUNCTION IN obj_inspect()
  # WHICH RETURNS DIFFERENT THINGS DEPENDING ON WHETHER get_env_address() IS CALLED TO HANDLE THE FUNCTION
  # CALL SITUATION (Andrea's wish) OR THE NORMAL SITUATION OF JUST RETRIEVING THE ENVIRONMENT NAME (see more below).
  if (length(env.inspect) == 2) {
    # Case when the the call to get_env_address() comes from within a function (like the case "g called by f")
    address = env.inspect[2]
    #address = substr(address, 1, nchar(address)-1)  # Remove the last character which is not ">" but "\".
  } else {
    # Normal case
    address = env.inspect[1]
  }
  if (!is.null(address)) {
    # (address is NULL when the environment env is not found in environment envir)
    if (type == "variable") {
      # Look for the < and > symbols enclosing the environment name or address
      # Note the use of the max() function so that we get the max position where the
      # characters are found, since the address appears at the end of the string stored in address.
      pos_start = max(gregexpr("<", address)[[1]][1])
      pos_end = max(gregexpr(">", address)[[1]][1])
    } else {
      # For packages
      pos_start = 1
      pos_end = regexpr(" ", address)[[1]][1] - 1
    }
    
    # Retrieve the memory address
    address = substr(address, pos_start, pos_end)
  }
  
  return(address)
}
