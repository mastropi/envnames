#' Return the execution environment of a function
#' 
#' Return the execution environment of a function by going over the execution environments of all functions in the calling chain.
#' 
#' @param funname name of the function of interest. It should be given with its full path, i.e. including
#' the environment where it is defined (e.g. env1$f), and with no arguments.
#' @param funaddress memory address of the function's execution environment of interest.
#' 
#' @return The execution environment of the function whose name is given as \code{funname} and memory address is given
#' as \code{funaddress}.
get_fun_env <- function(funname, funaddress) {
  # See the following interesting discussions about getting the object associated to a memory address in SO:
  # https://stackoverflow.com/questions/21965665/get-object-by-its-memory-address --> it works around the question of the poster
  # by simulating a linked list via environments!! (nice approach to linked lists)
  # https://stackoverflow.com/questions/29870960/access-object-by-address-pointer --> here they define a find.by.address() function
  # but it cannot be used to get the execution environment of a function! (as I want to do here)

  # Initialize env to NULL in case there is a problem
  # Note that, in order to retrieve the function's execution environment, we need to go through
  # the function calling chain and find where in the chain is the function found.
  # The execution environment is then retrieved by running the sys.frame(-level)
  # where level is the level's back in the calling chain where the function is found.
  # This was verified by running ls(envir=sys.frame(-level)) where I saw the objects
  # defined inside the function!
  # Note that eval(environment(), envir=sys.frame(-level)) does NOT give the execution environment,
  # it just gives the execution environment of the currently executed function (i.e. look_for() in this case)
  env = NULL
  # Get the function calling chain
  all_calls = sys.calls()
  # Iterate on all the calls from the previous to latest to first one so that we go UP in the calling chain
  # (note that we exclude the last call because it is THIS function look_for(), on which we are not interested)
  for (c in seq(length(all_calls)-1, 1, -1)) {
    # Compute the number of levels up in the calling chain starting from the current function (get_env_names)
    level = length(all_calls) - c
    # Get the function name and the address of its execution environment, so that we can check that we are in the function
    # where we want to search for the object. This is important because the same function may be invoked several times in
    # the function calling chain and we need to check each of those environments!
    funname_current = as.character(all_calls[[c]])[1]
    funaddress_current = address(sys.frame(-level))  # Note that the address we need to retrieve is NOT the address of all_calls[[c]] because this gives the address of the object all_calls[[c]] which is of class 'call'. We want the address of the function's execution environment that is currently analyzed in the function calling chain. 
    if (funname_current == funname && funaddress_current == funaddress) {
      # We reached the function whose execution environment we are looking for!
      # => Get the execution environment of the function we are analyzing now (whose name is given by funname)
      
      # IMPORTANT: Note that we use sys.frame(-level) to retrieve the execution environment of the calling function
      # and NOT parent.frame(level)... This is because parent.frames may not always include internal functions in
      # the counting of parent frames (e.g. print() if we call print(get_env_names())), as explained in the Note
      # section of the documentation for sys.parent.
      env = sys.frame(-level) # This is the EXECUTION environment of the function!
      break
    }
  }

  return(env)
}
