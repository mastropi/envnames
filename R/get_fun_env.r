#' Return the execution environment of a function
#' 
#' Return the execution environment of a function by going over the execution environments of all functions
#' in the calling chain.
#'
#' @param fun_name_or_address string containing either the name of the function of interest or the
#' memory address of the execution environment to retrieve (N.B. this sould not be the memory address
#' of the \emph{function itself}, but the memory address of its \emph{execution environment}).
#' When the function name is given, it should be given with its full path, i.e. including
#' the environment where it is defined (e.g. "env1$f") and with no arguments.
#' 
#' @details This function is expected to be called from within a function. Otherwise, the function calling chain
#' is empty and the function returns \code{NULL}.
#' 
#' @return When the input parameter is a memory address, the execution environment of the function
#' whose memory address (of the execution environment) equals the given memory address.
#' 
#' When the input parameter is a function name, a list of ALL the execution environments belonging
#' to a function whose name coincides with the given name. Note that these may be many environments
#' as the same function may be called several times in the function calling chain.
#' 
#' @examples
#' # Define the function that is called to show the behaviour of get_fun_env()
#' h <- function(x) {
#'   # Get the value of parameter 'x' in the execution environment of function 'env1$g'
#'   # If function 'env1$g' is not found, 'x' is evaluated in the current environment or function
#'   xval = evalq(x, get_fun_env("env1$g")[[1]])
#'   return(xval)
#' }
#' # Define the function that calls h() in a user-defined environment 
#' env1 <- new.env()
#' with(env1, 
#'   g <- function(y) {
#'     x = 2
#'     return( h(y) )
#'   }
#' )
#' # Call env1$g()
#' cat("The value of variable 'x' inside env1$g is", env1$g(3), "\n") 
#'   ## Prints '2', because the value of x inside env1$g() is 2
#'   ## ('3' is the value of variable 'y' in env1$g(), not of variable 'x')
#' 
#' # When get_fun_env() is called from outside a function, it returns NULL
#' get_fun_env("env1$g")  # NULL, even if function 'g' exists,
#'                        # but we are not calling get_fun_env() from a function
#' 
get_fun_env <- function(fun_name_or_address) {
  # In order to retrieve the function's execution environment, we need to go through
  # the function calling chain and find where in the chain the function is found.
  #
  # See the following interesting discussions about getting the object associated to a memory address in SO:
  # https://stackoverflow.com/questions/21965665/get-object-by-its-memory-address
  # --> it works around the question of the poster by simulating a linked list via environments!!
  # (nice approach to linked lists)
  # https://stackoverflow.com/questions/29870960/access-object-by-address-pointer
  # --> here they define a find.by.address() function but it cannot be used to get the execution environment
  # of a function! (as I want to do here)

	#----------------------------- Auxiliary functions --------------------------
	# NOTES for the next two functions about retrieving a function's execution environment:
	# 1) We use sys.frame(-level) to retrieve the function's execution environment of a calling
	# function and NOT parent.frame(level), because parent.frames may not always include internal functions in
	# the counting of parent frames (e.g. print() if we call print(get_env_names())), as explained in the Note
	# section of the documentation for sys.parent.
	# 2) We CANNOT use eval(environment(), envir=sys.frame(-level)) to do that because this just gives
	# the execution environment of the currently executed function

	# Execution environment by memory address
	# Return the function execution environment associated to the given memory address
	# calls is the object returned by sys.calls() with the function calling chain
	# funaddress is the memory address of the function execution environment we want to retrieve
	find_function_by_address = function(calls, funaddress) {
		# Output variable
		env = NULL
		
		# Iterate on all the calls from the "previous to latest" to first one so that we go UP in the calling chain
		# (note that we exclude the last call (we start from length(calls)-1 instead of from length(calls))
		# because it is THIS function on which we are not interested)
		for (c in seq(length(calls)-1, 1, -1)) {
			# Compute the number of levels up in the calling chain starting from the current function (get_env_names)
		  # Note that we need to go one level further (+1) because here we are INSIDE another function which
		  # is NOT counted in the calling chain passed in input parameter calls
		  level = length(calls) - c + 1

			# Get the function's execution environment and compare its memory address with the given memory address
			# Note that the address we need to retrieve is NOT the address of calls[[c]]
			# because this gives the address of the OBJECT calls[[c]] which is of class 'call'.
			# We want the address of the function's execution environment that is currently analyzed in the function calling chain.
			funaddress_current = address(sys.frame(-level)) 
			if (funaddress_current == funaddress) {
				# We reached the function whose execution environment we are looking for!
				# => Get the execution environment of the function we are analyzing now
				
			  # Note that we need to go one level further (-1) because here we are INSIDE another function which
			  # is NOT counted in input parameter calls
			  env = sys.frame(-level) # This is the EXECUTION environment of the function!
				break
			}
		}

		return(env)
	}

	# Execution environments by function name
	# Return ALL the execution environments associated to the given function name
	# calls is the object returned by sys.calls() with the function calling chain
	# funname is a string with the name of the function whose execution environment we want to retrieve
	find_function_by_name = function(calls, funname) {
		# Output variable
		envs = NULL

		# Iterate on all the calls from the "previous to latest" to first one so that we go UP in the calling chain
		# (note that we exclude the last call (we start from length(calls)-1 instead of from length(calls))
		# because it is THIS function on which we are not interested)
		for (c in seq(length(calls)-1, 1, -1)) {
			# Compute the number of levels up in the calling chain starting from the current function (get_env_names)
		  # Note that we need to go one level further (+1) because here we are INSIDE another function which
		  # is NOT counted in the calling chain passed in input parameter calls
		  level = length(calls) - c + 1

			# Get the function's execution environment and compare its memory address with the given memory address
			# where we want to search for the object. This is important because the same function may be invoked several times in
			# the function calling chain and we need to check each of those environments!
			funname_current = as.character(calls[[c]])[1]
			if (funname_current == funname) {
				# Add the environment to the list of environments associated with functions called 'funname'
				envs = c(envs, sys.frame(-level))
			}
		}

		return(envs)
	}	
	#----------------------------- Auxiliary functions --------------------------


	#-------------------------------- FUNCTION STARTS ---------------------------
  # Get the function calling chain and check if this function was called from within another function
  # (recall that sys.calls() returns first the CURRENT function and then the other functions in the chain,
  # so this is why we compare its length with > 1)
  if (length(calls <- sys.calls()) <= 1) return(NULL)

	# Check if the given parameter is a memory address
	if (is_memory_address(fun_name_or_address)) {
		# => The function returns only ONE environment, since all environments have distinct memory addresses
		return(find_function_by_address(calls, fun_name_or_address))
	} else {
		# The function returns a list of environments (as e.g. the same function may have been called several times!)
		return(find_function_by_name(calls, fun_name_or_address))
	}
}
