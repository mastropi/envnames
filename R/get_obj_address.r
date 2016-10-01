#' Return the memory address of an object
#' 
#' @param obj object whose memory address is requested. It can be given as a variable name, a string with
#' a variable name, a string with a memory address or an expression.
#' @param envir environment where the object should be searched for. All parent environments of
#' \code{envir} are searched as well.
#' @param n number of levels to go up from the calling function environment to resolve the name
#' of \code{obj}. It defaults to 0 which implies the calling environment.
#'
#' @details
#' The environment name should \emph{not} be part of the object name \code{obj}
#' (e.g. something like \code{env$x} is \emph{not} valid).
#' The call \code{get_obj_address(x, envir=env)} should be used instead.
#' 
#' If \code{obj} is already a memory address, it is returned unchanged.
#' 
#' @return The memory address of the input object given as a string enclosed in <>
#' (e.g. \code{"<0000000005E90988>"}), or NULL under any of the following situations:
#' \itemize{
#' \item the object does not exist in the given environment
#' \item the object is an expression that cannot be evaluated in the given environment
#' \item the object is a non-character constant (e.g. TRUE, 3, etc.)
#' \item the object refers to an unnamed list (e.g. '[[1]] 3', where '[[1]]' is the unnamed list)
#' \item the object is a character constant which however does not resolve to an existing variable in the
#' given environment. That is, the result of running e.g. \code{eval(as.name("aaa"))} is an error.
#' }
#' 
#' Note that for the last three cases, although the cited elements (constants, unnamed lists) have a memory
#' address, this address is meaningless as it changes with every invocation of the function. For instance, running
#' envnames:::address(3) several times will show a different memory address each time, and that is why
#' we chose to return NULL in those cases.  
#' 
#' @examples
#' env1 = new.env()
#' env1$x = 3                       # x defined in environment 'env1'
#' x = 4                            # x defined in the Global Environment  
#' get_obj_address(x, envir=env1)   # Searches for object 'x' in the 'env1' environment
#' get_obj_address("x", envir=env1) # Searches for object 'x' in the 'env1' environment
#' get_obj_address(x)               # Searches for 'x' in the Global Environment
#' 
#' # Memory addresses of objects whose names are stored in an array and retrieved using sapply()
#' env1$y <- 2;
#' objects <- c("x", "y")
#' sapply(objects, FUN=get_obj_address, envir=env1)	# Note that the address of object "x" is the same as the one returned above by get_obj_address(x, envir=env1)
get_obj_address = function(obj, envir=.GlobalEnv, n=0)
# NOTE: (2016/07/26) This function is needed to avoid an error when requesting the address() of an object that
# does not exist. This function returns NULL in such case instead of an error message!
{
  # First check if envir is a valid environment
  if(!is.environment(envir)) {
    envnames:::error_NotValidEnvironment(envir)
    return(NULL)
  }

  #------------------ 1. Try to retrieve the object address using 'obj' as is -----------------
  obj_name = get_obj_name(obj, n=n)
	if (!is.null(obj_name) && obj_name != "" && exists(obj_name, envir=envir, inherits=FALSE)) {
		## NOTE: The conditions !is.null() and != "" are in place because these are not valid arguments for the
		## exists() function. All the other names including invalid names such as "<a" are valid arguments for exists(). 
	  obj_address = address(eval(as.name(obj_name), envir=envir))
			## NOTE: note we are using eval(as.name(obj_name)) instead of eval(obj).
			## It's strange that the latter does not work because eval() first evaluates obj in the calling environment
			## (e.g. it would give 'env9' if obj=env9) and then evaluates the object to which 'obj' resolved to
			## in the environment specified in envir. But, despite this I get the error message that 'env9'
			## is not found when envir is not the Global Environment!
			## We can neither use evalq() because evalq() prevents 'obj' from being evaluated in the
			## current execution environment and in that case the address() function would be returning
			## the memory address of the local variable 'obj', not of the variable referenced by 'obj'!  

			## NOTE also that eval() presumably looks for the object recursively in all parent environments
			## if it is not found in the 'envir' environment, so in principle it does the same thing that
			## exists() does when inherits=TRUE, and we should be ok here in that eval() should not give an
			## error if exists() returned TRUE.
	} else {
		obj_address = NULL
	}

  if (is.null(obj_address)) {
    #--------------- 2. Try to retrieve the object address after evaluating the object --------
    # This is the case when e.g. obj is an expression as in 'objects[1]'
    obj_eval <- try(eval(obj, envir=envir), silent=TRUE)
    if (inherits(obj_eval, "try-error")) {
      # Note that we do NOT show any messages when the object is not found, because the object name cannot
			# always be obtained by deparse(substitute(obj)). In order to get the actual name we need to go back
			# to the n-th calling function and this is a little long to do --see how I did it in get_obj_name()
			# (using a WHILE loop)
      #cat("Object '", deparse(substitute(obj)), "' does not exist in environment '", deparse(substitute(envir)), "'\n", sep="")
      obj_address = NULL
    } else {
			#------------- 2a.- Check if the evaluated object above is an environment ---------------
			if (is.environment(obj_eval)) {
        # When the evaluated object is an evironment we should get the address directly
        # (i.e. without getting the evaluated object's name first, because get_obj_name() on an environment object
        # returns "environment" which is not useful for the environment address retrieval
        obj_address = address(obj_eval)
      } else {
				#----------- 2b.- Check if the evaluated object contains a memory address -------------
				# When the evaluted object is directly given as an address
				# We get here when e.g. obj is 'objects[1]' and the array objects contains memory address strings
				# as in "<0x00000000119dba68>".
				# done: Do a more thorough check for address, by checking that:
				# - [DONE] the string ends with ">"
				# - [NO: the third character does not have to be an "x"! -checked with a few examples] the third character is "x"
				# - [DONE] the address has length 18
				# - [DONE] only alpha-numeric characters with letters from A to F are found in the address (no underscore is allowed)

				# NOTE: This check should come AFTER Step 1 above because o.w. we get an error
				# since in that case the check is done using some expression that includes 'obj' enclosed in try(),
				# and when doing the second try() in step 1, R gives the warning message:
				# "restarting interrupted promise evaluation",
				# which apparently means that we are trying to evaluate again an expression that had already failed.  
				# Ref: http://stackoverflow.com/questions/20596902/r-avoiding-restarting-interrupted-promise-evaluation-warning
				ischaracter = is.character(obj_eval)
				if (ischaracter) {
					# Check if the 'obj' contains a string that is an address
					# Note that the blank space at the beginning of the pattern includes tabs (checked).
					isaddress = grep("^ *<[0-9a-f]{16}>$", obj_eval, ignore.case=TRUE)
					if (length(isaddress) > 0) { # We allow for one or moe spaces at the beginning of the string as in "   <0x00000000119dba68>"
						## Note that if we want to use PERL regular expression we should use double escape to represent
						## special characters as in grep("^\\s*<", obj, perl=TRUE)
						obj_address = obj_eval
					} else {
						obj_address = NULL
					}
				}
				#----------- 2c.- Do the same as step 1 but now for the evaluated object --------------
				# This case is for instance when obj_eval = "x" and "x" may be the name of an object in the given environment 
				if (is.null(obj_address)) {	# This means: obj_address is STILL null, none of the above steps changed it to another non-null value
					# Do the same we tried in step 1 but now for the evaluated object
	        obj_name = get_obj_name(obj_eval, n=n)
	        if (exists(obj_name, envir=envir, inherits=FALSE)) {
	          obj_address = address(eval(as.name(obj_name), envir=envir))
	        } else {
	          obj_address = NULL
	        }
				}
      }
    }
  }

  return(obj_address)
}
