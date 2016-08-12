#' Return the memory address of an object existing in a given environment
#' 
#' @param obj object whose memory address is requested. It can be given as a variable name, as a string or
#' as an expression. The environment name should NOT be part of the object name (e.g. \code{env$x} is
#' NOT valid)
#' @param envir environment where the object should be searched for. All parent environments of
#' \code{envir} are searched as well.
#' @param n number of levels to go up from the \code{get_obj_address} environment to resolve the name
#' of \code{obj}. It defaults to 1 which implies the calling environment. This parameter is useful in very
#' special circumstances, and normally should be left at its default value.
#' 
#' @return the memory address of the object as a string enclosed in <>. Ex: "<0000000005E90988>"
#' 
#' @examples
#' env1 = new.env()
#' env1$x = 3												# x defined in environment 'env1'
#' x = 4														# x defined in the Global Environment  
#' get_obj_address(x, envir=env1)		# Searches for object 'x' in the 'env1' environment
#' get_obj_address("x", envir=env1)	# Searches for object 'x' in the 'env1' environment
#' get_obj_address(x)								# Searches for 'x' in the Global Environment
#' 
#' # Memory addresses of objects whose names are stored in an array and retrieved using sapply()
#' env1$x <- 3; env1$y <- 2;
#' objects <- c("x", "y")
#' sapply(objects, FUN=get_obj_address, envir=env1)
get_obj_address = function(obj, envir=.GlobalEnv, n=1)
# NOTE: (2016/07/26) This function is needed to avoid an error when requesting the address() of an object that
# does not exist. This function returns NULL in such case instead of an error message!
{
  # First check if envir is a valid environment
  if(!is.environment(envir)) {
    envnames:::errorNotValidEnvironment(envir)
    return(NULL)
  }

  #-------------------- 1. Try to retrieve the object address as is ---------------------------
  obj_name = get_obj_name(obj, n=n)

	if (exists(obj_name, envir=envir, inherits=FALSE)) {
#	  cat("ENTRO 1\n")
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
#    cat("ENTRO 2\n")
    #--------------- 2. Try to retrieve the object address after evaluating the object --------
    # This is the case when e.g. obj is an expression as in 'objects[1]'
    obj_eval <- try(eval(obj, envir=envir), silent=TRUE)
    if (inherits(obj_eval, "try-error")) {
      # Do not show a message when the object is not found, because the object name is NOT deparse(substitute(obj))
      # and in order to get the actual name we need to go back to the n-th calling function and this is a little
      # long to do --see how I did it in get_obj_name() (using a WHILE loop)
      #cat("Object '", deparse(substitute(obj)), "' does not exist in environment '", deparse(substitute(envir)), "'\n", sep="")
      obj_address = NULL
    } else {
      if (is.environment(obj_eval)) {
        # When the object is an evironment we should get the address directly
        # (i.e. without getting the object name first, because get_obj_name() on an environment object
        # returns "environment" which doesn't make any sense for the environment address retrieval
#        cat("ENTRO 2a\n")
        obj_address = address(obj_eval)
      } else {
        # Do the same we tried in step 1 but now for the evaluated object
#        cat("ENTRO 2b\n")
        obj_name = get_obj_name(obj_eval, n=n)
        if (exists(obj_name, envir=envir, inherits=FALSE)) {
          obj_address = address(eval(as.name(obj_name), envir=envir))
        } else {
          obj_address = NULL
        }
      }
    }
  }

  return(obj_address)
}
