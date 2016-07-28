#' Return the memory address of an object existing in a given environment
#' 
#' @param obj object whose memory address is requested. It can be given as a variable name or as a string.
#' @param envir environment where the object should be searched for. All parent environments of
#' \code{envir} are searched as well.
#' @param n number of levels to go up from the \code{get_obj_address} environment to resolve the name
#' of \code{obj}. It defaults to the calling environment. This parameter is useful when calling
#' \code{get_obj_address} from within an \code{*apply} function where we should use n=2 to get to the
#' proper environment where the object name should be resolved.
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
#' env1$x <- 3; env1$y <- 2;
#' objects <- c("x", "y")
#' sapply(objects, FUN=function(x) { obj = as.name(x); get_obj_address(obj, envir=env1, n=2) }) 
get_obj_address = function(obj, envir=.GlobalEnv, n=1)
# NOTE: (2016/07/26) This function is needed to avoid an error when requesting the address() of an object that
# does not exist. This function returns NULL in such case instead of an error message!
{
	obj_name = get_obj_name(obj, n=n)
	if (exists(obj_name, envir=envir, inherits=TRUE)) {
		obj.address = address(eval(as.name(obj_name), envir=envir))
			## NOTE: note we are using eval(as.name(obj_name)) instead of eval(obj)
			## Although the latter should work because eval() first evaluates obj in the calling environment
			## i.e. this would give 'env9' if obj=env9, and then evaluates the object to which 'obj' resolved to
			## in the environment specified in envir! But, despite this I get the error message that 'env9'
			## is not found when envir is not the Global Environment!
			## We can neither use evalq() because evalq() prevents 'obj' from being evaluated in the
			## current execution environment and in that case the address() function would be returning
			## the memory address of the local variable 'obj', not of the variable referenced by 'obj'!  
			
			## NOTE also that eval() presumably looks for the object recursively in all parent environments
			## if it is not found in the 'envir' environment, so in principle it does the same thing that
			## exists() does when inherits=TRUE, and we should be ok here in that eval() should not give an
			## error if exists() returned TRUE.
	} else {
		obj.address = NULL
	}

  return(obj.address)
}
