#' Return the name of the calling function
#'
#' This is a wrapper for \code{get_fun_calling_chain(1)} which returns the name of the function calling
#' the function from which \code{get_fun_calling} is called.
get_fun_calling <- function() {
	return(get_fun_calling_chain(2))
}
