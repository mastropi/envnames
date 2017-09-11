#' Call the C function address() that retrieves the memory address of an R object
#'
#' @param x object whose memory address is of interest.
#' 
#' @return the memory address of object \code{x} or \code{NULL} if the object does not exist in the R workspace.
address <- function(x) {
  .Call("address", x, PACKAGE="envnames")
}
