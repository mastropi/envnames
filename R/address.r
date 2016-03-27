# Call C function address() that retrieves the memory address of an R object
address <- function(x) {
  .Call("address", x, PACKAGE="envnames")
}
