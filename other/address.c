// Definition of C function that could be used to print the memory address of an object in R.
// In the end, I didn't use this method, but rather I defined an SEXP address(SEXP) that returns the address.
// See src/address.c

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Print.h>

//extern "C" {
//  static void address(SEXP x);
//}

SEXP address(SEXP x) {
	Rprintf("<%p>", (void *)x);
}
