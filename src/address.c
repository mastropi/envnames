#include <R.h>
#include <Rdefines.h>

SEXP address(SEXP x);

SEXP address(SEXP x) {
	char address[20];
	snprintf(address, sizeof(address), "<%p>", (void *)x);
	return(ScalarString(mkChar(address)));
}
