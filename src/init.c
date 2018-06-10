#include <stdlib.h> 		// for NULL
#include <Rinternals.h>		// for SEXP
#include <R_ext/Rdynload.h>
#include "envnames.h"

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

/* .C calls */
extern SEXP address(SEXP x);

static const R_CallMethodDef R_CallDef[] = {
   CALLDEF(address, 1),
   {NULL, NULL, 0}
};

void R_init_envnames(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
