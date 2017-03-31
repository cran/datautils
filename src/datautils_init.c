#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP getElapsed(SEXP);
extern SEXP getTimestamp();

static const R_CallMethodDef CallEntries[] = {
    {"getElapsed",   (DL_FUNC) &getElapsed,   1},
    {"getTimestamp", (DL_FUNC) &getTimestamp, 0},
    {NULL, NULL, 0}
};

void R_init_datautils(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
