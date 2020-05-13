#include <stdbool.h>         // for 'true' and 'false'
#include <R.h>
#include <R_ext/Rdynload.h>  // for registering routines
#include <Rdefines.h>        // R macros and functions
#include <Rinternals.h>

SEXP bitwiseParity (SEXP R_x, SEXP R_even)
{
  /*
  Function to compute bitwise parity for a set of values
  INPUT
  R_x      An integer, logical or raw vector, or a list of such vectors
  R_even   A logical value specifying whether the parity should be even or odd

  OUTPUT
  - If R_x is a list of equally sized integer, logical, or raw vectors:
    A vector of the same class and length as the first element of R_x,
    containing the even or odd bitwise parity of the elements of R_x.
  - If R_x is an integer, logical, or raw vector:
    A vector of the same class as R_x, of length 1,
    containing the even or odd bitwise parity of the elements of R_x.
  - If R_x is a list or vector that does not fit the above criteria:
    A NULL value.
  */

  SEXP R_result;
  unsigned char *parity, *x;
  int i, j, k, m, n, nchars, idx, type, valid;

  m = LENGTH(R_x);

  if (m == 0) {
    // If input is zero-length, return zero-length vector of same type
    PROTECT(R_result = allocVector(TYPEOF(R_x), m));
  } else {
    valid = false;
    if (TYPEOF(R_x) == VECSXP) {
      // If input is list, check that all elements are integer, logical, or raw,
      // and that all elements match on type and length
      type = TYPEOF(VECTOR_ELT(R_x, 0));
      n = LENGTH(VECTOR_ELT(R_x, 0));
      if (type == INTSXP || type == LGLSXP || type == RAWSXP) {
        for (i = 0; i < m; i++) {
          valid = (TYPEOF(VECTOR_ELT(R_x, i)) == type) && 
                  (LENGTH(VECTOR_ELT(R_x, i)) == n);
          if (!valid) break;
        }
      }
      if (valid) {
        // Allocate output vector of same type and length as elements of input
        // and set an unsigned char pointer to it
        PROTECT(R_result = allocVector(type, n));
        if (type == INTSXP || type == LGLSXP) {
          parity = (unsigned char *) INTEGER(R_result);
          nchars = sizeof(int);
        } else {
          parity = RAW(R_result);
          nchars = sizeof(char);
        }
        // Initialize output vector
        for (j = 0; j < n; j++) {
          for (k = 0; k < nchars; k++) {
            idx = j*nchars+k;
            parity[idx] = 0;
          }
        }
        // Iterate over each vector in the input list
        for (i = 0; i < m; i++) {
          // Set an unsigned char pointer to the current vector of input list
          if (type == INTSXP || type == LGLSXP) {
            x = (unsigned char *) INTEGER(VECTOR_ELT(R_x, i));
          } else {
            x = RAW(VECTOR_ELT(R_x, i));
          }
          // Compute bitwise parity with current vector,
          // iterating j over each element of the vector
          // and k over each byte of the element
          for (j = 0; j < n; j++) {
            for (k = 0; k < nchars; k++) {
              idx = j*nchars+k;
              parity[idx] = parity[idx] ^ x[idx];
            }
          }
        }
      }
    } else {
      // If input is a vector, check that it is integer, logical, or raw,
      // and set unsigned char pointers to input and output vectors
      type = TYPEOF(R_x);
      n = 1;
      if (type == INTSXP || type == LGLSXP) {
        PROTECT(R_result = allocVector(type, n));
        x = (unsigned char *) INTEGER(R_x);
        parity = (unsigned char *) INTEGER(R_result);
        nchars = sizeof(int);
        valid = true;
      } else {
        PROTECT(R_result = allocVector(type, n));
        x = RAW(R_x);
        parity = RAW(R_result);
        nchars = sizeof(char);
        valid = true;
      }
      if (valid) {
        // Initialize output vector
        for (i = 0; i < m; i++) {
          for (j = 0; j < n; j++) {
            for (k = 0; k < nchars; k++) {
              idx = i*n+j*nchars+k;
              parity[idx] = 0;
            }
          }
        }
        // Compute bitwise parity, iterating i over each element of the input,
        // leaving j = 0, and k over each byte of the element
        for (i = 0; i < m; i++) {
          for (j = 0; j < n; j++) {
            for (k = 0; k < nchars; k++) {
              idx = i*n+j*nchars+k;
              parity[idx] = parity[idx] ^ x[idx];
            }
          }
        }
      }
    }
    if (valid) {
      // Flip bits of output vector if requested
      if (!INTEGER_VALUE(R_even)) {
        for (j = 0; j < n; j++) {
          for (k = 0; k < nchars; k++) {
            idx = j*nchars+k;
            parity[idx] = ~parity[idx];
          }
        }
      }
    } else {
      // If invalid input was provided, return NULL
      PROTECT(R_result = R_NilValue);
    }
  }

  UNPROTECT(1);
  return R_result;
}

static const R_CallMethodDef callMethods[] = {
  {"bitwiseParity", (DL_FUNC) &bitwiseParity,  2},
  {NULL, NULL, 0}
};

void R_init_GeneHive (DllInfo *dll)
{
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
