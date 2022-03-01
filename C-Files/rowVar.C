  #include <R.h>
  #include <Rinternals.h>
  #include <math.h>

  SEXP rowV(SEXP y, SEXP n, SEXP r){
    int *nc = INTEGER(n);
    double *x = REAL(y);
    int d = length(y);
    int *nr = INTEGER(r);
    int i, j, z;

  double xSq[(d)];
    SEXP result;
    PROTECT(result = allocVector(REALSXP, (*nr)));
    memset(REAL(result), 0, (*nr) * sizeof(double));
    double *rowVar = REAL(result);
    int fr = ((*nc) - 1);

    for(i = 0; i < (*nr); i++){
        double rowMean = 0; double xSm = 0;
        double rowMsq = 0;
        for(j = 0; j < (*nc); j++){
            rowMean += ((x[(i + ((*nr) * j)) ]) / (*nc));
            xSm += (xSq[(i + (*nr * j))]);
        }
        rowMsq = (*nc) * (pow(rowMean, 2));
        rowVar[i] = ((xSm - rowMsq) / fr);
    }
    UNPROTECT(1);
    return(result);
  }