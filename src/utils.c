// Copyright (C) 2012 Pierrick Bruneau

#include "utils.h"
		
SEXP getTimestamp() {
	// get current timestamp in format (sec, microsec)
	struct timeval *tv;
	tv = (struct timeval *)malloc(sizeof(struct timeval));
	
	gettimeofday(tv, NULL);
	
	SEXP R_res;
	PROTECT(R_res = allocVector(INTSXP, 2));
	INTEGER(R_res)[0] = tv->tv_sec;
	INTEGER(R_res)[1] = tv->tv_usec;
	
	UNPROTECT(1);
	free(tv);
	return(R_res);
}


SEXP getElapsed(SEXP stamp) {
	// get time elapsed since stamp in parameter
	// return result in double valued seconds
	
	struct timeval *tv;
	tv = (struct timeval *)malloc(sizeof(struct timeval));
	
	gettimeofday(tv, NULL);
	
	tv->tv_sec -= INTEGER(stamp)[0];
	if(tv->tv_usec >= INTEGER(stamp)[1]) {
		tv->tv_usec -= INTEGER(stamp)[1];
	} else {
		tv->tv_usec = 1000000 + tv->tv_usec - INTEGER(stamp)[1];
		tv->tv_sec--;
	}	
	
	double res = tv->tv_sec + tv->tv_usec / 1000000.0;
	SEXP R_res;
	PROTECT(R_res = allocVector(REALSXP, 1));
	REAL(R_res)[0] = res;
	
	UNPROTECT(1);
	free(tv);
	
	return(R_res);
}





