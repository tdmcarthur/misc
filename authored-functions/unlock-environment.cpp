#include <Rcpp.h>
using namespace Rcpp;
/* Thanks to http://stackoverflow.com/questions/25910778/unlockenvironment-implemented-via-rcpp-instead-of-inline/25922051#25922051
*/
/* This is taken from envir.c in the R 2.15.1 source
https://github.com/SurajGupta/r-source/blob/master/src/main/envir.c
*/

/* 

Example usage in R:
library("Rcpp")
Rcpp::sourceCpp("/Users/travismcarthur/git/misc/authored-functions/unlock-environment.cpp")

env <- new.env()
lockEnvironment(env)
try(env$a <- 1) ## error
unlock_environment(env)
try(env$a <- 1)

# OR
library(plyr)
unlock_environment(as.environment("package:plyr"))

*/

#define FRAME_LOCK_MASK (1<<14)
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))

// [[Rcpp::export]]
bool unlock_environment(Environment env) {
  UNLOCK_FRAME(env);
  return FRAME_IS_LOCKED(env) == 0;
}