### Description:
##   Parallelize any single-threaded function and assemble output in a list.
##   Currently limited to "embarassingly parallel" computations, essentially
##   the types of things you might run in a for() loop.  This would include
##   simulatation-type applications such as MCMC or bootstrapping.
##
##   This function could be extended to accept lists of different arguments.
##   For example, this would permit run regression models with different
##   sets of covariates.
##
###  Author: David Huh
##
###  Dependencies: parallel, foreach, doMC -or- doSNOW
##
###  Arguments:  fun.name  = a string specifying the name of the function
##               fun.pkg   = if the function you specify is not part of base R,
##                           you must specify the name of the package as a string.
##                n.reps   = number of times the function is run
##                dopkg    = a string specifying the foreach adaptor package to use.
##                           [options: "doMC" (default), "doSNOW"]
##                 ...     = any additional arguments that you would pass to the
##                           original function
##
###  Values:    a list of returned values, one for each run of the function.
##

easyParallel <- function(fun.name, fun.pkg=NULL, n.reps=2, dopkg="doMC", ...) {
  ## validate required arguments
  if (missing(fun.name))
    stop("Missing a function to be used.") 
  
  require(parallel, quietly=TRUE)
  ncores <- detectCores() - 1   # hold one core back for other tasks

  ## Utility function that installs package if not already installed
  pkgTest <- function(pkgname) {
    if (!pkgname %in% installed.packages()) install.packages(pkgname)
  }
  
  if (dopkg == "doMC") {
    pkgTest("doMC")
    require(doMC, quietly=TRUE)
    
    registerDoMC(cores = ncores)
  }

  if (dopkg == "doSNOW") {
    pkgTest("doSNOW")
    require(doSNOW, quietly=TRUE)
    
    registerDoSNOW(makeCluster(ncores, type = "SOCK"))
  }

  pkgTest("foreach")  # verify that the foreach package is installed
  
  resultFunction <- function(n.reps, fun.name, ...) {
    theFUN <- function(fun.name, ...) {
      FUN <- match.fun(fun.name)
      FUN(...)
    }

    foreach (i = 1:n.reps,
             .combine = "list",
             .multicombine = TRUE,
             .packages = fun.pkg) %dopar% {theFUN(fun.name = fun.name, ...)}
  }
  
  resultFunction(n.reps = n.reps, fun = fun.name, ...)
}

