## Load utility function

#install.packages("devtools")
library(devtools)
source_url("https://raw.github.com/davidhuh/easyParallel/master/easyParallel.R")

### Example:

## Without parallelization

not.parallel <- rnorm(n = 10, mean = 5, sd = 1)
not.parallel

## With parallelization:
##    Note that for a simple function like rnorm(), the overhead of setting up
## parallelization is less efficient than running the same function sequentially.
## However, for long-running computations, the parallelized runs will finish sooner.

parallel.run <- easyParallel(fun.name="rnorm", fun.pkg=NULL, n.reps=2, dopkg="doSNOW",
                             n = 10, mean = 5, sd = 1)

parallel.run