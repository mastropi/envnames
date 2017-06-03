# Created:      22-Jan-2017
# Author:       Daniel Mastropietro
# Description:  Test the envnames package using the testthat package
#

library(testthat)
library(envnames)
context("Function calling")

# NOTE THE NEED TO RUN WITH with(globalenv()) BECAUSE THE OBJECTS ARE DEFINED IN THE GLOBAL ENVIRONMENT
# AND NOT IN THE ENVIRONMENT DEFINED BY testthat.
with(globalenv(), {

# 1.- Prepare the workspace -----------------------------------------------
# IMPORTANT: MAKE SURE THAT THE GLOBAL ENVIRONMENT IS CLEAN, WHICH MAY NOT BE THE CASE WHEN RUNNING
# THE TESTS THROUGH testthat (e.g. using the Check tool in RStudio or calling devtools::test())
rm(list=ls())

env1 <- new.env()
env2 <- new.env()

### Build a chain of function calls ('->' means "calls"): env1$f -> env2$g -> h
# Second level calling function
h <- function(level=1, showParameters=TRUE) {
  return(get_fun_calling(level, showParameters))
}

# First level calling function
with(env2,
     g <- function(level=1, showParameters=TRUE) {
       if (level == 1) {
         # Get the information from the calling function from this function
         fun_calling = get_fun_calling(level, showParameters=showParameters)
       } else {
         # Call another function and get the information on the calling function from inside that function
         fun_calling = h(level=level, showParameters=showParameters)
       }

       return(fun_calling)
     }
)

# Root calling function whose name should be printed when g() is run
with(env1,
     f <- function(level, showParameters=TRUE) {
       fun_calling = env2$g(level, showParameters=showParameters)
       #chain = get_fun_calling_chain()
       #print(chain)
       return(fun_calling)
     }
)


# 2.- TEST! ---------------------------------------------------------------
test_that("T0) the calling function at level 0 returns the current function we are in", {
  # skip("not now")
  # browser()  # This can be used like a breakpoint for debugging. But stil F10 doesn't go to the next line, it will continue to the end of the program!
  expected = "R_GlobalEnv$h(level = level, showParameters = showParameters)"
  observed = with(globalenv(), env1$f(0))
  expect_equal(observed, expected)
})

test_that("T1) the calling function at the first level is correctly computed", {
  # skip("not now")
  # browser()  # This can be used like a breakpoint for debugging. But stil F10 doesn't go to the next line, it will continue to the end of the program!
  expected = "env1$f(1)"
  observed = with(globalenv(), env1$f(1))
  expect_equal(observed, expected)
})

test_that("T2) the calling function at level 2 returns the current function we are in", {
  # skip("not now")
  # browser()  # This can be used like a breakpoint for debugging. But stil F10 doesn't go to the next line, it will continue to the end of the program!
  expected = "env1$f(2)"
  observed = with(globalenv(), env1$f(2))
  expect_equal(observed, expected)
})

test_that("T3) the calling function name is returned and no parameters are shown", {
  # skip("not now")
  # browser()  # This can be used like a breakpoint for debugging. But stil F10 doesn't go to the next line, it will continue to the end of the program!
  expected = "env1$f"
  observed = with(globalenv(), env1$f(2, showParameters=FALSE))
  expect_equal(observed, expected)
})

# 3.- Cleanup -------------------------------------------------------------
rm(list=ls())

})  # with(globalenv())
