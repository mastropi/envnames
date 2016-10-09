# Created:      13-Aug-2016
# Author:       Daniel Mastropietro
# Description:  Test the envnames package using the testthat package
#

library(testthat)
library(envnames)
context("Function calling chain")

# NOTE THE NEED TO RUN WITH with(globalenv()) BECAUSE THE OBJECTS ARE DEFINED IN THE GLOBAL ENVIRONMENT
# AND NOT IN THE ENVIRONMENT DEFINED BY testthat.
with(globalenv(), {

# 1.- Prepare the workspace -----------------------------------------------
# IMPORTANT: MAKE SURE THAT THE GLOBAL ENVIRONMENT IS CLEAN, WHICH MAY NOT BE THE CASE WHEN RUNNING
# THE TESTS THROUGH testthat (e.g. using the Check tool in RStudio or calling devtools::test())
rm(list=ls())

env1 <- new.env()
env2 <- new.env()

# Build a chain of function calls ('->' means "calls"): env1$f -> env2$g -> h
h <- function() {
  return(get_fun_calling_chain())
}

with(env2,
     g <- function() {
       fun_calling_chain = get_fun_calling_chain()
       
       # Show calling environment without using envnames package and using it
       cat("Now inside function", get_fun_name(), "\n")
       cat("Environment name of calling function (using environmentName() function):  \"", environmentName(parent.frame()), "\"\n", sep="")
       cat("Environment name of calling function as returned by get_fun_calling(): ", get_fun_calling(), "\n", sep="")
       
       fun_calling_chain = h()
       return(fun_calling_chain)
     }
)

# Calling function whose name should be printed when g() is run
with(env1,
     f <- function() {
       # Start
       fun_calling_chain = env2$g()
       return(fun_calling_chain)
     }
)


# 2.- TEST! ---------------------------------------------------------------
test_that("T1) the function calling chain with several functions in the chain is correctly created", {
  # skip("not now")
  # browser()  # This can be used like a breakpoint for debugging. But stil F10 doesn't go to the next line, it will continue to the end of the program!
  expected = data.frame(level=as.character(c(2, 1, 0)), fun=c("f", "g", "h"), env=c("env1", "env2", "R_GlobalEnv"), envfun=c("env1$f", "env2$g", "R_GlobalEnv$h"), stringsAsFactors=FALSE)
  observed = with(globalenv(), env1$f())   # NOTE: We could also use globalenv()$env1$f() (although initially it did not work)
  expect_equal(observed, expected)
})

test_that("T2) the function calling chain when the calling function is defined in an environment nested within
          a *package* environment is correctly created", {
  skip("still to complete")
  expected = data.frame(level=character(0), fun=character(0), env=character(0), envfun=character(0), stringsAsFactors=FALSE)
  observed = with(envnames:::env_test, f())
  expect_equal(observed, expected)
})


# 3.- Cleanup -------------------------------------------------------------
rm(list=ls())

})  # with(globalenv())
