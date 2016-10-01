# Created:      13-Aug-2016
# Author:       Daniel Mastropietro
# Description:  Test the envnames package using the testthat package
#

library(testthat)
library(envnames)
context("Function calling chain")


# 1.- Prepare the workspace -----------------------------------------------
with(globalenv(), {
  env1 <- new.env()
  env2 <- new.env()

  # Build a chain of function calls: f calls g, g calls h
  h <- function(y) {
    return(get_fun_calling_chain())
  }
  
  with(env2,
       g <- function(x) {
         fun_calling_chain = get_fun_calling_chain()
         
         # Show calling environment without using envnames package and using it
         cat("Now inside function", get_fun_name(), "\n")
         cat("Environment name of calling function (using environmentName() function):  \"", environmentName(parent.frame()), "\"\n", sep="")
         cat("Environment name of calling function as returned by get_fun_calling_chain(1): ", fun_calling_chain[,"envfun"], "\n", sep="")
  
         fun_calling_chain = h(3)
         return(fun_calling_chain)
       }
  )

  # Calling function whose name should be printed when g() is run
  with(env1,
       f <- function(x) {
         # Setup for environment tracking
         #       env_address = setup_env(); on.exit(close_env(env_address))
         
         # Start
         fun_calling_chain = env2$g(x)
         return(fun_calling_chain)
       }
  )
})


# 2.- TEST! ---------------------------------------------------------------
test_that("T1) the function calling chain with several functions in the chain is correctly created", {
  # skip("not now")
  # browser()  # This can be used like a breakpoint for debugging. But stil F10 doesn't go to the next line, it will continue to the end of the program!
  expected = data.frame(level=as.character(c(0, 1, 2)), fun=c("h", "g", "f"), env=c("R_GlobalEnv", "env2", "env1"), envfun=c("R_GlobalEnv$h", "env2$g", "env1$f"), stringsAsFactors=FALSE)
    ## NOTE: Don't know why 'level' should be defined as character if it is defined as numeric in get_fun_calling_chain() function...
  observed = with(globalenv(), env1$f(1))   # NOTE: Using globalenv()$env1$f(1) does NOT work!
#  observed = env1$f(1)   # THIS DOES NOT WORK! (I get the error message that "argument is of length 0")
  expect_equal(observed, expected)
    ## Note: use as.vector(addresses) because o.w. the test fails because the names of the array elements do not match, but not the values
})


# 3.- Cleanup -------------------------------------------------------------
with(globalenv(), rm(list=c("env1", "env2", "h")))
