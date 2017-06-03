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
       fun_calling = get_fun_calling()

       # Show calling environment without using envnames package and using it
       cat("Now inside function", get_fun_name(), "\n")
       cat("Environment name of calling function (using environmentName() function):  \"", environmentName(parent.frame()), "\"\n", sep="")
       cat("Environment name of calling function as returned by get_fun_calling(): ", fun_calling, "\n", sep="")
       ## NOTE: It's important to store the output of get_fun_calling() in a variable and then show that variable, instead of directly
       ## calling get_fun_calling() from within the cat() function above, because cat() is a function and therefore its result
       ## will NOT be the same as when we call get_fun_calling() from outside cat()!!
       ## An alternative would be to increase the parameter of get_fun_calling() by 1; in this case use get_fun_calling(2)

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
  expected = data.frame(fun=c("h()", "g()", "f()"), env=c("R_GlobalEnv", "env2", "env1"), envfun=c("R_GlobalEnv$h()", "env2$g()", "env1$f()"), stringsAsFactors=FALSE)
  rownames(expected) = 1:nrow(expected) - 1
  observed = with(globalenv(), env1$f())   # NOTE: We could also use globalenv()$env1$f() (although initially it did not work)
  expect_equal(observed[1:3,], expected)
})

test_that("T2) the function calling chain when the calling function is defined in an environment nested within
          a *package* environment is correctly created (here we use an environment env_test whish should be 
          temporarily defined in the envnames package)", {
  skip("still to complete --need to define test environment env_test in the envnames package --temporarily because we don't want to include it in the package distribution... I guess")
  expected = data.frame(fun=character(0), env=character(0), envfun=character(0), stringsAsFactors=FALSE)
  rownames(expected) = 1:nrow(expected) - 1
  observed = with(envnames:::env_test, f())
  expect_equal(observed, expected)
})


# 3.- Cleanup -------------------------------------------------------------
rm(list=ls())

})  # with(globalenv())
