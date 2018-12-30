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
test_that("T0) the function returns NULL when called from outside a function", {
  skip("this cannot be tested because the call is inside function test_that()!")
  expected = NULL
  observed = get_fun_calling_chain()
  expect_equal(observed, expected)
})

test_that("T1) the function calling chain with several functions in the chain is correctly created", {
  # browser()
  expected = data.frame(fun=c("h", "g", "f"), env=c("R_GlobalEnv", "env2", "env1"), envfun=c("R_GlobalEnv$h", "env2$g", "env1$f"), stringsAsFactors=FALSE)
  rownames(expected) = 1:nrow(expected) - 1
  observed = with(globalenv(), env1$f())   # NOTE: We could also use globalenv()$env1$f() (although initially it did not work)
  expect_equal(observed[1:3,], expected)
})

test_that("T2) the function calling chain when the calling function is defined in an environment nested within
          a *package* environment is correctly created (here we use an environment that should already be 
          defined in the envnames package)", {
  # Prepare the environment by creating a function pointing to the env1$f() function defined above
  testenv$f <- env1$f

  # 1) Calling testenv$f()
  # Note that we compare just the FIRST 3 rows because the FULL calling chain depends on how we run this test
  # (e.g. whether by running the whole test_that() function, or by running just the lines and in addition
  # that calling chain may be quite long)
  expected = data.frame(fun=c("h", "g", "f"),
                        env=c("R_GlobalEnv", "env2", "testenv"),
                        envfun=c("R_GlobalEnv$h", "env2$g", "testenv$f"),
                        stringsAsFactors=FALSE)
  rownames(expected) = 1:nrow(expected) - 1
  observed = testenv$f()[1:3,]
  expect_equal(observed, expected)

  # 2) Calling testenv$f() using with()
  # Note that we compare just the FIRST 7 rows because the FULL calling chain depends on how we run this test
  # (e.g. whether by running the whole test_that() function, or by running just the lines and in addition
  # that calling chain may be quite long)
  expected = data.frame(fun=c("h", "g", "f",
                              "eval", "eval",
                              "with.default", "with"),
                        env=c("R_GlobalEnv", "env2", "",
                              "", "base",
                              "base", "base"),
                        envfun=c("R_GlobalEnv$h", "env2$g", "f",
                                 "eval", "base$eval",
                                 "base$with.default", "base$with"),
                        stringsAsFactors=FALSE)
  rownames(expected) = 1:nrow(expected) - 1
  observed = with(envnames:::testenv, f())[1:7,]
  expect_equal(observed, expected)

  # Clean-up
  rm("f", envir=testenv)
})


# 3.- Cleanup -------------------------------------------------------------
rm(list=ls())

})  # with(globalenv())
