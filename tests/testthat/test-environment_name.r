# Created:      Aug-2015
# Author:       Daniel Mastropietro
# Description:  Test the envnames package using the testthat package
#

library(testthat)
library(envnames)
context("Environment names")

# NOTE THE NEED TO RUN WITH with(globalenv()) BECAUSE THE OBJECTS ARE DEFINED IN THE GLOBAL ENVIRONMENT
# AND NOT IN THE ENVIRONMENT DEFINED BY testthat.
with(globalenv(), {

# 1.- Prepare the workspace -----------------------------------------------
# IMPORTANT: MAKE SURE THAT THE GLOBAL ENVIRONMENT IS CLEAN, WHICH MAY NOT BE THE CASE WHEN RUNNING
# THE TESTS THROUGH testthat (e.g. using the Check tool in RStudio or calling devtools::test())
rm(list=ls())

# Create new environments
# Environments in .GlobalEnv: Note the need to specifically require the environment to be created in .GlobalEnv
# either by calling with() or assign() as shown below.
# In fact when running this program through devtools::test() or through CTRL+SHIF+T in RStudio, anything
# defined in this code is NOT part of the global environment, but part of the function where this code
# gets inserted to or run from!
# This can be seen from the lines at the end of devtools::test() which call test code in the test_dir
# directory:
# ns_env <- load_all(pkg, quiet = TRUE)$env
# env <- new.env(parent = ns_env)
# with_envvar(r_env_vars(), testthat::test_dir(test_path, filter = filter, env = env, ...))
# Note that function with_envvar() was originally part of the devtools package but is now part of
# the withr package, which is "A set of functions to run code 'with' safely and temporarily
# modified global state." (Ref: https://cran.r-project.org/web/packages/withr/withr.pdf)
env1 <- new.env()
env2 <- new.env()
assign("env3", new.env(), envir=globalenv())
assign("env_of_envs", new.env(), envir=globalenv())    # User-defined environment that will contain other environments
# Environment inside a user-defined environment
with(env_of_envs, env11 <- new.env()) # Environment defined inside environment \code{env_of_envs}
# Environment that is a copy of another one
e <- env1
# Environment inside env_of_envs that is a copy of another one with the same name sitting in globalenv()
assign("env2", globalenv()$env2, envir=env_of_envs)
# Environment with the same name as another one in a different environment
with(env_of_envs, env1 <- new.env())

# Show the environments involved (current environment and parent environments of the variables defined above)
cat("\nEnvironments involved:\n")
cat("Current environment: "); print(sys.frame(sys.nframe()))
cat("Parent environment of env1: "); print(parent.env(env1))
cat("Parent environment of env2: "); print(parent.env(env2))
cat("Parent environment of env11: "); print(parent.env(env_of_envs$env11))

# Functions for testing function execution environments
h = function(n) {
#  all_calls = sys.calls()
#  for (c in length(all_calls):1) {
#    l = length(all_calls) - c + 1
#    fun_name = as.character(all_calls[[c]])[1]
#    cat("level:", l, "fun:", fun_name, "memory:", envnames:::address(parent.frame(l)), "\n")
#  }
  return(environment_name(parent.frame(n)))
}
# Function that calls h()
with(env_of_envs$env11, g <- function(n) { h(n) })
# Function that is called from the global environment
g = function() { 
  return(list=(environment_name(environment())), environment_name(parent.frame()))
}

# 2.- Address-name pairs lookup table -------------------------------------
# Get the table containing the address-name pairs of existing environments
#debugonce(get_env_names)
#trace(get_env_names, tracer=quote(cat(sprintf("tracing get_env_names(*, env=)\n", env))))
get_env_names()
#untrace(get_env_names)
get_env_names(envir=env_of_envs)


# 3.- TEST! ---------------------------------------------------------------
test_that("T0) the environment name of a named environment (e.g system or package environment) is correctly returned", {
  # skip("not now")
  # browser()  # This can be used like a breakpoint for debugging. But stil F10 doesn't go to the next line, it will continue to the end of the program!
  expected = "base"
  expect_equal(environment_name(baseenv()), expected)
})

test_that("T1) the environment name is correctly returned when the environment variable is given as a symbol (in all environments)", {
  # skip("not now")
  # browser()  # This can be used like a breakpoint for debugging. But stil F10 doesn't go to the next line, it will continue to the end of the program!
#  expected = c("env_of_envs$env2", "env2")
  expected = c("env2", "env2")
  names(expected) = c("R_GlobalEnv", "R_GlobalEnv$env_of_envs")
  observed = environment_name(env2)
  expect_equal(observed, expected)
  expect_equal(environment_name(env11, envir=globalenv()$env_of_envs), "env11")
})

test_that("T2) the environment name is correctly returned when environment variable enclosed in quote()", {
  # skip("this test should NOT pass: in fact environmentName(quote(globalenv())) returns the empty string")
  # browser()  # This can be used like a breakpoint for debugging. But stil F10 doesn't go to the next line, it will continue to the end of the program!
  expected = c("env2", "env2")
  names(expected) = c("R_GlobalEnv", "R_GlobalEnv$env_of_envs")
  observed = environment_name(quote(env2))
  expect_equal(observed, expected)
  expect_equal(environment_name(quote(env11), envir=globalenv()$env_of_envs), "env11")
  expected = "env11"
  names(expected) = "R_GlobalEnv$env_of_envs"
  expect_equal(environment_name(quote(env11)), expected)
})

test_that("T3) the environment name is NULL when the environment does not exist", {
  expect_equal(environment_name(env9), NULL)
})

test_that("T5) the environment name is correctly returned when given as an environment (e.g. <environment: 0x0000000019942d08>)", {
  expected = c("env2", "env2")
  names(expected) = c("R_GlobalEnv", "R_GlobalEnv$env_of_envs")
  observed = environment_name(as.environment(globalenv()$env2))
  expect_equal(observed, expected)
  expect_equal(environment_name(as.environment(globalenv()$env_of_envs$env11), envir=globalenv()$env_of_envs),  "env11")
})

test_that("T6) the environment name of an object given as a string which does not exist is NULL", {
  expect_equal(environment_name("doesNotExist"), NULL)
  expect_equal(environment_name("0x000000001806d1b8"), NULL)
})

test_that("T7) the environment name of an object given as a string containing the memory address of an environemnt returns the name of all environments with that memory address", {
  expected = c("env2", "env2")
  names(expected) = c("R_GlobalEnv", "R_GlobalEnv$env_of_envs")
  addr = get_obj_address(globalenv()$env2)
  print(addr)
  observed = environment_name(get_obj_address(globalenv()$env2))
  expect_equal(observed, expected)
  expect_equal(environment_name(get_obj_address(globalenv()$env_of_envs$env11), envir=globalenv()$env_of_envs), "env11")
})

test_that("T10) standardized environment names (globalenv and baseenv)", {
  expect_equal(environment_name(globalenv()), "R_GlobalEnv")
  expect_equal(environment_name(baseenv()), "base")
})

test_that("T11) all environments matching the same memory address are returned when matchname=FALSE", {
#  skip("*** FAILS WHEN RUN UNDER CHECK BUT DOESN'T FAIL WHEN RUN HERE OR WHEN TESTING THE PACKAGE! WHY??? when doing Check I don't see what is printed here... so I don't know what went wrong ***
#  OK: Reason is that the order of the environments returned by the environment_name() function is different when running the tests and when running the Check!! I solved this by using sort() below")
  expected = c("e", "env1", "env1")
  names(expected) = c("R_GlobalEnv", "R_GlobalEnv", "R_GlobalEnv$env_of_envs")
  observed = environment_name(env1, matchname=FALSE)
  expect_equal(observed, expected)
})

test_that("T12) only the environments having the same name are returned when matchname=TRUE, even if they share the SAME memory address", {
#  skip("*** FAILS WHEN RUN UNDER CHECK BUT DOESN'T FAIL WHEN RUN HERE OR WHEN TESTING THE PACKAGE! WHY??? when doing Check I don't see what is printed here... so I don't know what went wrong ***")
  expected = c("env2", "env2")
  names(expected) = c("R_GlobalEnv", "R_GlobalEnv$env_of_envs")
  observed = environment_name(env2, matchname=TRUE)
  expect_equal(observed, expected)
})

test_that("T13) only the environments having the same name are returned when matchname=TRUE, even if they have DIFFERENT memory addresses", {
  #  skip("*** FAILS WHEN RUN UNDER CHECK BUT DOESN'T FAIL WHEN RUN HERE OR WHEN TESTING THE PACKAGE! WHY??? when doing Check I don't see what is printed here... so I don't know what went wrong ***")
  expected = c("env1", "env1")
  names(expected) = c("R_GlobalEnv", "R_GlobalEnv$env_of_envs")
  observed = environment_name(env1, matchname=TRUE)
  expect_equal(observed, expected)
})

test_that("T21) the name of an environment defined inside a function is correctly returned", {
  # test 1
  expected = "envfun"
  # Function inside which an environment is defined containing other environments
  f = function() {
    envfun = new.env()
    with(envfun, e <- new.env())
    return( environment_name(envfun, envir=sys.frame(sys.nframe())) )
    ## NOTE that calling environment_name() without specifying envir returns NULL
    ## (2017/03/20) This will be changed soon so that in that case the environment name is returned as "R_GloabalEnv$f$envfun"
  }
  observed = f()
  expect_equal(observed, expected)
})

test_that("T22) the name of function execution environments are correctly returned", {
  skip("2017/03/21: Still need to update with correct results --e.g. the names of the list returned perhaps...")
  expect_equal(env_of_envs$env11$g(1), "g")
  expect_equal(env_of_envs$env11$g(2), "R_GlobalEnv")
  expect_equal(g(), list("g","R_GlobalEnv"))
})

#------------------ Extreme cases
test_that("T90) invalid 'env' variable returns NULL", {
  expect_equal(environment_name(NULL), NULL)
  expect_equal(environment_name("not an environment nor an address"), NULL)
})


# 4.- Cleanup -------------------------------------------------------------
rm(list=ls())

})  # with(globalenv())

