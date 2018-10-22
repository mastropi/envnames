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
# Environment called 'envir' (same name as parameter envir= in function environment_name()) that points to another environment
# (this is used to test an extreme case in T9x test set)
envir <- env_of_envs 
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
# getExecutionEnvironments() returns a set of execution environments (e.g. current execution environment
# and execution environment of function at a given parent generation)
with(env1, 
     getExecutionEnvironments <- function(n) {
#  all_calls = sys.calls()
#  for (c in length(all_calls):1) {
#    l = length(all_calls) - c + 1
#    fun_name = as.character(all_calls[[c]])[1]
#    cat("level:", l, "fun:", fun_name, "memory:", envnames:::address(parent.frame(l)), "\n")
#  }
  return( list(env_current=environment_name(), env_parent=environment_name(parent.frame(n))) )
  }
)
# Function used to test function getExecutionEnvironments() with TOO MUCH information in the calling path
# i.e., this function calls getExecutionEnvironments(), citing the GLOBAL environment in its path
# (this has to be a pre-defined environment such as the GLOBAL or BASE environment) (e.g. globalenv()$env1$f())
with(env_of_envs$env11,
     gMuch <- function(n) { globalenv()$env1$getExecutionEnvironments(n) })
# Function used to test function getExecutionEnvironments() with THE RIGHT information in the calling path
# i.e., this function  calls getExecutionEnvironments() that is defined in the same environment as that function
# specifying the environment that leads to the function, even if it is redundant (e.g. env1$f())
with(env1,
     gRight <- function(n) { env1$getExecutionEnvironments(n) })
# Function used to test function getExecutionEnvironments() with TOO LITTLE information in the calling path
# i.e., this function  calls getExecutionEnvironments() that is defined in the same environment as that function
# allowing us to NOT give the path to the called function (e.g. f())
with(env1,
     gLittle <- function(n) { getExecutionEnvironments(n) })
# Function used to test function getExecutionEnvironments() when called from OUTSIDE
gOutside <- function(n) { env1$getExecutionEnvironments(n) }
# Function that is called from the global environment
showExecutionEnvironments = function() {
  cat("We are in function", environment_name(), "called from environment", environment_name(parent.frame()))
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
  #browser()
  observed = environment_name(baseenv())
  expect_equal(environment_name(baseenv()), expected)
})

test_that("T1) the environment name is correctly returned when the environment variable is given as a symbol (in all environments)", {
  # skip("not now")
  expected = c("env2", "env2", "env2")
  names(expected) = sort(c("env_of_envs", "envir", "R_GlobalEnv"))
  observed = environment_name(env2)
  print(observed)
  expect_equal(observed, expected)
  expect_equal(environment_name(env11, envir=globalenv()$env_of_envs), "env11")
})

test_that("T2) the environment name is correctly returned when environment variable enclosed in quote()", {
  # browser()  # This can be used like a breakpoint for debugging. But stil F10 doesn't go to the next line, it will continue to the end of the program!
  expected = c("env2", "env2", "env2")
  names(expected) = sort(c("env_of_envs", "envir", "R_GlobalEnv"))
  observed = environment_name(quote(env2))
  expect_equal(observed, expected)
  expect_equal(environment_name(quote(env11), envir=globalenv()$env_of_envs), "env11")
  expected = c("env11", "env11")
  names(expected) = c("env_of_envs", "envir")
  expect_equal(environment_name(quote(env11)), expected)

  # But the above using quote() should NOT work for named environments retrieved by a function (e.g. globalenv(), baseenv())
  expect_equal(environment_name(quote(globalenv())), NULL)
  expect_equal(environment_name(quote(.GlobalEnv)), "R_GlobalEnv")
  expect_equal(environment_name(quote(baseenv())), NULL)
  expect_equal(environment_name(quote(emptyenv())), NULL)
})

test_that("T3) the environment name is NULL when the environment does not exist", {
  expect_equal(environment_name(env9), NULL)
})

test_that("T5) the environment name is correctly returned when given as an environment (e.g. <environment: 0x0000000019942d08>)", {
  expected = c("env2", "env2", "env2")
  names(expected) = sort(c("env_of_envs", "envir", "R_GlobalEnv"))
  observed = environment_name(as.environment(globalenv()$env2))
  expect_equal(observed, expected)
  expect_equal(environment_name(as.environment(globalenv()$env_of_envs$env11), envir=globalenv()$env_of_envs),  "env11")
})

test_that("T6) the environment name of an object given as a string which does not exist is NULL", {
  expect_equal(environment_name("doesNotExist"), NULL)
  expect_equal(environment_name("0x000000001806d1b8"), NULL)
})

test_that("T7) the environment name of an object given as a string containing the memory address of an environemnt
          returns the name of all environments with that memory address", {
  skip("This gives 'node stack overflow' error in 32-bit platform... apparently meaning that there is a recursive call that does not end...")
  expected = c("env2", "env2", "env2")
  names(expected) = sort(c("env_of_envs", "envir", "R_GlobalEnv"))
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
#  skip("*** FAILS WHEN RUN UNDER CHECK BUT DOESN'T FAIL WHEN RUN HERE OR WHEN TESTING THE PACKAGE! WHY???
#  when doing Check I don't see what is printed here... so I don't know what went wrong ***
#  OK: Reason is that the order of the environments returned by the environment_name() function
#  is different when running the tests and when running the Check!! I solved this by using sort() below")
  expected = c("env11", "env11")
  names(expected) = c("env_of_envs", "envir")
  observed = environment_name(env11, matchname=FALSE)
  expect_equal(observed, expected)
})

test_that("T12) only the environments having the same name are returned when matchname=TRUE, even if different environments share the SAME memory address", {
  expected = c("env11", "env11")
  names(expected) = c("env_of_envs", "envir")
  observed = environment_name(env11, matchname=TRUE)
  expect_equal(observed, expected)

  # However, when the environment is given with its path, NULL is returned because there is no environment called "env_of_envs$env11"!
  expect_equal(environment_name(env_of_envs$env11, matchname=TRUE), NULL)
})

test_that("T13) ALL the environments having the same name are returned when matchname=TRUE, even if they have DIFFERENT memory addresses", {
  expected = c("env1", "env1", "env1")
  names(expected) = sort(c("env_of_envs", "envir", "R_GlobalEnv"))
  observed = environment_name(env1, matchname=TRUE)
  expect_equal(observed, expected)
})

test_that("T21) the name of an environment defined inside a function is correctly returned", {
  skip("2018/10/10: This test works fine if run OUTSIDE the test_that() call,
       but o.w. it does not work, as we get the error 'invalid 'envir' argument'
       in line 'exists(obj_name, envir=env, inherits=FALSE) in function obj_find() line 259.
       I don't understand why because variable env IS an environment at that line.
       It's a function environment in the testthat namespace as shown by the output '<environment: namespace:testthat>'
       generated by the following lines placed in obj_find() before line 259:
       print(env)
       cat(class(env1))
       ")
  # Test 1
  # The following expected value is to be used when running the test OUTSIDE the test_that() block
  #expected = "f$envfun"
  # The following expected value is to be used when running the test INSIDE the test_that() block
  expected = "f$envfun"
  # Function inside which an environment is defined containing other environments
  f = function() {
    envfun = new.env()
    with(envfun, e <- new.env())
#    print(environment_name(sys.frame(sys.nframe())))
#    browser()
    return( environment_name(envfun, include_functions=TRUE, envir=NULL) )  # envir=sys.frame(sys.nframe())
    ## done-2017/10/15: (2017/09/26) Make this work with envir=NULL. Currently the output of environment_name() in that case is NULL.
  }
  observed = f()
  print(observed)
  expect_equal(observed, expected)
})

test_that("T22) the name of function execution environments are correctly shown
          when the function showing them is defined in the global environment", {
  expect_output(showExecutionEnvironments(), "We are in function showExecutionEnvironments called from environment base\\$eval")  # Now that we need to escape the $ symbol to express 'base$eval'!
  # Use the following test when NOT called from inside test_that()
  #expect_output(showExecutionEnvironments(), "We are in function showExecutionEnvironments called from environment R_GlobalEnv")
})

test_that("T23) the name of function execution environments are correctly returned
          when TOO MUCH information is given about the binding environment of the function when calling it", {
  expect_equal(env_of_envs$env11$gMuch(1), list(env_current="package:envnames$globalenv()$env1$getExecutionEnvironments",
                                                env_parent="env_of_envs$env11$gMuch"))
  
  expect_equal(env_of_envs$env11$gMuch(2), list(env_current="package:envnames$globalenv()$env1$getExecutionEnvironments", env_parent="base$eval"))
  # Use the following test when NOT called from inside test_that()
  #expect_equal(env_of_envs$env11$gMuch(2), list(env_current="package:envnames$globalenv()$env1$getExecutionEnvironments", env_parent="R_GlobalEnv"))
})

test_that("T24) the name of function execution environments are correctly returned
          when THE RIGHT information is given about the binding environment of the function when calling it", {
  expect_equal(env1$gRight(1), list(env_current="env1$getExecutionEnvironments",
                                    env_parent="env1$gRight"))
  expect_equal(env1$gRight(2), list(env_current="env1$getExecutionEnvironments",
                                    env_parent="base$eval"))
  # Use the following test when NOT called from inside test_that()
  #expect_equal(env1$gRight(2), list( env_current="env1$getExecutionEnvironments", env_parent="R_GlobalEnv"))
})

test_that("T25) the name of function execution environments are correctly returned
          when TOO LITTLE information is given about the binding environment of the function when calling it", {
  # NOTE that the environment shown in the returned list is 'e' and NOT 'env1' even though the getExecutionEnvironments()
  # is defined in env1. This happens because the e environment points to same env1 environment and
  # the get_env_names() generates a lookup table where the location of an environment ("location" column)
  # is the environment that has the first match with the memory address providing the location information.
  expect_equal(env1$gLittle(1), list(env_current="e$getExecutionEnvironments",
                                     env_parent="env1$gLittle"))
  expect_equal(env1$gLittle(2), list(env_current="e$getExecutionEnvironments",
                                     env_parent="base$eval"))
  # Use the following test when NOT called from inside test_that()
  #expect_equal(env1$gLittle(2), list(env_current="e$getExecutionEnvironments", env_parent="R_GlobalEnv"))
})


#------------------------------------- Extreme cases ------------------------------------------
test_that("T90) invalid 'env' variable returns NULL", {
  expect_equal(environment_name(NULL), NULL)
  expect_equal(environment_name("not an environment nor an address"), NULL)
})

test_that("T91) when an environment is called 'envir' (like the envir= parameter in environment_name()!) the name is correctly returned as 'envir',
                especially when 'envir' points to another environment in the workspace! (e.g. when envir = env_of_envs", {
  expect_equal(environment_name(envir, matchname=TRUE), "envir")

  # Use the following when running this test through the Test Package utility
  expected = c("env_of_envs", "envir")
  names(expected) = c("R_GlobalEnv", "R_GlobalEnv")
  expect_equal(environment_name(envir), expected)

  # WARNING: Use the following when running the test by SOURCEing this file
  #expected = c("env_of_envs", "envir", "R_GlobalEnv")
  #names(expected) = c("R_GlobalEnv", "R_GlobalEnv", NA)
  #expect_equal(environment_name(envir), expected)

  # WARNING: Use the following when running the test by just running this test_that() block
  #expected = c("env_of_envs", "envir")
  #names(expected) = c("R_GlobalEnv", "R_GlobalEnv")
  #expect_equal(environment_name(envir), expected)
})
#------------------------------------- Extreme cases ------------------------------------------


# 4.- Cleanup -------------------------------------------------------------
rm(list=ls())

})  # with(globalenv())

