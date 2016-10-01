# Created:      Aug-2015
# Author:       Daniel Mastropietro
# Description:  Test the envnames package using the testthat package
#

library(testthat)
library(envnames)
context("Environment names")

# 1.- Prepare the workspace -----------------------------------------------
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
with(globalenv(), env1 <- new.env())
with(globalenv(), env2 <- new.env())
assign("env3", new.env(), envir=globalenv())
assign("env_of_envs", new.env(), envir=globalenv())    # User-defined environment that will contain other environments
# Environment inside a user-defined environment
with(env_of_envs, env11 <- new.env()) # Environment defined inside environment \code{env_of_envs}

# Show the environments involved (current environment and parent environments of the variables defined above)
cat("\nEnvironments involved:\n")
cat("Current environment: "); print(sys.frame(sys.nframe()))
cat("Parent environment of env1: "); print(parent.env(env1))
cat("Parent environment of env2: "); print(parent.env(env2))
cat("Parent environment of env11: "); print(parent.env(env_of_envs$env11))


# 2.- Address-name pairs lookup table -------------------------------------
# Get the table containing the address-name pairs of existing environments
#debugonce(get_env_names)
#trace(get_env_names, tracer=quote(cat(sprintf("tracing get_env_names(*, env=)\n", env))))
get_env_names()
#untrace(get_env_names)
get_env_names(envir=env_of_envs)


# 3.- TEST! ---------------------------------------------------------------
# Note the use of quote() to enclose the environment variable
test_that("T1) the environment name is correctly returned when the environment variable is given as a variable (in all environments)", {
  # skip("not now")
  # browser()  # This can be used like a breakpoint for debugging. But stil F10 doesn't go to the next line, it will continue to the end of the program!
  expect_equal(environment_name(env1), "env1")
  expect_equal(environment_name(env11, envir=globalenv()$env_of_envs), "env11")
})

test_that("T2) the environment name is correctly returned when environment variable enclosed in quote()", {
  # skip("not now")
  # browser()  # This can be used like a breakpoint for debugging. But stil F10 doesn't go to the next line, it will continue to the end of the program!
  expect_equal(environment_name(quote(env1)), "env1")
  expect_equal(environment_name(quote(env11), envir=globalenv()$env_of_envs), "env11")
})

test_that("T3) the environment name is NULL when the environment does not exist", {
  # Note: need to enclose 'env9' in quote() o.w. we receive an error message that 'env9' does not exist and the test fails
  expect_equal(environment_name(quote(env9)), NULL)
})

test_that("T4) the environment name is NULL when the envir environment does not exist", {
  skip("Test skipped because the ERROR shown by environment_name() is considered an error by the testthat package")
  expect_equal(environment_name(env11, envir=alskdjfl), NULL)
})

test_that("T5) the environment name is correctly returned when given as an environment (e.g. <environment: 0x0000000019942d08>)", {
  expect_equal(environment_name(as.environment(globalenv()$env1)),  "env1")
  expect_equal(environment_name(as.environment(globalenv()$env_of_envs$env11), envir=globalenv()$env_of_envs),  "env11")
})

test_that("T6) the environment name of an object given as a string which does not exist is NULL", {
  expect_equal(environment_name("doesNotExist"), NULL)
  expect_equal(environment_name("0x000000001806d1b8"), NULL)
})

test_that("T7) the environment name of an object given as a string containing the memory address of an environemnt returns the name of the environment", {
  expect_equal(environment_name(get_obj_address(globalenv()$env1)), "env1")
  expect_equal(environment_name(get_obj_address(globalenv()$env_of_envs$env11), envir=globalenv()$env_of_envs), "env11")
})

# 4.- Cleanup -------------------------------------------------------------
with(globalenv(), rm(list=c("env1", "env2", "env3", "env_of_envs")))
