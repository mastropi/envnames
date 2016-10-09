# Created:      05-Oct-2016
# Author:       Daniel Mastropietro
# Description:  Test the envnames package using the testthat package
#

library(testthat)
library(envnames)
context("Get the name of objects")

# NOTE THE NEED TO RUN WITH with(globalenv()) BECAUSE THE OBJECTS ARE DEFINED IN THE GLOBAL ENVIRONMENT
# AND NOT IN THE ENVIRONMENT DEFINED BY testthat.
with(globalenv(), {

# 1.- Prepare the workspace -----------------------------------------------
# IMPORTANT: MAKE SURE THAT THE GLOBAL ENVIRONMENT IS CLEAN, WHICH MAY NOT BE THE CASE WHEN RUNNING
# THE TESTS THROUGH testthat (e.g. using the Check tool in RStudio or calling devtools::test())
rm(list=ls())

env1 <- new.env()
env_of_envs <- new.env()
with(env_of_envs, { env2 <- new.env(); zz <- 6; env2$zz <- 7 })
env1$x <- 3
y = "x"
v = c("x", "y", "z")
alist = list(z="x", v=3, u=2)


# 2.- TEST! ---------------------------------------------------------------
test_that("T1) the name of a simple variable name is returned, including if the variable is an environment", {
  # skip("not now")
  # browser()  # This can be used like a breakpoint for debugging. But stil F10 doesn't go to the next line, it will continue to the end of the program!
  # test 1
  expected = "x"
  observed = get_obj_name(x)
  expect_equal(observed, expected)
  # test2: the variable is an environment
  expected = "env1"
  observed = get_obj_name(env1)
  expect_equal(observed, expected)
})

test_that("T2) the name of a simple variable name given as a string is returned", {
  expected = "x"
  observed = get_obj_name("x")
  expect_equal(observed, expected)
})

test_that("T3) the name of a variable with quotes inside, still contains the quotes", {
  expected = "as.environment(\"package:stats\")"
  observed = get_obj_name("as.environment(\"package:stats\")")
  expect_equal(observed, expected)
})

test_that("T4) the name of a call to as.environment() resolves to the string inside the as.environment() function", {
  expected = "package:stats"
  observed = get_obj_name(as.environment("package:stats"))
  expect_equal(observed, expected)
  expected = "globalenv()"
  observed = get_obj_name(as.environment(globalenv()))
  expect_equal(observed, expected)
})

test_that("T5) functions like get(), as.name(), etc. are ignored when resolving the name", {
  expected = "x"
  observed = get_obj_name(get("x"))
  expect_equal(observed, expected)
  expected = "x"
  observed = get_obj_name(as.name("x"))
  expect_equal(observed, expected)
})

test_that("T6) quotes at the beginning and end of the object are removed in the name returned.
          NOTE that no quotes are allowed in the middle as well because parse() used in the get_obj_name() function doesn't like it as a valid expression!", {
  expected = "this contains begin and end quotes"
  observed = get_obj_name("\"this contains begin and end quotes\"")
  expect_equal(observed, expected)
})


# 3.- Cleanup -------------------------------------------------------------
rm(list=ls())

})  # with(globalenv())
