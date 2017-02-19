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

x <- 5
env1 <- new.env()
env_of_envs <- new.env()
with(env_of_envs, { env2 <- new.env(); zz <- 6; env2$zz <- 7 })
env1$x <- 3
y = "x"

# Functions
f = function() {}

# Arrays and lists
v = c("x", "y", "z")
alist = list(z="x", v=3, u=2)
alist_quoted = c(quote(x), quote(y), quote(nonexistent))
alist_complex = list(quote(f()), v[1])


# 2.- TEST! ---------------------------------------------------------------
test_that("T1) the name of a simple variable name is returned, including the case when the variable is an environment", {
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

test_that("T4) the name of global and package environments are correctly returned", {
  skip("Valid for new version")
  expected = "package:stats"
  observed = get_obj_name(as.environment("package:stats"))
  expect_equal(observed, expected)
  expected = "R_GlobalEnv"
  observed = get_obj_name(as.environment(globalenv()))
  expect_equal(observed, expected)
})

test_that("T5) functions like get(), as.name(), etc. are taken into account before resolving the name", {
  skip("Valid for new version")
  expected = as.character(x)
  observed = get_obj_name(get("x"))
  expect_equal(observed, expected)
  expected = "x"
  observed = get_obj_name(as.name("x"))
  expect_equal(observed, expected)
})

test_that("T6) quotes at the beginning and end of the object are removed in the name returned.
          NOTE that no quotes are allowed in the middle as well because parse() used in the get_obj_name() function doesn't like it as a valid expression!", {
  skip("this behaviour should change")
  expected = "this contains begin and end quotes"
  observed = get_obj_name("\"this contains begin and end quotes\"")
  expect_equal(observed, expected)
})


### After the simplified version of get_obj_name()
test_that("T11) numeric values are returned as characters", {
  expect_equal(get_obj_name(3), "3")
})

test_that("T12) the name of user-defined environments are correctly returned", {
  skip("Valid for new version")
  expected = "env1"
  observed = get_obj_name(env1)
  expect_equal(observed, expected)

  expected = "env_of_envs$env2"
  observed = get_obj_name(env_of_envs$env2)
  expect_equal(observed, expected)
})

test_that("T13) the name of objects inside environments are correctly returned", {
  skip("Valid for new version")
  expected = "x"
  observed = with(env1, get_obj_name(x))
  expect_equal(observed, expected)
  
  expected = "env1$x"
  observed = get_obj_name(env1$x)
  expect_equal(observed, expected)
})

test_that("T14) the name of a function is correctly returned", {
  expected = "f"
  observed = get_obj_name(f())
  expect_equal(observed, expected)
})

test_that("T15) the function works on arrays and lists and using sapply()", {
  skip("Valid for new version")
  # Arrays ------------------------------------------------------------------
  # On one element
  expected = as.character(v[1])
  observed = get_obj_name(v[1], eval=TRUE)
  expect_equal(observed, expected)

  # Out of range element
  expected = NA_character_
  observed = get_obj_name(v[5], eval=TRUE)
  expect_equal(observed, expected)
  
  # sapply()
  expected = as.character(v)
  names(expected) = v
  observed = sapply(v, get_obj_name, eval=TRUE)
  expect_equal(observed, expected)

  # Lists -------------------------------------------------------------------
  #--- Named list of strings and values
  # On one element
  expected = as.character(alist[1])
  observed = get_obj_name(alist[1], eval=TRUE)
  expect_equal(observed, expected)
  expected = as.character(alist[[1]])
  observed = get_obj_name(alist[[1]], eval=TRUE)
  expect_equal(observed, expected)
  
  # Out of range element
  expected = NULL
  observed = get_obj_name(alist[5], eval=TRUE)  # Note that calling alist[[5]] gives "out of bounds" error
  expect_equal(observed, expected)

  # sapply()
  expected = as.character(alist)
  names(expected) = names(alist)
  observed = sapply(alist, get_obj_name, eval=TRUE)
  expect_equal(observed, expected)

  #--- List of quoted objects
  # On one element
  expected = "x"
  observed = get_obj_name(alist_quoted[1], eval=TRUE)
  expect_equal(observed, expected)

  # sapply()
  expected = as.character(alist_quoted)
  observed = sapply(alist_quoted, get_obj_name, eval=TRUE)
  expect_equal(observed, expected)

  #--- List of complex objects (e.g. quote(f()), v[1])
  # On one element
  expected = "f"
  observed = get_obj_name(alist_complex[1], eval=TRUE)
  expect_equal(observed, expected)
  expected = v[1]
  observed = get_obj_name(alist_complex[2], eval=TRUE)
  expect_equal(observed, expected)
  
  # sapply()
  expected = list(NULL, v[1])   # The first element is the evaluation of f() which gives NULL 
  observed = sapply(alist_complex, get_obj_name, eval=TRUE)
  expect_equal(observed, expected)
})


# Extreme cases -----------------------------------------------------------
test_that("T90) the name of non-existent objects inside environments using the $ notation are returned as the corresponding string.
          However, note that the name of non-existent objects WITHOUT using the $ notation give the error that the object is not found", {
  skip("Valid for new version")
  expected = "env1$nonexistent"
  observed = get_obj_name(env1$nonexistent)
  expect_equal(observed, expected)
})
# Extreme cases -----------------------------------------------------------


# 3.- Cleanup -------------------------------------------------------------
rm(list=ls())

})  # with(globalenv())
