# Created:      17-Feb-2017
# Author:       Daniel Mastropietro
# Description:  Test the envnames package using the testthat package
#

library(testthat)
library(envnames)
context("Functions defined in functions.r")

# NOTE THE NEED TO RUN WITH with(globalenv()) BECAUSE THE OBJECTS ARE DEFINED IN THE GLOBAL ENVIRONMENT
# AND NOT IN THE ENVIRONMENT DEFINED BY testthat.
with(globalenv(), {
  
# 1.- Prepare the workspace -----------------------------------------------

# IMPORTANT: MAKE SURE THAT THE GLOBAL ENVIRONMENT IS CLEAN, WHICH MAY NOT BE THE CASE WHEN RUNNING
# THE TESTS THROUGH testthat (e.g. using the Check tool in RStudio or calling devtools::test())
rm(list=ls())

x <- 1
z <- 5
env1 <- new.env()
env1$x <- 3;
env1$y <- 2;

# check_obj_exists() ------------------------------------------------------
library(testthat)

test_that("T1) objects defined in an environment are found and their memory address is correct", {
  expected = list(found=TRUE, eval=eval(env1$y), address=envnames:::address(env1$y))
observed = envnames:::check_object_exists(y, envir=env1)
  expect_equal(observed, expected)
})

test_that("T2) objects defined in the global environment are found and their memory address is correct", {
  expected = list(found=TRUE, eval=eval(z), address=envnames:::address(z))
  observed = envnames:::check_object_exists(z)
  expect_equal(observed, expected)
})

test_that("T3) objects defined in both the global environment and a user-defined environment are correctly found and their memory address is correct", {
  # Look or the object in the global environment
  expected = list(found=TRUE, eval=eval(x), address=envnames:::address(x))
  observed = envnames:::check_object_exists(x)
  expect_equal(observed, expected)
  # Look or the object in the user-defined environment
  expected = list(found=TRUE, eval=eval(env1$x), address=envnames:::address(env1$x))
  observed = envnames:::check_object_exists(x, envir=env1)
  expect_equal(observed, expected)
})

test_that("T4) objects explicitly containing the environment they are in as e.g. <env>$<obj> are found and their memory address is correct", {
  expected = list(found=TRUE, eval=eval(env1$x), address=envnames:::address(env1$x))
  observed = envnames:::check_object_exists(env1$x)
  expect_equal(observed, expected)

  expected = list(found=TRUE, eval=eval(env1$x), address=envnames:::address(env1$x))
  observed = envnames:::check_object_exists(env1$x, envir=env1)
  expect_equal(observed, expected)

  expected = list(found=TRUE, eval=eval(env1$x), address=envnames:::address(env1$x))
  observed = envnames:::check_object_exists(globalenv()$env1$x)
  expect_equal(observed, expected)

  expected = list(found=TRUE, eval=eval(x), address=envnames:::address(x))
  observed = envnames:::check_object_exists(globalenv()$x)
  expect_equal(observed, expected)
})

test_that("T5) objects defined in a package are found and their memory address is correct", {
  expected = list(found=TRUE, eval=eval(aov), address=envnames:::address(aov))
  observed = envnames:::check_object_exists(aov)
  expect_equal(observed, expected)
})

#----- Extreme cases ------
test_that("T90) NULL, NA or strings return 'not found'", {
  expected = list(found=FALSE, eval=NULL, address=NULL)
  observed = envnames:::check_object_exists(NULL)
  expect_equal(observed, expected)
  expected = list(found=FALSE, eval=NULL, address=NULL)
  observed = envnames:::check_object_exists(NA)
  expect_equal(observed, expected)
  expected = list(found=FALSE, eval=NULL, address=NULL)
  observed = envnames:::check_object_exists("env1$x")
  expect_equal(observed, expected)
})

test_that("T91) non-existing objects return 'not found'", {
  expected = list(found=FALSE, eval=NULL, address=NULL)
  observed = envnames:::check_object_exists(nonexistent)
  expect_equal(observed, expected)
  expected = list(found=FALSE, eval=NULL, address=NULL)
  observed = envnames:::check_object_exists(nonexistent, envir=env1)
  expect_equal(observed, expected)
})
# check_obj_exists() ------------------------------------------------------

})