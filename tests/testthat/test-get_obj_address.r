# Created:      27-Jul-2016
# Author:       Daniel Mastropietro
# Description:  Test the envnames package using the testthat package
#

library(testthat)
library(envnames)
context("Object addresses")

# NOTE THE NEED TO RUN WITH with(globalenv()) BECAUSE THE OBJECTS ARE DEFINED IN THE GLOBAL ENVIRONMENT
# AND NOT IN THE ENVIRONMENT DEFINED BY testthat.
with(globalenv(), {
  
# 1.- Prepare the workspace -----------------------------------------------

# IMPORTANT: MAKE SURE THAT THE GLOBAL ENVIRONMENT IS CLEAN, WHICH MAY NOT BE THE CASE WHEN RUNNING
# THE TESTS THROUGH testthat (e.g. using the Check tool in RStudio or calling devtools::test())
rm(list=ls())

x <- 1
env1 <- new.env()
env_of_envs <- new.env()
env_of_envs$env11 <- new.env()
env_of_envs$env11$z <- 50
env1$x <- 3;
env1$y <- 2;
objects <- c("x", "y")
packages <- c(as.environment("package:stats"), as.environment(".GlobalEnv"))


# 2.- TEST! ---------------------------------------------------------------
test_that("T1) addresses of objects referenced through an environment (e.g. env1$x) are correctly returned", {
  # skip("not now")
  # browser()  # This can be used like a breakpoint for debugging. But stil F10 doesn't go to the next line, it will continue to the end of the program!
  addresses = sapply(globalenv()$objects, FUN=get_obj_address, envir=env1)
  expected = c(envnames:::address(globalenv()$env1$x), envnames:::address(globalenv()$env1$y))
  observed = as.vector(addresses)
  expect_equal(observed, expected)
    ## Note: use as.vector(addresses) because o.w. the test fails because the names of the array elements do not match, but not the values
})

test_that("T2) addresses of packages are correctly returned", {
  # skip("not now")
  # browser()
  addresses <- sapply(globalenv()$packages, FUN=get_obj_address)
  expect_equal(addresses, c(envnames:::address(as.environment("package:stats")), envnames:::address(as.environment(".GlobalEnv"))))
})

test_that("T3) the address of an object passed as a string is correctly returned (in different environments)", {
  # skip ("not now")
  # browser()
  expected = envnames:::address(globalenv()$env1)
  names(expected) = "R_GlobalEnv"
  expect_equal(get_obj_address("env1"), expected)
  expect_equal(get_obj_address("x", envir=globalenv()$env1), envnames:::address(globalenv()$env1$x))
})

test_that("T4) the address of an object passed as an object is correctly returned (in different environments)", {
  # skip ("not now")
  # browser()
  expected = envnames:::address(globalenv()$env1)
  names(expected) = "R_GlobalEnv"
  expect_equal(get_obj_address(env1), expected)
  expect_equal(get_obj_address(env11, envir=globalenv()$env_of_envs), envnames:::address(globalenv()$env_of_envs$env11))
  expect_equal(get_obj_address(x, envir=globalenv()$env1), envnames:::address(globalenv()$env1$x))
})

test_that("T5) the address of objects passed as expressions in specified environments is correctly returned
            (note that if we run this WITHOUT specifying the environment the address changes every time, because
            a new memory address is allocated to the result of globalenv()$objects[1], i.e. to 'x' which is
            the value of element 1 of 'objects'", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address(globalenv()$objects[1], envir=globalenv()$env1), envnames:::address(globalenv()$env1$x))
  expect_equal(get_obj_address(globalenv()$objects[1], envir=globalenv()), envnames:::address(globalenv()$x))
})

test_that("T6a) the address returned for an object referenced via its environment (as in env1$x) is the memory address of the object", {
  # skip ("not now")
  # browser()
  expected = envnames:::address(globalenv()$env1$x)
  names(expected) = "env1"
  observed = get_obj_address(globalenv()$env1$x)
  expect_equal(observed, expected)
})

test_that("T6b) the address referenced with the full path from within another environment using the with() statement
          is correctly returned (THIS IS QUITE TRICKY!!)", {
  # skip ("not now")
  expected = envnames:::address(globalenv()$env1$x)
  names(expected) = "env1"
  observed = with(env_of_envs, get_obj_address(globalenv()$env1$x))
  expect_equal(observed, expected)
})

test_that("T7) the address returned for a non-existing object is NULL", {
  # skip ("not now")
  # browser()
  # test 1
  expect_equal(get_obj_address("klajsdklfj"), NULL)
  # test 2: inside an environment
  expect_equal(get_obj_address(env1$dfsdf), NULL)
  # test 3: now from a specified environment
  expect_equal(get_obj_address(env1$dfsdf, envir=env1), NULL)
})

test_that("T8) the address returned for an object when the variable passed as environment of evaluation
          is not an environment, is NULL, and the appropriate error message is shown", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address(x, envir=globalenv()$env1$x), NULL)
  expect_message(get_obj_address(x, envir=globalenv()$env1$x), "*'3' is not a valid environment*")
})

test_that("T9) the address returned for an object stored in a deeply nested environment is correct", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address("z", envir=globalenv()$env_of_envs$env11), envnames:::address(globalenv()$env_of_envs$env11$z))
})

test_that("T10) the address of objects passed as memory address is NULL", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address("<000000000C330188>", envir=globalenv()), NULL)
  expect_equal(get_obj_address("<000000000C330188>"), NULL)
})


# 3.- Cleanup -------------------------------------------------------------
rm(list=ls())

})  # with(globalenv())
