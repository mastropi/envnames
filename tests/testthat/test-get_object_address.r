# Created:      27-Jul-2016
# Author:       Daniel Mastropietro
# Description:  Test the envnames package using the testthat package
#

library(testthat)
library(envnames)
context("Object addresses")


# 1.- Prepare the workspace -----------------------------------------------
with(globalenv(), {
  env1 <- new.env()
  env_of_envs <- new.env()
  env_of_envs$env11 <- new.env()
  env_of_envs$env11$z <- 50
  env1$x <- 3;
  env1$y <- 2;
  objects <- c("x", "y")
  packages <- c(as.environment("package:stats"), as.environment(".GlobalEnv"))
})


# 2.- TEST! ---------------------------------------------------------------
test_that("T1) addresses of objects referenced through an environment (e.g. env1$x) are correctly returned", {
  # skip("not now")
  # browser()  # This can be used like a breakpoint for debugging. But stil F10 doesn't go to the next line, it will continue to the end of the program!
  addresses <- sapply(globalenv()$objects, FUN=get_obj_address, envir=env1)
  expect_equal(as.vector(addresses), c(envnames:::address(globalenv()$env1$x), envnames:::address(globalenv()$env1$y)))
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
  expect_equal(get_obj_address("env1"), envnames:::address(globalenv()$env1))
  expect_equal(get_obj_address("x", envir=globalenv()$env1), envnames:::address(globalenv()$env1$x))
})

test_that("T4) the address of an object passed as an object is correctly returned (in different environments)", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address(env1), envnames:::address(globalenv()$env1))
  expect_equal(get_obj_address(env11, envir=globalenv()$env_of_envs), envnames:::address(globalenv()$env_of_envs$env11))
  expect_equal(get_obj_address(x, envir=globalenv()$env1), envnames:::address(globalenv()$env1$x))
})

test_that("T5) the address of objects passed as expressions is correctly returned", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address(globalenv()$objects[1], envir=globalenv()$env1), envnames:::address(globalenv()$env1$x))
})

test_that("T6) the address returned for a non-existing object is NULL", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address("klajsdklfj"), NULL)
})

test_that("T7) the address returned for an object when the variable passed as environment of evaluation is not an environment, is NULL", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address(x, envir=globalenv()$env1$x), NULL)
})

test_that("T8) the address returned for an object referenced via its environment (as in env1$x) is NULL
          (as the way to retrieve its address is by separating the object name from the environment as in get_obj_address(x, envir=env1))", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address(globalenv()$env1$x), NULL)
})

test_that("T9) the address returned for an object stored in a deeply nested environment is correct", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address("z", envir=globalenv()$env_of_envs$env11), envnames:::address(globalenv()$env_of_envs$env11$z))
})

test_that("T10) the address of objects passed as memory address is the memory address itself", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address("<000000000C330188>"), "<000000000C330188>")
})

test_that("T11) the address of objects passed as incorrect memory addresses is NULL", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address("<000000000C3301GZ>"), NULL)
})



# 3.- Cleanup -------------------------------------------------------------
with(globalenv(), rm(list=c("env1", "env_of_envs", "objects", "packages")))
