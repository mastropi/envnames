# Created:      27-Jul-2016
# Author:       Daniel Mastropietro
# Description:  Test the envnames package using the testthat package
#

library(envnames)
context("Object addresses")


# 1.- Prepare the workspace -----------------------------------------------
env1 <- new.env()
env_of_envs <- new.env()
env_of_envs$env11 <- new.env()
env_of_envs$env11$z <- 50
env1$x <- 3;
env1$y <- 2;
objects <- c("x", "y")
packages <- c(as.environment("package:stats"), as.environment(".GlobalEnv"))


# 2.- TEST! ---------------------------------------------------------------
test_that("addresses of objects referenced through an environment (e.g. env1$x) are correctly returned", {
  # skip("not now")
  # browser()  # This can be used like a breakpoint for debugging. But stil F10 doesn't go to the next line, it will continue to the end of the program!
  addresses <- sapply(objects, FUN=get_obj_address, envir=env1)
  expect_equal(as.vector(addresses), c(address(env1$x), address(env1$y)))
    ## Note: use as.vector(addresses) because o.w. the test fails because the names of the array elements do not match, but not the values
})

test_that("addresses of packages are correctly returned", {
  # skip("not now")
  # browser()
  addresses <- sapply(packages, FUN=get_obj_address)
  expect_equal(addresses, c(address(as.environment("package:stats")), address(as.environment(".GlobalEnv"))))
})

test_that("the address of an object passed as a string is correctly returned (in different environments)", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address("env"), address(env))
  expect_equal(get_obj_address("x", envir=env1), address(env1$x))
})

test_that("the address of an object passed as an object is correctly returned (in different environments)", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address(env), address(env))
  expect_equal(get_obj_address(env11, envir=env_of_envs), address(env_of_envs$env11))
  expect_equal(get_obj_address(x, envir=env1), address(env1$x))
})

test_that("the address of a objects passed as an expression is correctly returned", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address(objects[1]), address(x))
})

test_that("the address returned for a non-existing object is NULL", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address(klajsdklfj), NULL)
})

test_that("the address returned for an object when the variable passed as environment of evaluation is not an environment, is NULL", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address(env1$x, envir=env1$x), NULL)
})

test_that("the address returned for an object referenced via its environment (as in env1$x) is NULL", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address(env1$x), NULL)
})

test_that("the address returned for an object referenced via its environment (as in env1$x) is NULL", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address(z, envir=env_of_envs$env11), address(env_of_envs$env11$z))
})
