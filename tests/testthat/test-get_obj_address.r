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

# Environments
e <- new.env()
env1 <- new.env()
env_of_envs <- new.env()
env_of_envs$env11 <- new.env()
env_of_envs$env11$z <- 50

# Variables
x <- 1
env1$x <- 3
env1$y <- 2
e$variable_in_e <- 10

# Arrays and lists
objects <- c("x", "y", "z", "nonexistent")
alist <- list("x", "y", "z", "nonexistent")
alist_named <- list(var1="x", var2="y", var3="z", var4="nonexistent")
packages <- c(as.environment("package:stats"), as.environment(".GlobalEnv"))


# 2.- TEST! ---------------------------------------------------------------
test_that("T1) addresses of objects referenced through an environment (e.g. env1$x) are correctly returned", {
  # skip("not now")
  # browser()  # This can be used like a breakpoint for debugging. But stil F10 doesn't go to the next line, it will continue to the end of the program!
  expected = envnames:::address(env1$x)
  names(expected) = "env1"
  observed = get_obj_address(env1$x)
  expect_equal(observed, expected)
    ## Note: use as.vector(addresses) because o.w. the test fails because the names of the array elements do not match, but not the values
})

test_that("T2) addresses of packages are correctly returned", {
  # skip("not now")
  # browser()
  addresses <- sapply(globalenv()$packages, FUN=get_obj_address)
  expect_equal(addresses, c(envnames:::address(as.environment("package:stats")), envnames:::address(as.environment(".GlobalEnv"))))
})

test_that("T4) the address of an object is correctly returned in different environments", {
  skip("This test gives different results when running in TEST mode and when running in CHECK mode, and it fails when testing the package in https://win-builder.r-project.org/ which is the real test before package submission to CRAN.")
  # browser()
  expected = envnames:::address(globalenv()$env1)
  names(expected) = "R_GlobalEnv"
  expect_equal(get_obj_address(env1), expected)
  expect_equal(get_obj_address(env11, envir=globalenv()$env_of_envs), envnames:::address(globalenv()$env_of_envs$env11))
  expect_equal(get_obj_address(x, envir=globalenv()$env1), envnames:::address(globalenv()$env1$x))

  # The address of ALL the objects found with the same name (in different environments) are returned
  # *** WARNING: the result of the test changes depending if it's run under TEST or CHECK!
  # *** The difference apparently happens because the order of characters under TEST is different 
  # *** than the order of names in the CHECK!
  # *** because under TEST "R_GlobalEnv" comes AFTER "env1" but under CHECK it comes BEFORE "env1"
  # *** AMAZING!
  # Use the following when running the test with TEST package
  #expected = c(envnames:::address(globalenv()$env1$x), envnames:::address(x))
  # Use the following when running the test with CHECK package
  expected = c(envnames:::address(x), envnames:::address(globalenv()$env1$x))
  names(expected) = sort(c("env1", "R_GlobalEnv"))
  observed = get_obj_address(x)
  expect_equal(observed, expected)
})

test_that("T5a) the address of objects passed as expressions in SPECIFIED or UNSPECIFIED environments is correctly returned
            (2017/02/17) WHAT FOLLOWS IS NO LONGER TRUE (I FIXED THIS AND ADDED 3 NEW TESTS TO PROVE THAT on globalenv()$objects[2])
            (note that if we run this WIenvnames:::address(globalenv()$env1$y)THOUT specifying the environment the address changes every time, because
            a new memory address is allocated to the result of globalenv()$objects[1], i.e. to 'x' which is
            the value of element 1 of 'objects'", {
  # skip ("not now")
  # browser()
  # The environment where the object is located is SPECIFIED
  expect_equal(get_obj_address(globalenv()$objects[1], envir=globalenv()$env1), envnames:::address(globalenv()$env1$x))
  expect_equal(get_obj_address(globalenv()$objects[1], envir=globalenv()), envnames:::address(globalenv()$x))
  expect_equal(get_obj_address(globalenv()$objects[2], envir=globalenv()$env1), envnames:::address(globalenv()$env1$y))

  # Without specifying the environment where the object is located
  expected = envnames:::address(globalenv()$env1$y)
  names(expected) = "env1"
  expect_equal(get_obj_address(globalenv()$objects[2]), expected)
})

test_that("T5b) sapply() works on arrays, unnamed lists, and named lists", {
  # skip ("not now")
  # browser()

  # First check that get_obj_address(nonexistent) is NULL
  expect_equal(get_obj_address(nonexistent), NULL)

  # On arrays
  expected = list(x=get_obj_address(x), y=get_obj_address(y), z=get_obj_address(z), nonexistent=get_obj_address(nonexistent))
  observed = sapply(objects, get_obj_address)
  expect_equal(observed, expected)

  # On unnamed lists
  expected = list(get_obj_address(x), get_obj_address(y), get_obj_address(z), get_obj_address(nonexistent))
  observed = sapply(alist, get_obj_address)
  expect_equal(observed, expected)

  # On named lists
  expected = list(var1=get_obj_address(x), var2=get_obj_address(y), var3=get_obj_address(z), var4=get_obj_address(nonexistent))
  observed = sapply(alist_named, get_obj_address)
  expect_equal(observed, expected)
  
  # Now specify a specific environment through the envir= option
  expected = list(envnames:::address(env1$x), envnames:::address(env1$y), NULL, NULL)
  names(expected) = objects
  observed = sapply(objects, FUN=get_obj_address, envir=env1)
  expect_equal(observed, expected)
})

test_that("T5c) The address of an object whose name is defined in a list is correctly returned", {
  # skip ("not now")
  # browser()
  
  # On unnamed lists
  expected = get_obj_address(x)
  observed = get_obj_address(alist[[1]])
  expect_equal(observed, expected)

  # On named lists  
  expected = get_obj_address(x)
  observed = get_obj_address(alist_named$var1)
  expect_equal(observed, expected)
  expected = get_obj_address(x)
  observed = get_obj_address(alist_named[[1]])
  expect_equal(observed, expected)
})

test_that("T5d) The address of the element of a list referenced as if the list were an array is NULL
            (because the memory address of such element (e.g. alist[1]) changes every time it is run!)", {
  # On unnamed lists
  expected = NULL
  observed = get_obj_address(alist[1])
  expect_equal(observed, expected)

  # On named lists
  expected = NULL
  observed = get_obj_address(alist_named[1])
  expect_equal(observed, expected)
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
          is correctly returned (THIS WAS QUITE TRICKY TO IMPLEMENT!!)", {
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

# or in environments recursively defined within the
#' given environment

test_that("T9) the address returned for an object stored in a deeply nested environment is correct or, in other words,
          the search for an object is recursive an all the environments defined within every environment", {
  # skip ("not now")
  # browser()
  # If we explictly give the environment where the object exists
  expect_equal(get_obj_address(z, envir=globalenv()$env_of_envs$env11), envnames:::address(globalenv()$env_of_envs$env11$z))
  # If we DON'T give the environment where the object exists (but simply search for it recursively through all environments
  # defined within each environment of the workspace)
  # --> THIS IS THE NICE THING ABOUT THIS PACKAGE! :)
  expected = envnames:::address(globalenv()$env_of_envs$env11$z)
  names(expected) = "env_of_envs$env11"
  expect_equal(get_obj_address(z), expected)
})

test_that("T10) the address of objects passed as memory address is NULL", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address("<000000000C330188>", envir=globalenv()), NULL)
  expect_equal(get_obj_address("<000000000C330188>"), NULL)
})

test_that("T20) that promise errors happen... although... does it make sense that they happen?", {
  skip("This test fails on CRAN")
  # NOTE: Here we are testing that the call to expect_error(expect_equal()) return the "promise already under evaluation" error, which:
  # - in the case of get_obj_address(y, ...) is raised precisely when calling expect_equal()
  # - in the case of get_obj_address(x, ...) is raised ONLY when expect_equal() is enclosed by expect_error(). This is weird...
  # (the difference between x and y is that x is defined in the global environment and y is defined in environment 'env1')
  # Note also that, even if the "promise" error is not raised, we would NOT be easily able to test that the object addresses are
  # correct, because it is not easy to access the environment of the functions called by the object search process!
  # calling chain that makes the object being searched for appear in different function environments (e.g. compare())
  # References for the "promise" error:
  # https://stackoverflow.com/questions/4357101/promise-already-under-evaluation-recursive-default-argument-reference-or-earlie
  # (in the above link they say that the error happens because of parameters of the form x=x and they suggest defining parameter names
  # by adding a dot... e.g. x.=x. I don't understand this.)
  # https://stackoverflow.com/questions/17310825/r-promise-already-under-evaluation
  # (in the above link they show the tip of using sapply(sys.frames(), ls) to retrieve the objects defined in the execution
  # environment of each function in the functions calling chain)
  expect_error(expect_equal(get_obj_address(y, include_functions=TRUE), ""), "promise")
  expect_error(expect_equal(get_obj_address(x, include_functions=TRUE), c("address1", "address2", "address3")), "promise")
})  

test_that("T21) specifying include_functions=TRUE returns the address of the object in ALL the environments where it is found", {
  # When calling get_obj_address() from outside expect_equal(), the object is only found in the global environment!
  expected = envnames:::address(env1$y)
  names(expected) = "env1"
  observed = get_obj_address(y, include_functions=TRUE)
  expect_equal(observed, expected)
})

#---------------------------------- Extreme cases -----------------------------------
test_that("T90) the address of NA, NULL or a string is NULL (even if the object referenced by the string exists)", {
  # skip ("not now")
  # browser()
  expect_equal(get_obj_address(NA), NULL)
  expect_equal(get_obj_address(NULL), NULL)
  expect_equal(get_obj_address("env1$x"), NULL)
  expect_equal(get_obj_address("x"), NULL)
})

test_that("T91) the address of an object in an environment called 'e' is correctly returned. This may be an issue because
          there is a local variable called 'e' in get_obj_address() that I have at some point seen conflicting with
          the environment 'e' the object we are looking for is located... (this was fixed now --2017/02/19)", {
  expect_equivalent(get_obj_address(variable_in_e), envnames:::address(e$variable_in_e))
})

test_that("T92) the address returned for an object when the variable passed as environment of evaluation
          is not an environment, is NULL, and the appropriate error message is shown", {
    # skip ("not now")
    # browser()
    expect_equal(get_obj_address(x, envir=globalenv()$env1$x), NULL)
    expect_message(get_obj_address(x, envir=globalenv()$env1$x), "*'3' is not a valid environment*")
})

# This was the previous test I had in place to test that the address of an object given as a string is correctly returned
test_that("T3OLD) the address of an object passed as a string is correctly returned (in different environments)", {
  skip ("no longer true: we now return always NULL for strings")
  expected = envnames:::address(globalenv()$env1)
  names(expected) = "R_GlobalEnv"
  expect_equal(get_obj_address("env1"), expected)
  expect_equal(get_obj_address("x", envir=globalenv()$env1), envnames:::address(globalenv()$env1$x))
})

test_that("Empty data frames and lists do not cause errors", {
  # Related to issue #12
  expect_no_error(get_obj_address(data.frame()))
  expect_no_error(get_obj_address(list()))
})

#---------------------------------- Extreme cases -----------------------------------

# 3.- Cleanup -------------------------------------------------------------
rm(list=ls())

})  # with(globalenv())
