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
rm(list=ls(all.names=TRUE))

x <- 1
z <- 5
env1 <- new.env()
env1$x <- 3;
env1$y <- 2;
with(env1, env <- new.env())

# check_object_exists() ------------------------------------------------------
test_that("T11) check_object_exists(): objects defined in an environment are found and their memory address is the one returned by the address() function", {
  expected = list(found=TRUE, eval=eval(env1$y), address=envnames:::address(env1$y))
  observed = envnames:::check_object_exists(y, envir=env1)
  expect_equal(observed, expected)
})

test_that("T12) check_object_exists(): objects defined in the GLOBAL environment are found and their memory address is the one returned by the address() function", {
  expected = list(found=TRUE, eval=eval(z), address=envnames:::address(z))
  observed = envnames:::check_object_exists(z)
  expect_equal(observed, expected)
})

test_that("T13) check_object_exists(): objects defined and having the same name in both the global environment and a user-defined environment are correctly found and their memory address is the one returned by the address() function", {
  # Look or the object in the global environment
  expected = list(found=TRUE, eval=eval(x), address=envnames:::address(x))
  observed = envnames:::check_object_exists(x)
  expect_equal(observed, expected)
  # Look or the object in the user-defined environment
  expected = list(found=TRUE, eval=eval(env1$x), address=envnames:::address(env1$x))
  observed = envnames:::check_object_exists(x, envir=env1)
  expect_equal(observed, expected)
})

test_that("T14) check_object_exists(): objects explicitly containing the environment they are in --e.g. <env>$<obj>-- are found and their memory address is the one returned by the address() function", {
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

test_that("T15) check_object_exists(): objects defined in a package are found and their memory address is the one returned by the address() function", {
  expected = list(found=TRUE, eval=eval(aov), address=envnames:::address(aov))
  observed = envnames:::check_object_exists(aov)
  expect_equal(observed, expected)
})

test_that("T16) objects given as strings are found if the object referenced by the string exists,
          ALTHOUGH their memory address should not be considered as VALID because they are the memory address of the string, NOT of the object!", {
  expected = list(found=TRUE, eval="env1$x")
  observed = envnames:::check_object_exists("env1$x")
  observed$address = NULL   # Do not compare the 'address' attribute
  expect_equal(observed, expected)
})

#----- Extreme cases ------
test_that("T911) check_object_exists(): NULL and NA return 'not found'", {
  expected = list(found=FALSE, eval=NULL, address=NULL)
  observed = envnames:::check_object_exists(NULL)
  expect_equal(observed, expected)
  expected = list(found=FALSE, eval=NULL, address=NULL)
  observed = envnames:::check_object_exists(NA)
  expect_equal(observed, expected)
})

test_that("T912) check_object_exists(): non-existing objects return 'not found'", {
  expected = list(found=FALSE, eval=NULL, address=NULL)
  observed = envnames:::check_object_exists(nonexistent)
  expect_equal(observed, expected)
  expected = list(found=FALSE, eval=NULL, address=NULL)
  observed = envnames:::check_object_exists(nonexistent, envir=env1)
  expect_equal(observed, expected)
})
# check_object_exists() ------------------------------------------------------


# is_memory_address() --------------------------------------------------------
test_that("T21) is_memory_address(): a string obtained as the memory address of an object is identified as a valid memory address", {
  expected = TRUE
  observed = envnames:::is_memory_address(envnames:::address(x))
  cat("correct memory address:", envnames:::address(x), "\n")
  cat("observed result:", observed, "\n")
  expect_equal(observed, expected)
})

test_that("T22) is_memory_address(): ALL valid ways of specifying a memory address as a string are identified as a valid memory address", {
  expected = TRUE
  
  # Memory addresses in Windows 32-bit (8 hexadecimal digits)
  observed = envnames:::is_memory_address("<0974E880>")
  expect_equal(observed, expected)
  
  observed = envnames:::is_memory_address("<0x0974E880>")
  expect_equal(observed, expected)
  
  observed = envnames:::is_memory_address("<environment: 0x0974E880>")
  expect_equal(observed, expected)

  # Memory addresses in Windows 64-bit (16 hexadecimal digits)
  observed = envnames:::is_memory_address("<000000000974E880>  ")
  expect_equal(observed, expected)
  
  observed = envnames:::is_memory_address("    <0x000000000974E880>  ")
  expect_equal(observed, expected)
  
  observed = envnames:::is_memory_address("<environment: 0x000000000974E880>  ")
  expect_equal(observed, expected)

  # Memory addresses in Linux Debian 64-bit (12 hexadecimal digits)
  observed = envnames:::is_memory_address("<00000974E880>")
  expect_equal(observed, expected)
  
  observed = envnames:::is_memory_address("<0x00000974E880>  ")
  expect_equal(observed, expected)
  
  observed = envnames:::is_memory_address("    <environment: 0x00000974E880>")
  expect_equal(observed, expected)
})

test_that("T23) is_memory_address(): invalid memory addresses return FALSE", {
  skip("the is_memory_address() function was changed to accept any number of digits between 8 and 16,
       since in Ubuntu Debian memory addresses have 12 digits!")
  expected = FALSE
  if (R.version$arch == envnames:::.pkgenv$ARCH_32BIT) {
    # 64-bit address in 32-bit architecture
    observed = envnames:::is_memory_address("<000000000974E880>")
    expect_equal(observed, expected)
  } else if (R.version$arch == envnames:::.pkgenv$ARCH_64BIT) {
    # 32-bit address in 64-bit architecture
    observed = envnames:::is_memory_address("<0974E880>")
    expect_equal(observed, expected)
  }
})

test_that("T24) is_memory_address(): out-of-range digits or wrong prefixes in an apparently well constructed memory addresses return FALSE", {
  expected = FALSE
  
  # Out-of-range digits    
  observed = envnames:::is_memory_address("<0x0974G880>")
  expect_equal(observed, expected)
  
  # Wrong prefix "environment" (the colon at the end is missing)
  observed = envnames:::is_memory_address("<environment 0x0974E880>")
  expect_equal(observed, expected)
  
  # Out-of-range digits    
  observed = envnames:::is_memory_address("<0x000000000974G880>")
  expect_equal(observed, expected)

  # Wrong prefix "environment" (the colon at the end is missing)
  observed = envnames:::is_memory_address("<environment 0x000000000974E880>")
  expect_equal(observed, expected)
})
# is_memory_address() --------------------------------------------------------

# standardize_env_name() -----------------------------------------------------
test_that("T31) standardize_env_name(): standardization of system environments works", {
  expect_equal(envnames:::standardize_env_name("globalenv()"), "R_GlobalEnv")
  expect_equal(envnames:::standardize_env_name(".GlobalEnv"), "R_GlobalEnv")
  expect_equal(envnames:::standardize_env_name("emptyenv()"), "R_EmptyEnv")
  expect_equal(envnames:::standardize_env_name("baseenv()"), "base")
  expect_equal(envnames:::standardize_env_name(".BaseNamespaceEnv"), "base")
})

test_that("T32) standardize_env_name(): standardization of package and namespace environments works", {
  expect_equal(envnames:::standardize_env_name("as.environment(\"package:base\")"), "base")
  expect_equal(envnames:::standardize_env_name("getNamespace(\"base\")"), "base")
  expect_equal(envnames:::standardize_env_name("as.environment(\"package:envnames\")"), "package:envnames")
  expect_equal(envnames:::standardize_env_name("getNamespace(\"envnames\")"), "envnames")
})

test_that("T33) standardize_env_name(): standardization of user environments gives the name of the environment", {
  expect_equal(envnames:::standardize_env_name("env1"), "env1")
})

test_that("T34) standardize_env_name(): standardization of the name of an existing variable
           that is NOT an environment gives the name unchanged", {
  expect_equal(envnames:::standardize_env_name("x"), "x")
})

test_that("T35) standardize_env_name(): standardization of the name of an expression that resolves to a user environment
           gives the expression unchanged", {
  expect_equal(envnames:::standardize_env_name("parent.env(env1$env)"), "parent.env(env1$env)")
})

test_that("T36) standardize_env_name(): standardization of non-existing variables gives the input name unchanged", {
  expect_equal(envnames:::standardize_env_name("asdfafd"), "asdfafd")
})
# standardize_env_name() -----------------------------------------------------

})