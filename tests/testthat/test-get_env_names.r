# Created:      30-Sep-2016
# Author:       Daniel Mastropietro
# Description:  Test the envnames package using the testthat package
#

library(testthat)
library(envnames)
context("Table of address-name pairs")


# 1.- Prepare the workspace -----------------------------------------------



# 2.- Tests ---------------------------------------------------------------
test_that("Create the table of all environments (user and packages) in the whole workspace including packages.
           The search is recursive on all environments found.
           This is the default behaviour and should be obtained when envir=NULL.", {
  # skip("not now")
  expected = NULL
  observed = NULL
  expect_equal(observed, expected)
})

test_that("Create the table of user-environments present in a given user-environment (no packages should be listed)
           The search is recursive on all environments found in the given user-environment.", {
  # skip("not now")
  expected = NULL
  observed = NULL
  expect_equal(observed, expected)
})

test_that("Create the table of user-environments present in a given package (no packages should be listed).
           The global environment is a valid package.
           The search is recursive on all environments found in the given package.", {
  # skip("not now")
  expected = NULL
  observed = NULL
  expect_equal(observed, expected)
})


# 3.- Cleanup -------------------------------------------------------------






