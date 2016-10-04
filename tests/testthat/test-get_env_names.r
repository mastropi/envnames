# Created:      30-Sep-2016
# Author:       Daniel Mastropietro
# Description:  Test the envnames package using the testthat package
#

library(testthat)
library(envnames)
context("Table of address-name pairs")

# NULL test before setting up the environment
rm(list=ls())
test_that("The table returned only contains system environments when no user environments are defined", {
  # skip("not now")
  expected = search()
  envmap = get_env_names()
  observed = envmap$pathname
  expect_equal(observed, expected)
})


# 1.- Prepare the workspace -----------------------------------------------
with(globalenv(), {
  env1 <- new.env()
  env_of_envs <- new.env()
  with(env_of_envs, env1 <- new.env())
})

# 2.- Tests ---------------------------------------------------------------
test_that("Create the table of all environments (user and packages) in the whole workspace including packages.
           The search is recursive on all environments found.
           This is the default behaviour and should be obtained when envir=NULL.", {
  skip("it fails when run through the package CHECK functionality but it doesn't fail when run through its TEST functionality... WHY??")
  expected = data.frame(type=c(rep("user",3), rep("system", length(search()))),
                        location=c(rep("R_GlobalEnv", 3), rep(NA, length(search()))),
                        address=c(envnames:::address(globalenv()$env_of_envs), envnames:::address(globalenv()$env_of_envs$env1), envnames:::address(globalenv()$env1),
                                  sapply(search(), FUN=function(x) envnames:::address(as.environment(x)))),
                        pathname=c("env_of_envs", "env_of_envs$env1", "env1", search()),
                        path=c("", "env_of_envs", "", rep("", length(search()))),
                        name=c("env_of_envs", "env1", "env1", search()),
                        stringsAsFactors=FALSE)
  envmap = get_env_names()
  observed = envmap
  expect_equal(observed, expected)
})

test_that("Create the table of user-environments present in a given user-environment (no packages should be listed)
           The search is recursive on all environments found in the given user-environment.", {
  skip("it fails when run through the package CHECK functionality but it doesn't fail when run through its TEST functionality... WHY??")
  expected = c("env_of_envs", "env_of_envs$env1", "env1")
  expected = data.frame(type=rep("user",3),
                        location=rep("R_GlobalEnv", 3),
                        address=c(envnames:::address(globalenv()$env_of_envs), envnames:::address(globalenv()$env_of_envs$env1), envnames:::address(globalenv()$env1)),
                        pathname=c("env_of_envs", "env_of_envs$env1", "env1"),
                        path=c("", "env_of_envs", ""),
                        name=c("env_of_envs", "env1", "env1"),
                        stringsAsFactors=FALSE)
  envmap = get_env_names(envir=globalenv())
  observed = envmap
  expect_equal(observed, expected)
})

test_that("Create the table of user-environments present in a given package (no packages should be listed).
           The global environment is a valid package.
           The search is recursive on all environments found in the given package.", {
  skip("this test should only be run when the package has exported environments --see global_definitions.r")
  # NOTE: The following ls() call can be used to retrieve ALL objects defined in a package (both exported and not)
  #allobjects = ls(getNamespace("envnames"))
  expected = data.frame(type=rep("user",2),
                        location=rep("package:envnames", 2),
                        address=c(envnames:::address(testenv), envnames:::address(testenv$env1)),
                        pathname=c("testenv", "testenv$env1"),
                        path=c("", "testenv"),
                        name=c("testenv", "env1"),
                        stringsAsFactors=FALSE)
  envmap = get_env_names(as.environment("package:envnames"))
  observed = envmap
  expect_equal(observed, expected)
})


# 3.- Cleanup -------------------------------------------------------------
with(globalenv(), rm("env1", "env_of_envs"))
