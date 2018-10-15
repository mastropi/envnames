# Created:      30-Sep-2016
# Author:       Daniel Mastropietro
# Description:  Test the envnames package using the testthat package
#

library(testthat)
library(envnames)
context("Look up table of address-name pairs")

# NOTE THE NEED TO RUN WITH with(globalenv()) BECAUSE THE OBJECTS ARE DEFINED IN THE GLOBAL ENVIRONMENT
# AND NOT IN THE ENVIRONMENT DEFINED BY testthat.
with(globalenv(), {
  
# NULL test before setting up the environment
rm(list=ls())

test_that("T0) The table returned only contains system/package environments AND the empty environment
          when no user environments are defined", {
  # skip("not now")
  expected = c(search(), names(envnames:::get_namespace_addresses()), "R_EmptyEnv")
  envmap = get_env_names()
  observed = envmap$pathname[envmap[,"type"] != "function"] # Eliminate the function environments as I am not interested in checking those here
  expect_equal(observed, expected)
})

# 1.- Prepare the workspace -----------------------------------------------
rm(list=ls())

env1 <- new.env()
env_of_envs <- new.env()
with(env_of_envs, env1 <- new.env())
with(env_of_envs$env1, env2 <- new.env())

# Compute the namespace addresses and their names (needed to run the tests)
namespace_addresses = envnames:::get_namespace_addresses()

# 2.- Tests ---------------------------------------------------------------
test_that("T1) Create the table of all environments (user and packages) in the whole workspace including packages.
           The search is recursive on all environments found.
           This is the default behaviour and should be obtained when envir=NULL.", {
  skip("it fails when run through the package CHECK functionality but it doesn't fail when run through its TEST functionality... WHY?? (printing the output doesn't show it in CHECK)")
  expected = data.frame(type=c(rep("user",4), rep("system/package", length(search())), rep("namespace", length(namespace_addresses))),
                        location=c(rep("R_GlobalEnv", 4), rep(NA, length(search())), rep(NA, length(namespace_addresses))),
                        address=c(envnames:::address(globalenv()$env_of_envs), envnames:::address(globalenv()$env_of_envs$env1), envnames:::address(globalenv()$env_of_envs$env1$env2), envnames:::address(globalenv()$env1),
                                  sapply(search(), FUN=function(x) envnames:::address(as.environment(x))), namespace_addresses),
                        pathname=c("env_of_envs", "env_of_envs$env1", "env_of_envs$env1$env2", "env1", search(), names(namespace_addresses)),
                        path=c("", "env_of_envs", "env_of_envs$env1", "", rep("", length(search())), rep("", length(namespace_addresses))),
                        name=c("env_of_envs", "env1", "env2", "env1", search(), names(namespace_addresses)),
                        stringsAsFactors=FALSE)
  envmap = get_env_names()
  observed = envmap
  expect_equal(observed, expected)
})

test_that("T2) Create the table of user-environments present just in the global environment
           (no packages in the search() path should be listed)
           The search is recursive on all environments found in the given user-environment.", {
  skip("To be run MANUALLY by executing these lines below because the test fails when run through the package
       CHECK or TEST functionalities... WHY?? (printing the output doesn't show it in CHECK)")
  expected = data.frame(type=rep("user",4),
                        location=rep("R_GlobalEnv", 4),
                        locationaddress=rep(NA, 4),
                        address=c(envnames:::address(globalenv()$env_of_envs), envnames:::address(globalenv()$env_of_envs$env1), envnames:::address(globalenv()$env_of_envs$env1$env2), envnames:::address(globalenv()$env1)),
                        pathname=c("env_of_envs", "env_of_envs$env1", "env_of_envs$env1$env2", "env1"),
                        path=c("", "env_of_envs", "env_of_envs$env1", ""),
                        name=c("env_of_envs", "env1", "env2", "env1"),
                        stringsAsFactors=FALSE)
  envmap = get_env_names(envir=globalenv())
  observed = envmap
  expect_equal(observed, expected)
})

test_that("T3) Create the table of user-environments present in a given user-defined environment
           (no packages in the search() path should be listed)
           The search is recursive on all environments found in the given user-environment.", {
  #skip("it fails when run through the package CHECK functionality but it doesn't fail when run through its TEST functionality... WHY??")
  expected = data.frame(type=rep("user",2),
                        location=rep("env_of_envs", 2),
                        locationaddress=rep(NA_character_, 2),
                        address=c(envnames:::address(globalenv()$env_of_envs$env1), envnames:::address(globalenv()$env_of_envs$env1$env2)),
                        pathname=c("env1", "env1$env2"),
                        path=c("", "env1"),
                        name=c("env1", "env2"),
                        stringsAsFactors=FALSE)
  envmap = get_env_names(envir=env_of_envs)
  observed = envmap[envmap[,"type"] != "function",] # Eliminate the function environments as I am not interested in checking those here
  expect_equal(observed, expected)
})

test_that("T4) Create the table of user-environments present in a given package (no packages should be listed).
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
rm(list=ls())

})  # with(globalenv())
