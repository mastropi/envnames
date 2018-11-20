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

test_that("T0a) the table returned only contains system/package environments AND the empty environment
          when no user environments are defined, except for the user environments defined in the envnames package", {
  # skip("not now")
  expected = c(search(), names(envnames:::get_namespace_addresses()), "R_EmptyEnv")
  envmap = get_env_names()
  # Eliminate the function environments and the user environments defined in the envnames package
  # as I am not interested in checking those here
  observed = envmap$pathname[envmap[,"type"] != "function" & (envmap[,"location"] != "package:envnames" | is.na(envmap[,"location"]))]
  expect_equal(observed, expected)
})

# (2018/11/19)
# Test that the table can be returned without errors with only one row
# NOTE: This test CANNOT be run inside a test_that() call because
# when doing so the lookup table contains more than one row, as all
# the function execution environments are also included in the lookup table.
# AND IT CANNOT EVEN BE RUN INSIDE THIS test-get_env_names.r FILE BECAUSE
# ALSO FUNCTION ENVIRONMENTS APPEAR WHEN DOING SO!
# THEREFORE, REFER TO THE TEST IN envnames-package.Rd where we call
# environment_name(env11, envir=env_of_envs)   # "env11"
# WHICH TESTS WHAT WE WANT TO TEST HERE.
#env_of_envs = new.env()
#env_of_envs$env1 = new.env()
#expected = 1
#envmap = get_env_names(env_of_envs)
#expect_equal(nrow(envmap), expected)

# 1.- Prepare the workspace -----------------------------------------------
rm(list=ls())

env1 <- new.env()
env_of_envs <- new.env()
with(env_of_envs, env1 <- new.env())
with(env_of_envs$env1, env2 <- new.env())

# Compute the namespace addresses and their names (needed to run the tests)
namespace_addresses = envnames:::get_namespace_addresses()

# 2.- Tests ---------------------------------------------------------------
test_that("T1) the default behaviour is the expected one, i.e. all environments (user and packages)
           in the whole workspace are retrieved.
           User-defined environments are found in both the global environment and packages.
           The search is recursive on all user-defined environments found.
           This is the default behaviour and should be obtained when envir=NULL.
           Note that we do NOT compare the info about function execution environments
           because the expected value for is too complicated to write.", {
  # The following expected data frame is used when TESTing the package
  expected_test = data.frame(type=c(rep("user", 3),
                                    rep("user", 4),
                                    rep("system/package", length(search())),
                                    rep("namespace", length(namespace_addresses)),
                                    "empty"),
                        location=c(rep("package:envnames", 3),
                                   rep("R_GlobalEnv", 4),
                                   rep(NA, length(search())),
                                   rep(NA, length(namespace_addresses)),
                                   NA),
                        locationaddress=c(rep(address(as.environment("package:envnames")), 3),
                                          rep(address(globalenv()), 4),
                                          rep(NA, length(search())),
                                          rep(NA, length(namespace_addresses)),
                                          NA),
                        address=c(envnames:::address(as.environment("package:envnames")$testenv),
                                  envnames:::address(as.environment("package:envnames")$testenv$env1),
                                  envnames:::address(as.environment("package:envnames")$testenv$env1$env22),
                                  envnames:::address(globalenv()$env_of_envs),
                                  envnames:::address(globalenv()$env_of_envs$env1),
                                  envnames:::address(globalenv()$env_of_envs$env1$env2),
                                  envnames:::address(globalenv()$env1),
                                  sapply(search(), FUN=function(x) envnames:::address(as.environment(x))),
                                  namespace_addresses,
                                  address(emptyenv())),
                        pathname=c("testenv", "testenv$env1", "testenv$env1$env22",
                                   "env_of_envs", "env_of_envs$env1", "env_of_envs$env1$env2", "env1",
                                   search(),
                                   names(namespace_addresses),
                                   "R_EmptyEnv"),
                        path=c("", "testenv", "testenv$env1",
                               "", "env_of_envs", "env_of_envs$env1", "",
                               rep("", length(search())),
                               rep("", length(namespace_addresses)),
                               ""),
                        name=c("testenv", "env1", "env22",
                               "env_of_envs", "env1", "env2", "env1",
                               search(),
                               names(namespace_addresses),
                               "R_EmptyEnv"),
                        stringsAsFactors=FALSE)

  # The following expected data frame is used when CHECKing the package
  # (the order of the user-defined environments is different, for instance
  # the environmnts in the global environment come before those in the envnames package)
  expected_check = data.frame(type=c(rep("user", 4),
                                     rep("user", 3),
                                     rep("system/package", length(search())),
                                     rep("namespace", length(namespace_addresses)),
                                     "empty"),
                        location=c(rep("R_GlobalEnv", 4),
                                   rep("package:envnames", 3),
                                   rep(NA, length(search())),
                                   rep(NA, length(namespace_addresses)),
                                   NA),
                        locationaddress=c(rep(address(globalenv()), 4),
                                          rep(address(as.environment("package:envnames")), 3),
                                          rep(NA, length(search())),
                                          rep(NA, length(namespace_addresses)),
                                          NA),
                        address=c(envnames:::address(globalenv()$env1),
                                  envnames:::address(globalenv()$env_of_envs),
                                  envnames:::address(globalenv()$env_of_envs$env1),
                                  envnames:::address(globalenv()$env_of_envs$env1$env2),
                                  envnames:::address(as.environment("package:envnames")$testenv),
                                  envnames:::address(as.environment("package:envnames")$testenv$env1),
                                  envnames:::address(as.environment("package:envnames")$testenv$env1$env22),
                                  sapply(search(), FUN=function(x) envnames:::address(as.environment(x))),
                                  namespace_addresses,
                                  address(emptyenv())),
                        pathname=c("env1", "env_of_envs", "env_of_envs$env1", "env_of_envs$env1$env2",
                                   "testenv", "testenv$env1", "testenv$env1$env22",
                                   search(),
                                   names(namespace_addresses),
                                   "R_EmptyEnv"),
                        path=c("", "", "env_of_envs", "env_of_envs$env1",
                               "", "testenv", "testenv$env1",
                               rep("", length(search())),
                               rep("", length(namespace_addresses)), 
                               ""),
                        name=c("env1", "env_of_envs", "env1", "env2",
                               "testenv", "env1", "env22",
                               search(),
                               names(namespace_addresses),
                               "R_EmptyEnv"),
                        stringsAsFactors=FALSE)
  
  envmap = get_env_names()
  observed = envmap[envmap$type!="function",]
  rownames(observed) = 1:nrow(observed)
  #cat("observed data frame:\n")
  #print(observed)
  expect_result = try(expect_equal(observed, expected_test), silent=TRUE)
  if (inherits(expect_result, "try-error")) {
    expect_equal(observed, expected_check)
  }
})

test_that("T2) no packages in the search path are listed when the search environment is restricted to the global environment,
           and that the search is recursive on all user environments found.
           Note that we only compare the map for user environments, excluding function execution environments
           because that is out of scope and too complicated.", {
  # The following expected data frame is used when TESTing the package
  expected_test = data.frame(type=rep("user",4),
                        location=rep("R_GlobalEnv", 4),
                        locationaddress=rep(address(globalenv()), 4),
                        address=c(envnames:::address(globalenv()$env_of_envs),
                                  envnames:::address(globalenv()$env_of_envs$env1),
                                  envnames:::address(globalenv()$env_of_envs$env1$env2),
                                  envnames:::address(globalenv()$env1)),
                        pathname=c("env_of_envs", "env_of_envs$env1", "env_of_envs$env1$env2", "env1"),
                        path=c("", "env_of_envs", "env_of_envs$env1", ""),
                        name=c("env_of_envs", "env1", "env2", "env1"),
                        stringsAsFactors=FALSE)

  # The following expected data frame is used when CHECKing the package (the order of the user-defined environments is different)
  expected_check = data.frame(type=rep("user",4),
                        location=rep("R_GlobalEnv", 4),
                        locationaddress=rep(address(globalenv()), 4),
                        address=c(envnames:::address(globalenv()$env1),
                                  envnames:::address(globalenv()$env_of_envs),
                                  envnames:::address(globalenv()$env_of_envs$env1),
                                  envnames:::address(globalenv()$env_of_envs$env1$env2)),
                        pathname=c("env1", "env_of_envs", "env_of_envs$env1", "env_of_envs$env1$env2"),
                        path=c("", "", "env_of_envs", "env_of_envs$env1"),
                        name=c("env1", "env_of_envs", "env1", "env2"),
                        stringsAsFactors=FALSE)
  
  envmap = get_env_names(envir=globalenv())
  observed = envmap[1:4,]
  expect_result = try(expect_equal(observed, expected_test), silent=TRUE)
  if (inherits(expect_result, "try-error")) {
    expect_equal(observed, expected_check)
  }
})

test_that("T3) no packages in the search path are listed when the search environment is restricted to a user environment,
           and that the search is recursive on all user environments found.
           No function execution environments should be listed.", {
  expected = data.frame(type=rep("user",2),
                        location=rep("env_of_envs", 2),
                        locationaddress=rep(address(env_of_envs), 2),
                        address=c(envnames:::address(globalenv()$env_of_envs$env1), envnames:::address(globalenv()$env_of_envs$env1$env2)),
                        pathname=c("env1", "env1$env2"),
                        path=c("", "env1"),
                        name=c("env1", "env2"),
                        stringsAsFactors=FALSE)
  envmap = get_env_names(envir=env_of_envs)
  observed = envmap[envmap[,"type"] != "function",] # Eliminate the function environments as I am not interested in checking those here
  expect_equal(observed, expected)
})

test_that("T4) user-defined environments and nested environments are found inside a given package
           and no packages should be listed in the returned lookup table.", {
  #skip("this test should only be run when the package has exported environments --see global_definitions.r")
  expected = data.frame(type=rep("user",3),
                        location=rep("package:envnames", 3),
                        locationaddress=rep(address(as.environment("package:envnames")), 3),
                        address=c(address(envnames:::testenv), address(envnames:::testenv$env1), address(envnames:::testenv$env1$env22)),
                        pathname=c("testenv", "testenv$env1", "testenv$env1$env22"),
                        path=c("", "testenv", "testenv$env1"),
                        name=c("testenv", "env1", "env22"),
                        stringsAsFactors=FALSE)
  envmap = get_env_names(as.environment("package:envnames"))
  observed = envmap[envmap$type!="function",]
  expect_equal(observed, expected)
})

test_that("T5) user-defined environments and nested environments are found inside a given package namespace
           and no packages should be listed in the returned lookup table.", {
   #skip("this test should only be run when the package has exported environments --see global_definitions.r")
   # NOTE: The following ls() call can be used to retrieve ALL objects defined in a package (both exported and not)
   #allobjects = ls(getNamespace("envnames"))
   expected = data.frame(type=rep("user",3),
                         location=rep("envnames", 3),
                         locationaddress=rep(address(getNamespace("envnames")), 3),
                         address=c(address(envnames:::testenv), address(envnames:::testenv$env1), address(envnames:::testenv$env1$env22)),
                         pathname=c("testenv", "testenv$env1", "testenv$env1$env22"),
                         path=c("", "testenv", "testenv$env1"),
                         name=c("testenv", "env1", "env22"),
                         stringsAsFactors=FALSE)
   envmap = get_env_names(getNamespace("envnames"))
   observed = envmap[envmap$type!="function",]
   expect_equal(observed, expected)
})

# 3.- Cleanup -------------------------------------------------------------
rm(list=ls())

})  # with(globalenv())
