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
test_that("T1) the default behaviour is the expected one, i.e. all environments (user and packages)
           in the whole workspace are retrieved.
           The search is recursive on all environments found.
           This is the default behaviour and should be obtained when envir=NULL.
           Note that we do NOT compare the info about function execution environments
           because the expected value for is too complicated to write.", {
  # WARNING: Use the following expected data frame when TESTing the package
  expected = data.frame(type=c(rep("user",4),
                               rep("system/package", length(search())),
                               rep("namespace", length(namespace_addresses)),
                               "empty"),
                        location=c(rep("R_GlobalEnv", 4),
                                   rep(NA, length(search())),
                                   rep(NA, length(namespace_addresses)),
                                   NA),
                        locationaddress=c(rep(address(globalenv()), 4),
                                          rep(NA, length(search())),
                                          rep(NA, length(namespace_addresses)),
                                          NA),
                        address=c(envnames:::address(globalenv()$env_of_envs),
                                  envnames:::address(globalenv()$env_of_envs$env1),
                                  envnames:::address(globalenv()$env_of_envs$env1$env2),
                                  envnames:::address(globalenv()$env1),
                                  sapply(search(), FUN=function(x) envnames:::address(as.environment(x))),
                                  namespace_addresses,
                                  address(emptyenv())),
                        pathname=c("env_of_envs", "env_of_envs$env1", "env_of_envs$env1$env2", "env1",
                                   search(),
                                   names(namespace_addresses),
                                   "R_EmptyEnv"),
                        path=c("", "env_of_envs", "env_of_envs$env1", "",
                               rep("", length(search())),
                               rep("", length(namespace_addresses)),
                               ""),
                        name=c("env_of_envs", "env1", "env2", "env1",
                               search(),
                               names(namespace_addresses),
                               "R_EmptyEnv"),
                        stringsAsFactors=FALSE)

  # WARNING: Use the following expected data frame when CHECKing the package (the order of the user-defined environments is different)
  expected = data.frame(type=c(rep("user",4),
                               rep("system/package", length(search())),
                               rep("namespace", length(namespace_addresses)),
                               "empty"),
                        location=c(rep("R_GlobalEnv", 4),
                                   rep(NA, length(search())),
                                   rep(NA, length(namespace_addresses)),
                                   NA),
                        locationaddress=c(rep(address(globalenv()), 4),
                                          rep(NA, length(search())),
                                          rep(NA, length(namespace_addresses)),
                                          NA),
                        address=c(envnames:::address(globalenv()$env1),
                                  envnames:::address(globalenv()$env_of_envs),
                                  envnames:::address(globalenv()$env_of_envs$env1),
                                  envnames:::address(globalenv()$env_of_envs$env1$env2),
                                  sapply(search(), FUN=function(x) envnames:::address(as.environment(x))),
                                  namespace_addresses,
                                  address(emptyenv())),
                        pathname=c("env1", "env_of_envs", "env_of_envs$env1", "env_of_envs$env1$env2",
                                   search(),
                                   names(namespace_addresses),
                                   "R_EmptyEnv"),
                        path=c("", "", "env_of_envs", "env_of_envs$env1",
                               rep("", length(search())),
                               rep("", length(namespace_addresses)), 
                               ""),
                        name=c("env1", "env_of_envs", "env1", "env2",
                               search(),
                               names(namespace_addresses),
                               "R_EmptyEnv"),
                        stringsAsFactors=FALSE)
  
  envmap = get_env_names()
  observed = envmap[envmap$type!="function",]
  rownames(observed) = 1:nrow(observed)
  expect_equal(observed, expected)
})

test_that("T2) no packages in the search path are listed when the search environment is restricted to the global environment,
           and that the search is recursive on all user environments found.
           Note that we only compare the map for user environments, excluding function execution environments
           because that is out of scope and too complicated.", {
  # WARNING: Use the following expected data frame when TESTing the package
  expected = data.frame(type=rep("user",4),
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

  # WARNING: Use the following expected data frame when CHECKing the package (the order of the user-defined environments is different)
  expected = data.frame(type=rep("user",4),
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
  expect_equal(observed, expected)
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
  # NOTE: The following ls() call can be used to retrieve ALL objects defined in a package (both exported and not)
  #allobjects = ls(getNamespace("envnames"))
  expected = data.frame(type=rep("user",2),
                        location=rep("package:envnames", 2),
                        locationaddress=rep(address(as.environment("package:envnames")), 2),
                        address=c(address(envnames:::testenv), address(envnames:::testenv$env1)),
                        pathname=c("testenv", "testenv$env1"),
                        path=c("", "testenv"),
                        name=c("testenv", "env1"),
                        stringsAsFactors=FALSE)
  envmap = get_env_names(getNamespace("envnames"))
  observed = envmap[envmap$type!="function",]
  expect_equal(observed, expected)
})


# 3.- Cleanup -------------------------------------------------------------
rm(list=ls())

})  # with(globalenv())
