# Created:      05-Oct-2016
# Author:       Daniel Mastropietro
# Description:  Test the envnames package using the testthat package
#

library(testthat)
library(envnames)
context("Find objects")


# NOTE THE NEED TO RUN WITH with(globalenv()) BECAUSE THE OBJECTS ARE DEFINED IN THE GLOBAL ENVIRONMENT
# AND NOT IN THE ENVIRONMENT DEFINED BY testthat.
with(globalenv(), {

# 1.- Prepare the workspace -----------------------------------------------
# IMPORTANT: MAKE SURE THAT THE GLOBAL ENVIRONMENT IS CLEAN, WHICH MAY NOT BE THE CASE WHEN RUNNING
# THE TESTS THROUGH testthat (e.g. using the Check tool in RStudio or calling devtools::test())
rm(list=ls())

env1 <- new.env()
env_of_envs <- new.env()
with(env_of_envs, { env2 <- new.env(); zz <- 6; env2$zz <- 7 })
env1$x <- 3
y = "x"
zz = 44
yy = "zz"
# Array of variable NAMES
v = c("x", "y", "z")
# Array of variable SYMBOLS (which is actually stored as a list by R!)
vquote = c(quote(x), quote(y), quote(z))
# A list of mixed variables
alist = list(z="x", v="env_of_envs$zz", u=2)


# 2.- TEST! ---------------------------------------------------------------
test_that("T1) an object in the global environment is found", {
  # skip("not now")
  # browser()  # This can be used like a breakpoint for debugging. But stil F10 doesn't go to the next line, it will continue to the end of the program!
  expected = "R_GlobalEnv"
  #print(get_fun_calling_chain())
  #print(get_fun_calling())

  observed = obj_find(y)
  expect_equal(observed, expected)
  observed = obj_find(y, envir=globalenv())
  expect_equal(observed, expected)
})

test_that("T2) an object in a package is found", {
  # skip("not now")
  expected = "base"

  observed = obj_find(mean)
  expect_equal(observed, expected)
  observed = obj_find(mean, envir=baseenv())
  expect_equal(observed, expected)
})

test_that("T3) an object inside a user-defined environment is found", {
  # skip("not now")
  expected = "env1"

  observed = obj_find(x)
  expect_equal(observed, expected)
  observed = obj_find(x, envir=env1)
  expect_equal(observed, expected)
})

test_that("T3b) environments are found", {
  # skip("not now")
  # test 1
  expected = "R_GlobalEnv"
  observed = obj_find(env1)
  expect_equal(observed, expected)
  observed = obj_find("env1")
  expect_equal(observed, expected)
  # test 2
  expected = "env_of_envs"
  observed = obj_find(env2)
  expect_equal(observed, expected)
  observed = obj_find("env2", envir=env_of_envs)
  expect_equal(observed, expected)
})

test_that("T4) objects are found when given as text, including if object is given with the full environment path", {
  # skip("not now")
  expected = "env1"

  observed = obj_find("x")
  expect_equal(observed, expected)
  observed = obj_find("env1$x")
  expect_equal(observed, expected)
  observed = obj_find("x", envir=env1)
  expect_equal(observed, expected)
  observed = obj_find("env1$x", envir=env1)
  expect_equal(observed, expected)
})

test_that("T5) an object referenced via a text expression is not found (e.g. alist$z)
           unless the expression is the full environment path to the object (already tested)", {
  # skip("not now")
  expected = NULL
  observed = obj_find("alist$z")
  expect_equal(observed, expected)
})

test_that("T6) an object referenced by an expression (e.g. alist$z where z = \"x\" or v[1] where v[1] = \"x\")
          is found in the correct environment", {
  # skip("not now")
  # test 1
  expected = "env1"
  
  observed = obj_find(alist$z)
  expect_equal(observed, expected)
  observed = obj_find(alist$z, envir=env1)
  expect_equal(observed, expected)
  # test 2
  expected = "env1"
  
  observed = obj_find(v[1])
  expect_equal(observed, expected)
  observed = obj_find(v[1], envir=env1)
  expect_equal(observed, expected)
  # test 2
  expected = "env1"
  
  observed = obj_find(get("z", alist)) # "env1" because get("z", alist) resolves to "x"
  expect_equal(observed, expected)
  observed = obj_find(get("z", alist), envir=env1) # "env1" because get("z", alist) resolves to "x"
  expect_equal(observed, expected)
})

test_that("T7a) if object v exists in the global environment and also in another object (e.g. alist$v)
           obj_find() should search for the (possible) object that is stored in alist$v as name
           (as opposed to searching for 'v' in the global environment)", {
  # skip("not now")
  expected = "env_of_envs"

  observed = obj_find(alist$v)
  expect_equal(observed, expected)
  observed = obj_find(alist$v, envir=env1)
  expect_equal(observed, expected)
})

test_that("T7b) an object referenced by another variable (e.g. y = \"x\") is found (WITHOUT needing to enclose as.name() in quote())", {
  # skip("not now")
  # Case when the symbol referenced by 'y' exists in the calling environment
  expected = c("env_of_envs", "env_of_envs$env2", "R_GlobalEnv")
  observed = obj_find(as.name(yy))
  expect_equal(observed, expected)
  # Case when the symbol referenced by 'y' does NOT exist but exists in another environment
  expected = "env1"
  observed = obj_find(as.name(y))
  expect_equal(observed, expected)
})

test_that("T7c) obj_find() can be called within sapply() on an array of object names or object symbols (which is actually a list)", {
  # Test 1
  expected = list(x="env1", y="R_GlobalEnv", z=NULL)
  observed = sapply(v, obj_find)
  expect_equal(observed, expected)
  # Test 2
  expected = list("env1", "R_GlobalEnv", NULL)
  observed = sapply(vquote, obj_find)
  expect_equal(observed, expected)
})

test_that("T8) looking for an object whose name is the name of another object (e.g. y = \"x\")
           should return the environment of the original object", {
  # skip("not now")
  expected = "R_GlobalEnv"
  
  observed = obj_find(y)
  expect_equal(observed, expected)
  observed = obj_find(y, envir=.GlobalEnv)
  expect_equal(observed, expected)
})

test_that("T9) objects are found in all nested environments
          as well as packages and environments in the search() path when envir=NULL", {
  # skip("not now")
  # test 1
  expected = c("env_of_envs", "env_of_envs$env2", "R_GlobalEnv")
  observed = obj_find("zz")
  expect_equal(observed, expected)
  # test 2: restricted to an environment
  expected = c("env_of_envs", "env2")
  observed = obj_find("zz", envir=env_of_envs)
  expect_equal(observed, expected)
})

test_that("T10a) when obj_find() is called from within an environment, objects are searched and found in all sub-environments
          as well as in packages and environments reachable through the search() path when envir=NULL", {
  # skip("not now")
  # test 1
  expected = c("env_of_envs", "env_of_envs$env2", "R_GlobalEnv")
  observed = with(env_of_envs, obj_find("zz"))
  expect_equal(observed, expected)
  # test 2
  expected = c("env_of_envs", "env_of_envs$env2", "R_GlobalEnv")
  observed = with(env_of_envs, obj_find(zz))
  expect_equal(observed, expected)
  # test 3
  expected = c("env_of_envs$env2")
  observed = with(env_of_envs, obj_find(env2$zz, globalsearch=FALSE))  # We must use globalsearch=FALSE because o.w. env2$zz is looked for in the global environment.
  expect_equal(observed, expected)

  # test 3: test 1 restricted to an environment
  expected = c("env_of_envs", "env2")
  observed = with(env_of_envs, obj_find("zz", envir=env_of_envs))
  expect_equal(observed, expected)
  # test 4: test 2 restricted to an environment
  expected = c("env_of_envs", "env2")
  observed = with(env_of_envs, obj_find(zz, envir=env_of_envs))
  expect_equal(observed, expected)
  # test 5: test 3 restricted to an environment
  expected = c("env_of_envs$env2")
  observed = with(env_of_envs, obj_find(env2$zz, envir=env_of_envs))
  expect_equal(observed, expected)
})

test_that("T10b) when obj_find() is called from within an environment but the object to search is given
          with an absolute path to the actual object as in globalenv()$env1$x, the object is found", {
  # skip("not now")
  # test 1
  expected = c("env1")
  observed = with(env_of_envs, obj_find(globalenv()$env1$x))
  expect_equal(observed, expected)
  
  # test 2
  expected = c("base")
  observed = with(env_of_envs, obj_find(baseenv()$mean))
  expect_equal(observed, expected)
})

test_that("T11) objects given with a full environment path involving globalenv() or as.environment(package) as in globalenv()$x are found", {
  # skip("not now")
  # test 1
  expected = "R_GlobalEnv"
  
  observed = obj_find(globalenv()$y)
  expect_equal(observed, expected)
  observed = obj_find(globalenv()$y, envir=globalenv())
  expect_equal(observed, expected)

  # test 2
  expected = "env1"
  
  observed = obj_find(globalenv()$env1$x)
  expect_equal(observed, expected)
  observed = obj_find(globalenv()$env1$x, envir=.GlobalEnv)
  expect_equal(observed, expected)

  # test 3: using as.environment()
  expected = "R_GlobalEnv"
  observed = obj_find(as.environment(globalenv())$y)
  expect_equal(observed, expected)
  observed = obj_find(as.environment(globalenv())$y, envir=globalenv())
  expect_equal(observed, expected)

  # test 4
  expected = "package:envnames"

  observed = obj_find(as.environment("package:envnames")$environment_name) # This refers to the function environment_name() in package envnames
  expect_equal(observed, expected)
  observed = obj_find(as.environment("package:envnames")$environment_name, envir=as.environment("package:envnames"))
  expect_equal(observed, expected)

  # test 5
  expected = "base"

  observed = obj_find(as.environment("package:base")$mean)
  expect_equal(observed, expected)
  observed = obj_find(as.environment("package:base")$mean, as.environment("package:base"))
  expect_equal(observed, expected)
})


#--------------------- Searches for non-existing objects ------------------
test_that("T90) a non-existing object is not found and no error is raised", {
  # skip("not now")
  # test 1
  expected = NULL
  observed = obj_find(nonexistent)
  expect_equal(observed, expected)
  # test 2
  expected = NULL
  observed = obj_find(env1$nonexistent)
  expect_equal(observed, expected)
})

test_that("T91) using get() on a variable that refers to a non existing objects returns NULL", {
  # skip("not now")
  # test 1
  expected = NULL
  observed = obj_find(get(y))
  expect_equal(observed, expected)
  # test 2
  expected = NULL
  observed = obj_find(get(y, env1))
  expect_equal(observed, expected)
  # test 3
  expected = NULL
  observed = obj_find(get(y), envir=env1)    # NULL because get() is evaluated in the global environment where x does not exist
  expect_equal(observed, expected)
})

test_that("T92) looking for 'NA' returns NULL", {
  expected = NULL
  observed = obj_find(NA)
  expect_equal(observed, expected)
})
#--------------------- Searches for non-existent objects ------------------

# 3.- Cleanup -------------------------------------------------------------
rm(list=ls())

})  # with(globalenv())
