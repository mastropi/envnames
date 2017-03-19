# Created:      05-Oct-2016
# Author:       Daniel Mastropietro
# Description:  Test the envnames package using the testthat package
#

library(testthat)
library(envnames)
context("Get the name of objects")

# NOTE THE NEED TO RUN WITH with(globalenv()) BECAUSE THE OBJECTS ARE DEFINED IN THE GLOBAL ENVIRONMENT
# AND NOT IN THE ENVIRONMENT DEFINED BY testthat.
with(globalenv(), {

# 1.- Prepare the workspace -----------------------------------------------
# IMPORTANT: MAKE SURE THAT THE GLOBAL ENVIRONMENT IS CLEAN, WHICH MAY NOT BE THE CASE WHEN RUNNING
# THE TESTS THROUGH testthat (e.g. using the Check tool in RStudio or calling devtools::test())
rm(list=ls())

x <- 5
env1 <- new.env()
env_of_envs <- new.env()
with(env_of_envs, { env2 <- new.env(); zz <- 6; env2$zz <- 7 })
env1$x <- 3
y = "x"

# Functions
f = function() {}
# Function to test call to get_obj_name() with n=1
g = function(xx, n=1, eval=FALSE, silent=TRUE) {
  # Define an internal 'v' variable (i.e. the same name as variable 'v' defined in the global environment) to check that
  # the correct 'v' is evaluated when eval=TRUE
  v = c("SHOULD NOT BE SEEN", "SHOULD NOT BE SEEN 2")
  return( get_obj_name(xx, n=n, eval=eval, silent=silent) )
    ## NOTE that n=1 refers to the parent frame of g(), despite the fact that get_obj_name() is inside a print() function,
    ## which has its own environment... but that environment is NOT counted towards the retrieval of parent frames... (fortunately)
    ## --see Note in the documentation of sys.parent()
}
# Function to test call to get_obj_name() with n=2 (as this function calls g() which calls get_obj_name())
h = function(yy, n=2, eval=FALSE, silent=TRUE) {
  return( g(yy, n=n, eval=eval, silent=silent) )
}

# Arrays and lists
v = c("x", "y", "z")
alist = list(z="x", v=3, u=2)
alist_quoted = c(quote(x), quote(y), quote(nonexistent))
alist_complex = list(quote(f()), v[1])


# 2.- TEST! ---------------------------------------------------------------
test_that("T1) the name of a simple variable name is returned, including the case when the variable is a user-defined environment", {
  # skip("not now")
  # browser()  # This can be used like a breakpoint for debugging. But stil F10 doesn't go to the next line, it will continue to the end of the program!
  # test 1
  expected = "x"
  observed = get_obj_name(x)
  expect_equal(observed, expected)
  # test2: the variable is an environment
  expected = "env1"
  observed = get_obj_name(env1)
  expect_equal(observed, expected)
})

test_that("T2) the name of a simple variable name given as a string is returned", {
  expected = "x"
  observed = get_obj_name("x")
  expect_equal(observed, expected)
})

test_that("T3) the name of a variable with quotes inside, still contains the quotes", {
  expected = "as.environment(\\\"package:stats\\\")"
  observed = get_obj_name("as.environment(\"package:stats\")")
  expect_equal(observed, expected)
})

test_that("T4) the name of global and package environments are correctly returned", {
  # skip("This test should pass in new version")
  expected = "package:stats"
  observed = get_obj_name(as.environment("package:stats"), eval=TRUE)
  expect_equal(observed, expected)
  expected = "R_GlobalEnv"
  observed = get_obj_name(as.environment(globalenv()), eval=TRUE)
  expect_equal(observed, expected)
})

test_that("T5) functions like get(), as.symbol(), etc. are taken into account before resolving the name", {
  # skip("This test should pass in new version")
  expected = as.character(x)
  observed = get_obj_name(get("x"), eval=TRUE)
  expect_equal(observed, expected)
  expected = "x"
  observed = get_obj_name(as.symbol("x"), eval=TRUE)
  expect_equal(observed, expected)
})

test_that("T6) quotes at the beginning and end of the object are NOT removed.
          NOTE that the result of this CHANGED in the new version of get_obj_name(). Is this ok?", {
  # skip("this behaviour should change")
  expected = "\\\"this contains begin and end quotes\\\""
  observed = get_obj_name("\"this contains begin and end quotes\"", eval=TRUE)
  expect_equal(observed, expected)
})

### After the simplified version of get_obj_name()
test_that("T11) numeric values are returned as characters", {
  expect_equal(get_obj_name(3), "3")
})

test_that("T12) the name of user-defined environments are correctly returned", {
  # skip("This test should pass in new version")
  expected = "env1"
  observed = get_obj_name(env1)
  expect_equal(observed, expected)

  expected = "env_of_envs$env2"
  observed = get_obj_name(env_of_envs$env2)
  expect_equal(observed, expected)
})

test_that("T13) the name of objects inside environments are correctly returned", {
  # skip("This test should pass in new version")
  expected = "x"
  observed = with(env1, get_obj_name(x))
  expect_equal(observed, expected)
  
  expected = "env1$x"
  observed = get_obj_name(env1$x)
  expect_equal(observed, expected)
})

test_that("T14) the name of a function is correctly returned", {
  expected = "f()"
  observed = get_obj_name(f())
  expect_equal(observed, expected)
})

test_that("T15) the function works on arrays and lists and using sapply()", {
  skip("This test should pass in new version, but first check how we want lists to behave")
  # Arrays ------------------------------------------------------------------
  # On one element
  expected = as.character(v[1])
  observed = get_obj_name(v[1], eval=TRUE)
  expect_equal(observed, expected)

  # Out of range element
  expected = "NA_character_"
  observed = get_obj_name(v[5], eval=TRUE)
  expect_equal(observed, expected)
  
  # sapply()
  expected = as.character(v)
  names(expected) = v
  observed = sapply(v, get_obj_name, eval=TRUE)
  expect_equal(observed, expected)

  # Lists -------------------------------------------------------------------
  #--- Named list of strings and values
  # On one element retrieved as a vector (this returns a string representing the structure of the list, since alist[1] is a list)
  expected = deparse(alist[1])
  observed = get_obj_name(alist[1], eval=TRUE)
  expect_equal(observed, expected)
  # On one element retrieved as a list element (this returns the name of the object stored in the list element)
  expected = as.character(alist[[1]])  # Note that deparse(alist[1]) gives "\"x\"", and as.character() returns "x"
  observed = get_obj_name(alist[[1]], eval=TRUE)
  expect_equal(observed, expected)
  
  # Out of range element as a vector
  expected = deparse(alist[5])
  observed = get_obj_name(alist[5], eval=TRUE)  # Note that calling alist[[5]] gives "out of bounds" error
  expect_equal(observed, expected)
  # Out of range element as a list element
  expected = NULL   # (2017/03/17) get_obj_name() returns "alist[[5]]"... I think it should return NULL...
  observed = get_obj_name(alist[[5]], eval=TRUE)  # Note that calling alist[[5]] gives "out of bounds" error
  expect_equal(observed, expected)
  
  # sapply()
  expected = as.character(alist)
  names(expected) = names(alist)
  observed = sapply(alist, get_obj_name, eval=TRUE)
  expect_equal(observed, expected)

  #--- List of quoted objects
  # On one element as a vector
  expected = deparse(alist_quoted[1])  # This is "list(x)" NOT "x"
  observed = get_obj_name(alist_quoted[1], eval=TRUE)
  expect_equal(observed, expected)
  # On one element as a list element
  expected = "x"
  observed = get_obj_name(alist_quoted[[1]], eval=TRUE)
  expect_equal(observed, expected)
  
  # sapply()
  expected = as.character(alist_quoted)
  observed = sapply(alist_quoted, get_obj_name, eval=TRUE)
  expect_equal(observed, expected)

  #--- List of complex objects (e.g. quote(f()), v[1])
  # On one element as a list element
  expected = "f()"
  observed = get_obj_name(alist_complex[[1]], eval=TRUE)
  expect_equal(observed, expected)
  expected = v[1]
  observed = get_obj_name(alist_complex[[2]], eval=TRUE)
  expect_equal(observed, expected)
})

test_that("sapply() on a list of complex objects... those objects are NOT evaluated though... can we fix that??", {
  skip("(2017/03/17) DOES NOT PASS as it returns \"f()\" and \"v[1]\". I don't know if this can easily be solved...")
  expected = list(NULL, v[1])   # The first element is the evaluation of f() which gives NULL 
  observed = sapply(alist_complex, get_obj_name, eval=TRUE)
  expect_equal(observed, expected)
})


# Tests on n > 0 ----------------------------------------------------------
test_that("T21) Object names are correcly returned when eval=FALSE at different levels n", {
  expect_equal( g(globalenv()$y), "globalenv()$y" )
  expect_equal( g(v[2]), "v[2]" )
  expect_equal( g(env1), "env1" )
  expect_equal( g(env1$envx), "env1$envx" )
  expect_equal( g("x"), "x" )
  expect_equal( g(a()), "a()" )  # It doesn't matter if a() exists or not

  # If we call g() with n=0 we should get the name of the first argument of g()
  expect_equal( g(v[2], n=0), names(as.list(g))[1] )

  # Same for h(): if we call h() with n=1 we should get the name of the first argument of h()
  expect_equal( h(v[2], n=1, silent=FALSE), names(as.list(h))[1] )    # names(as.list(h)) returns the input parameters of function h()
  # *** But if we call h() with n=0 we should get the name of the first argument of g() ***
  expect_equal( h(v[2], n=0, silent=FALSE), names(as.list(g))[1] )

  # Test more complicated objects as vectors or functions
  expect_equal( h(v, n=0, silent=FALSE), names(as.list(g))[1] )
  expect_equal( h(v, n=1, silent=FALSE), names(as.list(h))[1] )
  expect_equal( h(f(), n=0, silent=FALSE), names(as.list(g))[1] )
  expect_equal( h(f, n=0, silent=FALSE), names(as.list(g))[1] )
  expect_equal( h(f, n=1, silent=FALSE), names(as.list(h))[1] )
})

test_that("T22) Object names are correcly returned at levels n>0 when eval=TRUE and it's the same regardless of the value of n.
          In fact, the value of the object that is passed through different functions is always the same inside each function!", {
  expect_equal( g(v[2], n=1, eval=TRUE, silent=FALSE), v[2] )
  expect_equal( h(v[2], n=2, eval=TRUE, silent=FALSE), v[2] )
  expect_equal( h(v[2], n=1, eval=TRUE, silent=FALSE), v[2] )
  expect_equal( h(v[2], n=0, eval=TRUE, silent=FALSE), v[2] )
  
  # When the object passed is a more complicated object (vector, function, etc.)
  expect_equal( h(v, n=0, eval=TRUE, silent=FALSE), deparse(v) )
  expect_equal( h(v, n=1, eval=TRUE, silent=FALSE), deparse(v) )
  expect_equal( h(f(), n=0, eval=TRUE, silent=FALSE), NULL )      # The return value of function f() is NULL
  expect_equal( h(f, n=0, eval=TRUE, silent=FALSE), deparse(f) )
  expect_equal( h(f, n=1, eval=TRUE, silent=FALSE), deparse(f) )
})
# Tests on n > 0 ----------------------------------------------------------


# Extreme cases -----------------------------------------------------------
test_that("T91) the name of non-existent objects are NULL when eval=TRUE", {
  expected = NULL
  observed = get_obj_name(env1$nonexistent, eval=TRUE)
  expect_equal(observed, expected)

  expected = NULL
  observed = get_obj_name(nonexistent, eval=TRUE)
  expect_equal(observed, expected)
})

test_that("T92) the name of non-existent objects are the strings containing the non-existent object", {
  expected = "env1$nonexistent"
  observed = get_obj_name(env1$nonexistent, eval=FALSE)
  expect_equal(observed, expected)
  
  expected = "nonexistent"
  observed = get_obj_name(nonexistent, eval=FALSE)
  expect_equal(observed, expected)
  
  # Now check that get() fails when we input the output of get_obj_name() on non-existent objects
  expect_error( get(get_obj_name(nonexistent)), "object 'nonexistent' not found")
})

test_that("T93) the name of NULL is NULL, and the name of NA is \"NA\", regardless of parameter 'eval'", {
  expect_equal( get_obj_name(NULL, eval=FALSE), NULL )
  expect_equal( get_obj_name(NULL, eval=TRUE), NULL )
  expect_equal( get_obj_name(NA, eval=FALSE), "NA" )
  expect_equal( get_obj_name(NA, eval=TRUE), "NA" )
})
# Extreme cases -----------------------------------------------------------


# 3.- Cleanup -------------------------------------------------------------
rm(list=ls())

})  # with(globalenv())
