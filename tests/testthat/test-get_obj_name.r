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

# Variable containing a numeric value
x <- 5
# Variable containing a string, which in turn refers to another variable
y = "x"

# Environments and variables defined in them
env1 <- new.env()
env_of_envs <- new.env()
with(env_of_envs, { env2 <- new.env(); zz <- 6; env2$zz <- 7 })
env1$x <- 3

# Functions
fValue = function() { return(7) }
fNull = function() {}
# Function that tests a call to get_obj_name() with n=1
g = function(xx, n=1, eval=FALSE, silent=TRUE) {
  # Define an internal 'v' variable (i.e. the same name as variable 'v' defined in the global environment) to check that
  # the correct 'v' is evaluated when eval=TRUE
  v = c("SHOULD NOT BE SEEN", "SHOULD NOT BE SEEN 2")
  return( get_obj_name(xx, n=n, eval=eval, silent=silent) )
    ## NOTE that n=1 refers to the parent frame of g(), even if the call to get_obj_name() were to be inside a print() function,
    ## which has its own environment... but that environment is NOT counted towards the retrieval of parent frames... (fortunately)
    ## --see Note in the documentation of sys.parent()
}
# Function to test calling get_obj_name() with n=2 (as this function calls g() which in turn calls get_obj_name())
h = function(yy, n=2, eval=FALSE, silent=TRUE) {
  return( g(yy, n=n, eval=eval, silent=silent) )
}

# Arrays and lists (they should be defined AFTER the definition of functions because these are referenced now)
v = c("x", "y", "z")
alist = list(z="x", v=3, u=2)
alist_quoted = c(quote(x), quote(y), quote(nonexistent))
alist_expressions = list(quote(fNull()), v[1])


# 2.- TEST! ---------------------------------------------------------------
# Basic tests ----------------------------------------------------------
test_that("T1) the name of a variable is returned as a string,
          including the case when the variable is a user-defined environment", {
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

test_that("T2) the name of a variable given as a string is returned as that string unchanged", {
  expected = "x"
  observed = get_obj_name("x")
  expect_equal(observed, expected)
})

test_that("T3) the name of a string or expression containing quotes still contains the quotes", {
  expected = "as.environment(\\\"package:stats\\\")"    # Note that the first '\' masks the '\' and the second '\' masks the quote
  observed = get_obj_name("as.environment(\"package:stats\")")
  expect_equal(observed, expected)

  expected = "as.environment(\"package:stats\")"
  observed = get_obj_name(as.environment("package:stats"))
  expect_equal(observed, expected)

  expected = "as.symbol(\"x\")"
  observed = get_obj_name(as.symbol("x"))
  expect_equal(observed, expected)

  # Same thing with quotes at the beginning and end of the string (they are preserved)
  expected = "\\\"this contains begin and end quotes\\\""
  observed = get_obj_name("\"this contains begin and end quotes\"")
  expect_equal(observed, expected)
})

test_that("T3) numeric values are returned as characters", {
  expect_equal(get_obj_name(3), "3")
})

test_that("T4) the name of objects inside environments are correctly returned", {
  expected = "x"
  observed = with(env1, get_obj_name(x))
  expect_equal(observed, expected)

  expected = "env1$x"
  observed = get_obj_name(env1$x)
  expect_equal(observed, expected)
})

test_that("T5) the name of a function is correctly returned", {
  expected = "fValue()"
  observed = get_obj_name(fValue())
  expect_equal(observed, expected)
})
# Basic tests ----------------------------------------------------------


# Environments ----------------------------------------------------------
test_that("T11) the name of user-defined environments when given through their variable names are correctly returned", {
  expected = "env1"
  observed = get_obj_name(env1)
  expect_equal(observed, expected)

  expected = "env_of_envs$env2"
  observed = get_obj_name(env_of_envs$env2)
  expect_equal(observed, expected)
})

test_that("T12a) (eval=TRUE) the name of built-in and package environments are returned as just their names
          (e.g. \"R_GlobalEnv\" as opposed to \"<environment R_GlobalEnv>\")", {
  expected = "package:stats"
  observed = get_obj_name(as.environment("package:stats"), eval=TRUE)
  expect_equal(observed, expected)

  expected = "R_GlobalEnv"
  observed = get_obj_name(as.environment(globalenv()), eval=TRUE)
  expect_equal(observed, expected)

  expected = "R_EmptyEnv"
  observed = get_obj_name(as.environment(emptyenv()), eval=TRUE)
  expect_equal(observed, expected)

  expected = "base"
  observed = get_obj_name(as.environment(baseenv()), eval=TRUE)
  expect_equal(observed, expected)
})

test_that("T12b) (eval=TRUE) the name of unnamed environments
          (i.e. user-defined environments or function execution environments)
          are correclty returned", {
  # NOTE: All these tests only work when run inside the test_that() function,
  # as o.w. the result is e.g. "R_GlobalEnv" when the tests are run from the global environment.

  # Function that is used to test get_obj_name() when an environment is given as parameter
  # and traced back to the function's parent generations.
  f <- function(env, n=1) {
    # Parameter n should be > 1 in order to test get_obj_name() as we want to test it (i.e. by checking that
    # the initial loop done there on nback doesn't fail when parameter 'obj' is an "unnamed" environment
    # as mentioned above, e.g. <environment: 0x000000001b485670>)
    return( get_obj_name(env, n=n, eval=TRUE) )
  }

  # User-defined environment (given by its address as we do by using 'globalenv()$' in front of the environment name)
  expect_equal(get_obj_name(globalenv()$env1, eval=TRUE), "env1")

  # Function execution environment
  expect_equal(get_obj_name(environment(), eval=TRUE), "base$eval")

  # Test the case where we retrieve the name of an unnamed environment deep down in the calling chain
  # (so that the loop performed on nback in get_obj_name is actually tested on function execution environments!
  # (which are given in the form <environment: 0x000000001b485670>))
  expect_equal(f(environment(), n=1), "base$eval")
  expect_equal(f(environment(), n=2), "base$eval")
  expect_equal(f(environment(), n=3), "testthat$test_code")
})
# Environments ----------------------------------------------------------


# eval=TRUE ----------------------------------------------------------
test_that("T21) expressions passed to get_obj_name() are evaluated when eval=TRUE,
          and the result associated to that value is returned as a string", {
  # 0) Equivalence/Non-equivalence with deparse() and deparse(substitute()) depending on the type of the input parameter
  # (Ref: Entry on 18-Sep-2017 on Project Tasks Excel)
  f <- function(x) {
    obj_name = get_obj_name(x, n=1, eval=TRUE)  # This gives the value of parameter x when called from the outside world via e.g. f(3)
    if (is.numeric(x)) {
      expect_equal(obj_name, deparse(x))
      expect_equal(obj_name, deparse(substitute(x)))
    } else {
      expect_equal(paste('"', obj_name, '\"', sep=""), deparse(x))  # i.e. obj_name = "test" and deparse(x) = "\"test\""
      expect_equal(paste('"', obj_name, '\"', sep=""), deparse(substitute(x)))  # i.e. obj_name = "test" and deparse(x) = "\"test\""
    }

    # Non-equivalence with evaluating x in the parent frame
    expect_error( expect_equal(obj_name, deparse(substitute(x, parent.frame(1)))) )
  }
  f(3)
  f("char-value")

  # 1) Name of an expression returning a numeric value is the numeric value given as a string
  expected = as.character(x) # as.character(x) converts the value of 'x' to a string (e.g. "5")
  observed = get_obj_name(get("x"), eval=TRUE)  # Note: get("x") returns the value of 'x' (e.g. 5)
  expect_equal(observed, expected)
  # and this is equivalent to using 'x' directly instead of get("x") because get("x") returns the value of 'x' as does the expression 'x', which returns the value of variable 'x'
  observed = get_obj_name(x, eval=TRUE)
  expect_equal(observed, expected)

  # 2) Name of an expression returning a string is that string
  expected = y
  observed = get_obj_name(get("y"), eval=TRUE)  # Note: get("x") returns the value of 'x' which in this case is 5
  expect_equal(observed, expected)
  # and this is equivalent to using 'x' directly instead of get("x") because get("x") returns the value of 'x' as does the expression 'x', which returns the value of variable 'x'
  observed = get_obj_name(y, eval=TRUE)
  expect_equal(observed, expected)

  # 3) Name of other expressions / functions
  expected = "x"
  observed = get_obj_name(as.symbol("x"), eval=TRUE)  # Note: as.symbol("x") returns 'x', so the name associated to 'x' is "x"
  expect_equal(observed, expected)

  # Function returning a numeric value
  expected = as.character(fValue())
  observed = get_obj_name(fValue(), eval=TRUE)
  expect_equal(observed, expected)

  # Function returning NULL
  # (note that the result of get_obj_name() in this case is NOT as.character(fNull())
  # --as is the case with get_obj_name(fValue, eval=TRUE) where the result is as.character(fValue())--
  # i.e. it is not as.character(NULL) which resolves to 'as.character(0)')
  expected = NULL
  observed = get_obj_name(fNull(), eval=TRUE)
  expect_equal(observed, expected)
})
# eval=TRUE ----------------------------------------------------------


# Tests on n > 0 ----------------------------------------------------------
test_that("T21) Object names are correcly returned when eval=FALSE at different levels n", {
  silent = TRUE

  expect_equal( g(globalenv()$y), "globalenv()$y" )
  expect_equal( g(v[2]), "v[2]" )
  expect_equal( g(env1), "env1" )
  expect_equal( g(env1$envx), "env1$envx" )
  expect_equal( g("x"), "x" )
  expect_equal( g(a()), "a()" )  # It doesn't matter if a() exists or not

  # If we call g() with n=0 we should get the name of the first argument of g()
  expect_equal( g(v[2], n=0), names(as.list(g))[1] )

  # Same for h(): if we call h() with n=1 we should get the name of the first argument of h()
  expect_equal( h(v[2], n=1, silent=silent), names(as.list(h))[1] )    # names(as.list(h)) returns the input parameters of function h()
  # *** But if we call h() with n=0 we should get the name of the first argument of g() ***
  expect_equal( h(v[2], n=0, silent=silent), names(as.list(g))[1] )

  # Test more complicated objects as vectors or functions
  expect_equal( h(v, n=0, silent=silent), names(as.list(g))[1] )
  expect_equal( h(v, n=1, silent=silent), names(as.list(h))[1] )
  expect_equal( h(fNull(), n=0, silent=silent), names(as.list(g))[1] )
  expect_equal( h(fNull, n=0, silent=silent), names(as.list(g))[1] )
  expect_equal( h(fNull, n=1, silent=silent), names(as.list(h))[1] )
})

test_that("T22) The value returned when eval=TRUE is the same as the return value of deparse() (converted to single string) and doesn't change with n.
          In fact, the value of the object that is passed through different functions is always the same inside each function!", {
  silent = TRUE

  expect_equal( g(v[2], n=1, eval=TRUE, silent=silent), v[2] )
  expect_equal( h(v[2], n=2, eval=TRUE, silent=silent), v[2] )
  expect_equal( h(v[2], n=1, eval=TRUE, silent=silent), v[2] )
  expect_equal( h(v[2], n=0, eval=TRUE, silent=silent), v[2] )

  # When the object passed is a more complicated object (vector, function, etc.)
  expect_equal( h(v, n=0, eval=TRUE, silent=silent), deparse(v) )
  expect_equal( h(v, n=1, eval=TRUE, silent=silent), deparse(v) )
  expect_equal( h(fNull(), n=0, eval=TRUE, silent=silent), NULL )
  expect_equal( h(fNull, n=0, eval=TRUE, silent=silent), paste(deparse(fNull), collapse = "\n"))
  expect_equal( h(fNull, n=1, eval=TRUE, silent=silent), paste(deparse(fNull), collapse = "\n"))
})
# Tests on n > 0 ----------------------------------------------------------


# arrays, lists, and sapply() with eval=TRUE -------------------------------
test_that("T31) the function works on arrays and lists and using sapply()", {
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

  #--- List of expressions (e.g. functions, array elements, etc.)
  # On one element as a list element
  expected = "fNull()"
  observed = get_obj_name(alist_expressions[[1]], eval=TRUE)
  expect_equal(observed, expected)
  expected = v[1]
  observed = get_obj_name(alist_expressions[[2]], eval=TRUE)
  expect_equal(observed, expected)
})

test_that("T32) sapply() run on a list of expressions", {
  expected = c("fNull()", eval(alist_expressions[[2]]))
  observed = sapply(alist_expressions, get_obj_name, eval=TRUE)
  expect_equal(observed, expected)
})
# arrays, lists, and sapply() with eval=TRUE -------------------------------


# Extreme cases -----------------------------------------------------------
test_that("T91) the name of non-existent objects or functions are NULL when eval=TRUE", {
  expected = NULL
  observed = get_obj_name(env1$nonexistent, eval=TRUE)
  expect_equal(observed, expected)

  expected = NULL
  observed = get_obj_name(nonexistent, eval=TRUE)
  expect_equal(observed, expected)

  expected = NULL
  observed = get_obj_name(fNonExistent(), eval=TRUE)
  expect_equal(observed, expected)
})

test_that("T92) the name of non-existent objects are the strings containing the non-existent object name", {
  expected = "env1$nonexistent"
  observed = get_obj_name(env1$nonexistent)
  expect_equal(observed, expected)

  expected = "nonexistent"
  observed = get_obj_name(nonexistent)
  expect_equal(observed, expected)

  expected = "fNonExistent()"
  observed = get_obj_name(fNonExistent())
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

test_that("objects deparsing to more than 60 characters do not cause an error", {
  # related to #12
  expect_no_error(
    get_obj_name(data.frame(x = 1:8, y = c("a", "b", "c", "d", "e", "f", "g", "h")), n = 1)
  )
})


# 3.- Cleanup -------------------------------------------------------------
rm(list=ls())

})  # with(globalenv())
