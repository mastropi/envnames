# Created:      04-Sep-2017
# Author:       Daniel Mastropietro
# Description:  Test the envnames package using the testthat package
#

library(testthat)
library(envnames)
context("get_obj_value")

# NOTE THE NEED TO RUN WITH with(globalenv()) BECAUSE THE OBJECTS ARE DEFINED IN THE GLOBAL ENVIRONMENT
# AND NOT IN THE ENVIRONMENT DEFINED BY testthat.
with(globalenv(), {

# 1.- Prepare the workspace -----------------------------------------------
# IMPORTANT: MAKE SURE THAT THE GLOBAL ENVIRONMENT IS CLEAN, WHICH MAY NOT BE THE CASE WHEN RUNNING
# THE TESTS THROUGH testthat (e.g. using the Check tool in RStudio or calling devtools::test())
rm(list=ls())

x <- 5
y <- 3
z <- 10
df <- data.frame(y=y, z=z)
env1 <- new.env()
with(env1, z <- 15)
X <- data.frame(x=c(1,2,3), y=c(2,2,-2))

# Define functions that are called to show the behaviour of get_obj_value()
h = function(x) {
  # Get the value of parameter 'x' n levels up, i.e. the value of the parameter that led to the current parameter x
  # in the environment that is n levels up in the function calling chain.
  xval_objvalue = get_obj_value(x, n=1, silent=TRUE)
  xval_deparse = deparse(x)
  xval_parent = eval.parent(x, n=1)
  # If we need to show the parent environments... (when debugging this function)
  #print(lapply(sys.frames(), ls))   # Lists the content of each active frame (there is only one though... I thought there would be also the frame corresponding to g())
  #print(lapply(sys.frames(), environment_name))  # This gives the name of the environments associated to the frames!! NICE! :)
  #env = parent.frame(n=1)
  #print("env inside h():")
  #print(env)
  #browser()
  #env_name = environment_name(env, envmap=get_env_names())
  #cat("parent environment: ", env_name, "\n")  # The parent environment should be function "g"
  #print(get_fun_calling_chain(showParameters=FALSE))
  return( list(xval_objvalue=xval_objvalue, xval_deparse=xval_deparse, xval_parent=xval_parent) )
}

g <- function(y) {
  # Create a variable with the same name as the parameter of function h() so that we can show that
  # get_obj_value(x) returns the value of y in this function g() and NOT the value of x
  # i.e. showing that the get_obj_value(x) function first looks for the NAME of variable x
  # in one environment above, and THEN evaluates that variable in that environment.
  x = 2
  return( h(y)$xval_objvalue )
}


# 2.- TEST! ---------------------------------------------------------------
test_that("T1) the result of using an object reference is the same as eval.parent(object, n=n)", {
  # skip("not now")
  # browser()
  result = h(y)
  expect_equal(result$xval_objvalue, result$xval_parent)
  result = h(x)
  expect_equal(result$xval_objvalue, result$xval_parent)
  result = h(X)
  expect_equal(result$xval_objvalue, result$xval_parent)
})

test_that("T2) the result of using a string is the same as the VALUE of the result of eval.parent(object, n=n)
          when the string given as parameter to get_obj_value() is not already the value (e.g. '3')", {
  # skip("not now")
  # browser()
  result = h("x")
  expect_equal(result$xval_objvalue, eval(parse(text=result$xval_parent)))
  result = h("3")
  expect_equal(result$xval_objvalue, result$xval_parent)
})

test_that("T3) parameters are parsed to their values either when given as objects or as strings from any environment.
          The string case only works when the environment name is NOT explicitly mentioned using the '$' notation", {
  # skip("not now")
  # browser()
  expect_equal(get_obj_value(y), 3)
  expect_equal(get_obj_value("y"), 3)
  expect_equal(get_obj_value(3), 3)
  expect_equal(get_obj_value("3"), "3")
  expect_equal(with(env1, get_obj_value(z)), 15)
  expect_equal(with(env1, get_obj_value("z")), 15)
  expect_equal(with(globalenv(), get_obj_value(z)), 10)
  expect_equal(with(globalenv(), get_obj_value("z")), 10)
  # Using the '$' notation
  expect_equal(env1$z, 15)
  expect_equal(globalenv()$z, 10)
  # When explicitly mentioning the environment name with '$', the object value is NOT returned.
  expect_equal(get_obj_value("env1$z"), "env1$z")
  expect_equal(get_obj_value("globalenv()$z"), "globalenv()$z")
})

test_that("T4) parameters are parsed to their values by matching the object's name with its value in the corresponding environment", {
  # skip("not now")
  # browser()
  expect_equal(g(z), 10)
  expect_equal(g("z"), "z")
  expect_equal(g(env1$z), 15)
  expect_equal(with(env1, g("z")), "z")
})


# 3.- Cleanup -------------------------------------------------------------
rm(list=ls())

})  # with(globalenv())
