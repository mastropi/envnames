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

x <- 5  # This variable MUST exist in the global environment if we don't want the call to evalq()
        # in hCompareGetObjValueWithEvalAndEvalq() to fai (with the error that x does not exist
        # in the global environment when hCompareGetObjValueWithEvalAndEvalq() is called from the global environment)
y <- 3
z <- 10
df <- data.frame(y=y, z=z)
env1 <- new.env()
with(env1, z <- 15)
X <- data.frame(x=c(1,2,3), y=c(2,2,-2))

# Define functions that are called to show the behaviour of get_obj_value()
hCompareGetObjValueWithEvalAndEvalq = function(x) {
  # Get the value of parameter 'x' n levels up, i.e. the value of the parameter that led to the current parameter x
  # in the environment that is n levels up in the function calling chain.
  xval_objvalue = envnames:::get_obj_value(x, n=1, silent=TRUE)
  xval_eval = eval(x, parent.frame(n=1))
  xval_evalq = evalq(x, parent.frame(n=1))
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
  
  # Return the different values to be compared in a list
  # Note that the first value is simply 'x', i.e. the value of x WITHOUT calling get_obj_value()
  # nor eval() nor evalq(). This is used to prove that the value of variables in all parent generations
  # is always the same, and equal to the value of the variable in the calling function (i.e. the value of 'x')
  return( list(xval=x, xval_objvalue=xval_objvalue, xval_eval=xval_eval, xval_evalq=xval_evalq) )
}

gFunctionCallingH <- function(y) {
  # Create a variable with the same name as the parameter of function h() so that we can show that
  # get_obj_value(x) returns the value of y in this function g() and NOT the value of x
  # i.e. showing that the get_obj_value(x) function first looks for the NAME of variable x
  # in one environment above, and THEN evaluates that variable in that environment.
  x = 2
  return( hCompareGetObjValueWithEvalAndEvalq(y) )
}


# 2.- TEST! ---------------------------------------------------------------
test_that("T1) All the variables in the parent generations leading to the 
          object queried by get_obj_value() have the SAME value, which is equal to the result of
          the eval() function, which in turn is equal to the value of the queried object
          at the calling function, which in turn is equal to the value passed to the function
          in the first place.", {
  # skip("not now")
  # browser()
  # NOTE: The expect_equal() calls in each test below follow the order of the description of the test
  # given as first argument to the test_that() function.
  result = gFunctionCallingH(y)
  expect_equal(result$xval_objvalue, result$xval_eval)
  expect_equal(result$xval_objvalue, result$xval)
  expect_equal(result$xval_objvalue, y)
  
  result = gFunctionCallingH(x)
  expect_equal(result$xval_objvalue, result$xval_eval)
  expect_equal(result$xval_objvalue, result$xval)
  expect_equal(result$xval_objvalue, x)

  result = gFunctionCallingH(X)
  expect_equal(result$xval_objvalue, result$xval_eval)
  expect_equal(result$xval_objvalue, result$xval)
  expect_equal(result$xval_objvalue, X)
})

test_that("T11) the result of passing the object queried by get_obj_value() as a string (e.g. \"x\")
          is the same as the result of passing the object as a symbol (e.g. x)
          as long as the string does NOT correspond to a number,
          i.e. as long as the string is the name of an existing object", {
  # skip("not now")
  # browser()
  result_as_string = hCompareGetObjValueWithEvalAndEvalq("x")
  result_as_symbol = hCompareGetObjValueWithEvalAndEvalq(x)
  expect_equal(result_as_string$xval_objvalue, result_as_symbol$xval_objvalue)

  result_as_string = hCompareGetObjValueWithEvalAndEvalq("y")
  result_as_symbol = hCompareGetObjValueWithEvalAndEvalq(y)
  expect_equal(result_as_string$xval_objvalue, result_as_symbol$xval_objvalue)

  # Case when the string represents a number and the result of passing the value as a string
  # or as the number differ --> we expect a failure of the expect_equal() call.
  result_as_string = hCompareGetObjValueWithEvalAndEvalq("3")
  result_as_symbol = hCompareGetObjValueWithEvalAndEvalq(3)
  expect_failure( expect_equal(result_as_string$xval_objvalue, result_as_symbol$xval_objvalue),
                  "Types not compatible: character is not double")
})

test_that("T12) the result of passing the object queried by get_obj_value() as a string (e.g. \"x\")
          is DIFFERENT to the result of evaluating that string with eval() (which yields the string itself)", {
  # skip("not now")
  # browser()
  result_as_string = hCompareGetObjValueWithEvalAndEvalq("x")
  expect_failure( expect_equal(result_as_string$xval_objvalue, result_as_string$xval_eval),
                  "Types not compatible: double is not character")
})

test_that("T13) the value of a string representing a number is the string itself", {
  # skip("not now")
  # browser()
  z = "3"
  result = hCompareGetObjValueWithEvalAndEvalq(z)
  expect_equal(result$xval_objvalue, result$xval_eval)
  expect_equal(result$xval_objvalue, result$xval)
  expect_equal(result$xval_objvalue, z)
})

test_that("T14) the value of a string representing a non existing object is the string itself", {
  # skip("not now")
  # browser()
  z = "nonexistent"
  result = hCompareGetObjValueWithEvalAndEvalq(z)
  expect_equal(result$xval_objvalue, result$xval_eval)
  expect_equal(result$xval_objvalue, result$xval)
  expect_equal(result$xval_objvalue, z)
})

test_that("T21) objects are parsed to their values either when given as symbols or as strings from any environment.
          The string case only works when the environment name is NOT explicitly mentioned using the '$' notation.", {
  # skip("not now")
  # browser()
  expect_equal(envnames:::get_obj_value(y), y)
  expect_equal(envnames:::get_obj_value("y"), y)
  expect_equal(envnames:::get_obj_value(3), 3)
  expect_equal(envnames:::get_obj_value("3"), "3")
  expect_equal(with(env1, envnames:::get_obj_value(z)), env1$z)
  expect_equal(with(env1, envnames:::get_obj_value("z")), env1$z)
  expect_equal(with(globalenv(), envnames:::get_obj_value(z)), z)
  expect_equal(with(globalenv(), envnames:::get_obj_value("z")), z)
  # Using the '$' notation:
  # when explicitly mentioning the environment name with '$', the object value is NOT returned
  # Note that this is the same value returned by eval() since eval() treats every string as a string.
  expect_equal(envnames:::get_obj_value("env1$z"), "env1$z")
  expect_equal(envnames:::get_obj_value("globalenv()$z"), "globalenv()$z")
})

test_that("T31) the result of get_obj_value() is NOT the same as evalq() if a variable
          with the same name exists in the parent generation and has a different value
          than the value of the variable at the calling environment.
          To clarify, this is the case of 'x' which is the name of the parameter
          of function hCompareGetObjValueWithEvalAndEvalq().", {
  # skip("not now")
  # browser()
  result = hCompareGetObjValueWithEvalAndEvalq(z)
  #cat("objvalue:", result$xval_objvalue, "\n") # 10, value of parameter x in hCompareGetObjValueWithEvalAndEvalq()
  #cat("evalq:", result$xval_evalq, "\n") # 5, value of x in the global environment
  expect_failure( expect_equal(result$xval_objvalue, result$xval_evalq) )

  result = gFunctionCallingH(z)
  #cat("objvalue:", result$xval_objvalue, "\n") # 10, value of parameter x in hCompareGetObjValueWithEvalAndEvalq()
  #cat("evalq:", result$xval_evalq, "\n") # 2, value of x in gFunctionCallingH()
  expect_failure( expect_equal(result$xval_objvalue, result$xval_evalq) )
})


# 3.- Cleanup -------------------------------------------------------------
rm(list=ls())

})  # with(globalenv())
