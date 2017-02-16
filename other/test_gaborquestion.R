# 21-Jan-2017
# These tests came up after a question by Gabor Grothendieck, the person who raised
# the concern of being able to retrieve the name of user-defined environments in R
# to Brian Ripley back in Jul-2010.
# He asked me: Why inside a function environment_name(environment()) returns NULL?
# I presume he wanted environment_name(environment) to return the environment where
# the function is defined and the function name? (e.g. env$f if f() is defined in
# environment 'env')

library(envnames)

f_prints_env_name <- function() {
  # This prints the name of the environment to which the environment returned 
  # by environment() was assigned to (i.e. 'envfun')
  envfun = environment()
  envfun_name = environment_name(environment(), envir=environment())
  print(envfun_name)
}

f_prints_NULL <- function() {
  # This doesn't return any environment name because environment() does not have a
  # name or it was not assigned to any variable (in which case it would have the name
  # of the variable to which it was assigned (as in f_prints_env_name())
  envfun_name = environment_name(environment(), envir=environment())
  print(envfun_name)
}

f_prints_env_plus_function_name <- function() {
  get_fun_calling(0)
}

f_prints_env_name()
f_prints_NULL()
f_prints_env_plus_function_name()   # This is perhaps what Gabor wants...
