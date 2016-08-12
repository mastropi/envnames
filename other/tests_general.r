# 26-Jul-2016
# General tests that I need to perform in order to go forward with the refactoring of the package


g = function() {
  get_calling_chain();
  # print(env_address);
  # # Get the name of the calling environment
  # env_calling_name = get_env_calling()
  # cat("Now inside function", get_fun_name(), ": calling environment is", env_calling_name, "\n")
  
  print(sys.nframe());
  print(sys.frame(sys.nframe()));
  print(sys.frame(sys.nframe()-1));
  print(sys.frame(sys.parent()))              # Equivalent to parent.frame()
  print(parent.env(sys.frame(sys.nframe())))  # Environment where function g() is defined, which is retrieved as the environment of the **execution** environment of function g()
}

f = function() {
  g();
  print(environment())  # Execution environment. Equivalent to sys.frame(sys.nframe()).
  print(parent.frame())
}

environment(g) <- env1
environment(f) <- env11

f()


# Create an environment inside another environment
env_of_envs = new.env()
with(env_of_envs, env9 <- new.env())


################## FOR A NEW VERSION OF THE get_env_names() FUNCTION (with extended functionality)
### 1.- Get all environments defined in every package in the search path --------
### This is an extension of the get_env_names() function because it also lists the environments
### inside packages other than the Global Environment.
# The names of the returned character array contain the root of the packages where the environments have been
# found with a number indicating the object ID (e.g. .GlobalEnv1 for fthe first found object, .GlobalEnv2 for the second found object, etc.)
# This should replace the creation of the env_names variable in get_env_names().
environments = unlist(  sapply(  search(),
                                 FUN=function(package) {
                                   Filter(function(x) "environment" %in% class(get(x, envir=as.environment(package))),
                                          ls(envir=as.environment(package))
                                   )
                                 }
)
)



# Get the address of every environment found
addresses = sapply( environments, FUN=function(x) get_obj_address(parse(text=x)) )

# Put them all together in a data frame
df.envmap = data.frame(address=addresses, name=environments)
## The rownames contain the package name where the environments are found (e.g. .GlobalEnv1, .GlobalEnv2, etc.)


source("R/get_obj_name.r")
source("R/get_obj_address.r")

# 2.- Need to recurse on every environment to see if there are other environments defined inside and get their addresses
for (env_name in environments) {
  env = eval(parse(text=env_name))  # NOTE: it is safer to use parse() than as.name() because as.name() returns a name enclosed in ` when e.g. we pass a string containing the dollar sign as in env_of_envs$env9
  new_environments = Filter(function(x) "environment" %in% class(get(x, envir=env)),
                             ls(envir=env))
  # Get the address of the new environments
  new_addresses = sapply( new_environments, FUN=function(x) { obj = as.name(x); get_obj_address(obj, envir=env, n=2) } )
  print(paste(env_name, "$", new_environments, sep=""))
}
################## FOR A NEW VERSION OF THE get_env_names() FUNCTION (with extended functionality)


# IMPORTANT: How to retrieve an object from an environment:
env_of_envs = new.env()
env_of_envs$env9 = new.env()
# NOTE: we need to use evalq(), o.w. env9 is evaluated in the current environment (globalenv()) before passing it to the evaluator.
evalq(env9, envir=env_of_envs)
evalq(env9, envir=list(globalenv()), enclos=env_of_envs)
# NOTE 2: If an object is not found in an environment, the parent environments are searched for that object,
# as stated in https://cran.r-project.org/doc/manuals/R-lang.html#Environment-objects:
#   Environments can be thought of as consisting of two things:
#   a frame, which is a set of symbol-value pairs, and an enclosure, a pointer to an enclosing environment.
#   When R looks up the value for a symbol the frame is examined and if a matching symbol is found its value
#   will be returned.
#   If not, the enclosing environment is then accessed and the process repeated.
#   Environments form a tree structure in which the enclosures play the role of parents.
#   The tree of environments is rooted in an empty environment, available through emptyenv(),
#   which has no parent.
#
# Ref: http://www.r-bloggers.com/environments-in-r/ (NICE SUMMARY ARTICLE!)
# Especially the following:
# -  When a namespaced package is loaded, a new environment is created and all exported items are copied into it.
# *** The namespace becomes the environment for the functions in that package. ***
#

# The following function works perfectly well!! i.e.:
# - we can pass an object as parameter (not only a string with the object name)
# - no error is given when the object does not exist
obj_inspect2 <- function(obj, envir=.GlobalEnv) {
  obj_name = get_obj_name(obj)
  if (exists(obj_name, envir=envir, inherits=TRUE)) {
    print("found")
    obj.inspection = address(eval(as.name(obj_name), envir=envir))
#    obj.inspection = address(eval(obj, envir=envir))     # This does NOT work when envir is not the global
#    cat("value is:\n")
#    print(get(obj, envir=envir))
  } else {
    print("not found")
    obj.inspection = NULL
  }
  
  return(obj.inspection)
}

# PUT ALL THESE TESTS IN A testthat CODE
obj = "x"
obj_inspect2(env9, envir=globalenv())  # not found
obj_inspect2(env9, envir=env_of_envs)  # found
obj_inspect2(x, envir=env_of_envs)     # found (because the search by exists() continuous in parent environments)
obj_inspect2("x", envir=env_of_envs)   # found (because the search by exists() continuous in parent environments)
obj_inspect2("x", envir=globalenv())   # found
y = as.name("x")
obj_inspect2(y, envir=globalenv())     # found
obj_inspect2(as.name("x"), envir=globalenv()) # not found (OK)
obj_inspect2(get("x"), envir=globalenv())     # not found (OK)
obj_inspect2(x, envir=globalenv())     # found
obj_inspect2(obj, envir=globalenv())   # found
obj_inspect2(xdafafs, envir=globalenv())   # not found
