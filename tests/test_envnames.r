# Created:      20-Oct-2014
# Author:       Daniel Mastropietro
# Description:  Test the envnames package by hand (i.e. without using the testthat package)
#

library(envnames)

# Clear workspace
rm(list=ls())

# Compile the R code present in the package directory
# (this should not be necessary if we have loaded the envnames package as done above)
# files = list.files(path="R/", pattern=".r$")
# for (f in files) {
#   cat("Compiling...", f, "\n");
#   source(file.path("R/",f))
# }

### 0. Prepare workspace for testing
env1 = new.env()
env2 = new.env()
env3 = new.env(parent=baseenv())
with(env1, f <- function(x) x + 1)
with(env2, g <- function(x) x*pi)


### 1. Retrieve the environment name
# (2016/01/15) Note the use of quote() to enclose the environment for now which has to do with
# the fact that an environment cannot be converted to a string (as in as.character(env1)).
# In principle I should be able to solve for the need of using quote() by adding the quote() function
# appropriately inside the environment_name() function... but still need to figure out how.
# (2016/08/11) We no longer need to enclose the environment name in quote()!
environment_name(env1)
environment_name(env2)
environment_name(env3)
environment_name(env9)   # This should be NULL as env9 is not an environment


### 2. Create another environment that holds other environments in turn
env_of_envs = new.env()
with(env_of_envs, env11 <- new.env())
with(env_of_envs, env12 <- new.env())
with(env_of_envs$env11, h <- function(x) x*2)

# Retrieve the environment name
# (2016/08/11) Here we still need to enclose the environment name in quote(), because envir= is not the global environment... but why? I thought I had solved this!
# (2016/09/29) Enclosing the environment name in quote() is no longer necessary! nice! (although it's accepted as well)
environment_name(env11, envir=env_of_envs)
environment_name(quote(env12), envir=env_of_envs)
environment_name(env13, envir=env_of_envs)  # This should return NULL as env13 does not exist (note that no error is raised! nice!)


### 3.- Test the display of the calling chain (Andrea Spanò's wish) -------------------------
# Called function
with(env2,
     g <- function(x) {
        cat("Call stack inside g:\n")
        print(get_fun_calling_chain())
        
        # Start
        x = x + 1;
        
        return(x)
        }
)

# Calling function
with(env1,
     f <- function(x) {
  #------------------------- Just for FYI --------------------------
  # The following is done for informational purposes only but it is not actually needed
  # to show the chain of calling environments and functions (a.k.a. the calling stack)
  env_current = environment()           # This is the current execution environment
  env_parent = parent.env(env_current)  # This is the calling environment
  env_parent2 = parent.env(env_parent)  # This is the chained calling environment (two levels up)

  # Get the name of the calling environment env_parent
  env_parent_name = environment_name(env_parent, envir=env_parent2)
  # Get the address of the current environment (execution environment)
  env_address = get_obj_address(env_current, envir=env_parent)
  cat("Address of execution environment:", env_address, "\n")
  cat("Name of calling environment:", env_parent_name, "\n")
  #------------------------- Just for FYI --------------------------

  cat("Call stack inside f:\n")
  print(get_fun_calling_chain())

  # Start
  x = 0;
  gval = env2$g(x)

  invisible(return(gval))
}
)

# Call f to show the two calling chains (inside f and inside g)
cat("Value of x after calling f is:", env1$f(7))


### 4. Find the environment(s) where an object is defined
obj_find(f)
