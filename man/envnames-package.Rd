\docType{package}
\name{envnames-package}
\alias{envnames-package}
\alias{envnames}
\title{Track user-defined environment names}
\description{The main goal of this package is to overcome the limitation of the built-in \code{\link{environmentName}} function of the base package which cannot retrieve the name of an environment unless it is a package or the global environment. This implies that all user-defined environments don't have a "name assigned" that can be retrieved and refer to the environment.

The envnames package solves this problem by creating a lookup table that maps environment names to their memory addresses. Using this lookup table, it is possible to retrieve the name of any environment where an object resides or, from within a function, to retrieve the calling stack showing the function names and their enclosing environment name, i.e. the environment where the functions have been defined. The latter can be done with just a function call which returns a string that can directly be used inside a \code{cat()} call to display the function name (as opposed to using the R function \code{\link{sys.call}} which does not return a string, but a more complicated object, a "call" from where we should extract the string for used inside \code{cat()}).

Package conventions: all functions in this package follow the underscore-separated and all-lower-case naming convention (e.g. \code{environment_name()}, \code{get_obj_address()}, etc.).
}
\details{
The package currently contains 10 visible functions. Following is a short guideline on how to use the main functions of the package. Please refer to the vignette for further information.

1) Use \code{\link{address}} to retrieve the memory address of any object, including environments.

2) Use \code{\link{environment_name}} to retrieve the name of an environment created with \code{\link{new.env}} based on its memory address.

3) Use \code{\link{obj_find}} to find the environments where a given object is defined.

4) Use \code{\link{get_fun_calling}(n)} from within a function to retrieve the name of the calling function \code{n} levels up in the calling stack together with their enclosing environment name.

5) Use \code{\link{get_fun_calling_chain}} from within a function to get the calling functions stack.
}
\author{Daniel Mastropietro

Maintainer: Daniel Mastropietro <mastropi@uwalumni.com>
}
\references{
Motivation for developing this package:  
- A comment by instructor Andrea Spano during his [R for developers course]\url{http://www.quantide.com/courses-overview/r-for-developers} about the impossibility of retrieving the name of user-defined environments.  
- A [question]\url{https://stat.ethz.ch/pipermail/r-help/2010-July/245646.html} posted by Gabor Grothendieck at the R-Help forum
}
\keyword{ package }
\keyword{ environment }
\seealso{ \code{\link{environmentName}} in the base package for the built-in function that retrieves environment names of packages.

\code{\link{exists}} and \code{\link{find}} for alternatives of looking for objects in the workspace.

\code{\link{sys.call}} for other alternatives for retrieving the function calling stack.
}
\examples{
### Example 1: Retrieve the names of user-defined environments (created with new.env())
# Create new environments
env1 <- new.env()                      # Environment in .GlobalEnv
env2 <- new.env()                      # Environment in .GlobalEnv
env3 <- new.env(parent=baseenv())      # Environment whose enclosure or parent environment is the base environment (as opposed to the global environment)
env_of_envs <- new.env()               # User-defined environment that contains other environments
with(env_of_envs, env11 <- new.env()) # Environment defined inside environment env_of_envs

# Retrieve the environment name
environment_name(env1)                       # named array with value "env1" and name "R_GlobalEnv"
environment_name(env3)                       # named array with value "env3" and name "R_GlobalEnv"
environment_name(env9)                       # NULL (env9 does not exist)
environment_name(env_of_envs)                # named array with value "env_of_envs" and name "R_GlobalEnv"
environment_name(env11, envir=env_of_envs)   # "env11"
environment_name(env11)                      # named array with value "env_of_envs$env11" and name "env_of_envs"


### Example 2: Retrieve calling functions and their environments
### Note in particular the complicated use of sys.call() to retrieve the call as a string... 
# Define two environments
env1 <- new.env()
env2 <- new.env()
# Define function g() in environment env2 to be called by function f() below
# Function g() prints the name of the calling function.
with(env2,
     g <- function(x) {
       # Current function name
       fun_current_name = get_fun_name()

       # Get the name of the calling environment and function
			 fun_calling_name = get_fun_calling()

       # Show calling environment using and not using the envnames package
			 # Essenially the environment name without using the envnames package is empty.
       cat("Now inside function", fun_current_name, "\n")
       cat("Calling environment name (using environmentName(parent.frame())):  \"", environmentName(parent.frame()), "\"\n", sep="")
       cat("Calling environment name (using sys.call(1) through as.character( as.list(sys.call(1))[[1]]) )):", " \"", as.character( as.list(sys.call(1))[[1]]), "\"\n", sep="")
       cat("Calling environment name (using envnames::get_fun_calling()): \"", fun_calling_name, "\"\n", sep="")

       # Start process
       x = x + 2;
       return(invisible(x))
     }
)

# Calling function whose name should be printed when g() is run
with(env1,
     f <- function(x) {
       # Start
       gval = env2$g(x)
       return(invisible(gval))
     }
)

# Run function f to show the difference between using and not using the envnames package to retrieve the function calling stack.
env1$f(7)


### Example 3: find the location of an object
# This differs from the R function exists() because it also searches in user-defined environments and any environments wihin.
obj_find(f)                   # "env1"
obj_find("f")                 # Same thing: "env1"
obj_find("f", silent=FALSE)   # Run verbosely

env2$x <- 2
obj_find(x)                   # "env2"

obj_find(nonexistent)         # NULL

\dontshow{
# This is not shown because the example is very similar to Example 3 above
# Define function g inside environment env3 whose parent is the baseenv environment
# In this case, we need to explicitly call the functions in package 'envnames' with envnames::<function>
with(env3,
     g <- function(x) {
       # Setup for environment tracking
#       env_address = envnames::setup_env(); on.exit(envnames::close_env(env_address))
       
       # Get the name of the calling environment
       env_calling_name = envnames::get_fun_calling_chain()[1,"env"]
       fun_calling_name = envnames::get_fun_name(1)
       cat("Now inside function", envnames::get_fun_name(), ": calling function is", fun_calling_name, ", calling environment is", env_calling_name, "\n")
       
       # Start process
       x = x + 3;
       return(invisible(x))
     }
)

# Calling function
h <- function(x) {
  # Start
  xval = env3$g(x)        # NOTE: The results are NOT what expected if calling g(x) as with(envir, g(x))... The envnames::get_env_calling() function in this case returns env3 as calling environment instead of the execution environment of h!! (WHY???)
  return(invisible(xval))
}

h(7)

### Cleanup: delete environments
rm(list=c("env1", "env2", "env3", "env_of_envs", "env_table_env_of_envs"))
}
}
