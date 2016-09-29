\docType{package}
\name{envnames-package}
\alias{envnames-package}
\alias{envnames}
\title{Track user-defined and execution environment names}
\description{The main goal of this package is to overcome the limitation of the built-in \code{\link{environmentName}} function of the base package which cannot retrieve the name of an environment unless it is a package or the global environment.

The envnames package solves this problem by creating a lookup table that maps environment names to their memory addresses. Using this lookup table, it is possible to retrieve the name of any environment where an object resides or to retrieve the environment of the calling function.

All function in this package follow the underscore_separated all lower case naming convention.
}
\details{
The package currently contains 10 functions. Following is a guideline on how to use the package to track user-defined environment names and function environments:

1) Use environment_name() to retrieve the name of an environment created with \code{\link{new.env}}.

2) Use \code{\link{obj_find}} to find the environments where a given object is defined.
}
\author{Daniel Mastropietro

Maintainer: Daniel Mastropietro <mastropi@uwalumni.com>
}
\references{N/A}
\keyword{ package }
\keyword{ environment }
\seealso{ \code{\link{environmentName} } in the base package for the built-in function that retrieves environment names of packages.}
\examples{
### Example 1: Retrieve the names of environments created with new.env()
# Create new environments
env1 = new.env()                      # Environment in .GlobalEnv
env2 = new.env()                      # Environment in .GlobalEnv
env3 = new.env(parent=baseenv())      # Environment in base environment
env_of_envs = new.env()               # User-defined environment that contains other environments
with(env_of_envs, env11 <- new.env()) # Environment defined inside environment \code{env_of_envs}

# Create the table containing the address-name pairs of existing environments
#setup_envmap()
#setup_envmap(envir=env_of_envs)

# Retrieve the environment name
# Note the use of quote() to enclose the environment variable
environment_name(env1)                       # "env1"
environment_name(env3)                       # "env3"
environment_name(env9)                       # NULL (env9 does not exist)
environment_name(env_of_envs)                # "env_of_envs"
environment_name(env11, envir=env_of_envs)   # "env11"


### Example 2: Track of execution environments
# Define two environments
env1 = new.env()
env2 = new.env()
# Define function g() in environment \code{env2} to be called by function f() below
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
       cat("Calling environment name (using environmentName() function):  \"", environmentName(parent.frame()), "\"\n", sep="")
       cat("Calling environment name (using envnames::get_fun_calling_chain()): \"", fun_calling_name, "\"\n", sep="")

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

# Run function f to show the difference between using and not using envnames
env1$f(7)


### Example 3: find the location of an object
# This differs from the R function find() because it also searches in user-defined environments
obj_find(f)                   # "env1"
obj_find("f")                 # Same thing: "env1"
obj_find("f", silent=FALSE)   # Run verbosely

env2$xx = 2
obj_find(xx)                  # "env2"

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
}

### Cleanup: delete environments
rm(list=c("env1", "env2", "env3", "env_of_envs", "env_table_env_of_envs"))
}
