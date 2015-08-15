\docType{package}
\name{envnames-package}
\alias{envnames-package}
\alias{envnames}
\title{Tracks user-defined and execution environment names}
\description{The main goal of this package is to overcome the limitation of the built-in \code{\link{environmentName}} function of the base package which cannot retrieve the name of an environment unless it is a package or the global environment.

The envnames package solves this problem by creating a lookup table that maps environment names to their memory addresses. Using this lookup table, it is possible to retrieve the name of any environment where an object resides or to retrieve the environment of the calling function.
}
\details{
\tabular{ll}{
Package: \tab envnames\cr
Type: \tab Package\cr
Version: \tab 0.2\cr
Date: \tab 2014-10-20\cr
License: \tab GPL\cr
}
1) Use setup_env_table() to create a lookup table with memory addresses and names of existing environments (including user-defined environments and loaded package environments).


2) Inside a function, use setup_env() to setup the global lookup table that keeps track of execution environments.


3) Use environment_name() to retrieve the name of an environment created with new.env() using the above lookup table.


4) Use obj_find() to find the environments where a given object is defined.
}
\author{Daniel Mastropietro

Maintainer: Daniel Mastropietro <mastropi@uwalumni.com>
}
\references{N/A}
\keyword{ package }
\keyword{ environment }
\seealso{ \code{\link{environmentName}} in the base package for the built-in function that retrieves environment names of packages.}
\examples{
### Example 1: Retrieve the names of environments created with new.env()
# Create new environments
env1 = new.env()                      # Environment in .GlobalEnv
env2 = new.env()                      # Environment in .GlobalEnv
env3 = new.env(parent=baseenv())      # Environment in base environment
env_of_envs = new.env()               # User-defined environment that contains other environments
with(env_of_envs, env11 <- new.env()) # Environment defined inside environment \code{env_of_envs}

# Create the table containing the address-name pairs of existing environments
(env_table_global = setup_env_table())
(env_table_env_of_envs = setup_env_table(envir=env_of_envs))

# Retrieve the environment name
# Note the use of quote() to enclose the environment variable
environment_name(quote(env1), env_table_global)                           # "env1"
environment_name(quote(env3), env_table_global)                           # "env3"
environment_name(quote(env9), env_table_global)                           # NULL (env9 does not exist)
environment_name(quote(env_of_envs), env_table_global)                    # "env_of_envs"
environment_name(quote(env11), env_table_env_of_envs, envir=env_of_envs)  # "env11"


### Example 2: Track of execution environments
# Define two environments
env1 = new.env()
env2 = new.env()
# Define function g() in environment \code{env2} to be called by function f() below
# Function g() prints the name of the calling function.
with(env2,
     g <- function(x) {
       # Setup for environment tracking
       env_address = setup_env(); on.exit(close_env(env_address))

       # Get the name of the calling environment
       env_calling_name = get_env_calling()
       
       # Show calling environment without using envnames package and using it
       cat("Now inside function", get_fun_name(), "\n")
       cat("Calling environment name (using environmentName() function):  \"", environmentName(sys.frame(sys.parent())), "\"\n", sep="")
       cat("Calling environment name (using envnames::get_env_calling()): \"", env_calling_name, "\"\n", sep="")

       # Start process
       x = x + 2;
       return(invisible(x))
     }
)

# Calling function whose name should be printed when g() is run
with(env1,
     f <- function(x) {
       # Setup for environment tracking
       env_address = setup_env(); on.exit(close_env(env_address))
       
       # Start
       gval = env2$g(x)
       return(invisible(gval))
     }
)

# Run function f with argument 7
env1$f(7)                     # Prints: "Now inside function g : calling environment is env1:f"


### Example 3: find the location of an object
# This differs from the R function find() because it also searches in user-defined environments
obj_find("f")                 # "env1"
obj_find("f", silent=TRUE)    # Run in silent mode

env2$xx = 2
obj_find("xx")                # "env2"


### Cleanup: delete environments
rm(list=c("env1", "env2", "env3", "env_of_envs", "env_table_global", "env_table_env_of_envs"))


\dontshow{
# Define function g inside environment env3 whose parent is the baseenv environment
# In this case, we need to explicitly call the functions in package 'envnames' with envnames::<function>
with(env3,
     g <- function(x) {
       # Setup for environment tracking
       env_address = envnames::setup_env(); on.exit(envnames::close_env(env_address))
       
       # Get the name of the calling environment
       env_calling_name = envnames::get_env_calling()
       cat("Now inside function", envnames::get_fun_name(), ": calling environment is", env_calling_name, "\n")
       
       # Start process
       x = x + 3;
       return(invisible(x))
     }
)

# Calling function
h <- function(x) {
  # Setup for environment tracking
  env_address = setup_env(); on.exit(close_env(env_address))
  
  # Start
  xval = env3$g(x)        # NOTE: The results are NOT what expected if calling g(x) as with(envir, g(x))... The envnames::get_env_calling() function in this case returns env3 as calling environment instead of the execution environment of h!! (WHY???)
  return(invisible(xval))
}

# Before calling h(7) need to set the environment() attribute of env3 to the environment where it was defined. Otherwise the process does not work.
environment(env3) = globalenv()
h(7)
}
}
