\name{envnames-package}
\alias{envnames-package}
\alias{envnames}
\docType{package}
\title{Keeps track of user-defined and execution environment names}
\description{Set of tools to:

- retrieve the environment name of environments created with new.env()

- find the environment where an object is defined (including user-defined environments)

- keep track of execution environments
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
\seealso{N/A}
\examples{
### Example 1: Retrieving the names of environments created with new.env()
# Create new environments
env1 = new.env()
env2 = new.env()
env3 = new.env(parent=baseenv())
env_of_envs = new.env()
with(env_of_envs, env11 <- new.env())

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
# Define function g() in the env2 environment to be called by function f() below
# Function g() prints the name of the calling function.
with(env2,
     g <- function(x) {
       # Setup for environment tracking
       env_address = setup_env(); on.exit(close_env(env_address))

       # Get the name of the calling environment
       env_calling_name = get_env_calling()
       cat("Now inside function", get_fun_name(), ": calling environment is", env_calling_name, "\n")

       # Start process
       x = x + 2;
       return(invisible(x))
     }
)

# Calling function
with(env1,
     f <- function(x) {
       # Setup for environment tracking
       env_address = setup_env(); on.exit(close_env(env_address))
       
       # Start
       gval = env2$g(x)
       return(invisible(gval))
     }
)

# Run function f and 
env1$f(7)


### Example 3: find the location of an object
# This differs from the R function find() because it also searches in user-defined environments
obj_find("f")                 # "env1"
obj_find("f", silent=TRUE)    # Run in silent mode

env2$xx = 2
obj_find("xx")                # "env2"

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
