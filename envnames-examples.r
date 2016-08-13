# Check that there are no objects in the environment
print(ls())

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
       # Setup for environment tracking
#       env_address = setup_env(); on.exit(close_env(env_address))

       # Get the name of the calling environment
#       env_calling_name = get_env_calling()
			 fun_calling_name = get_fun_calling()
			 cat("FUN CALLING\n")
			 print(fun_calling_name)

       # Show calling environment without using envnames package and using it
       cat("Now inside function", get_fun_name(), "\n")
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
       # Setup for environment tracking
#       env_address = setup_env(); on.exit(close_env(env_address))
       
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
with(env1, obj_find(f))       # "env1" (note the use of \code{with} since \code{f} is defined in the \code{env1} environment and does not exist in the global environment)
obj_find("f", silent=TRUE)    # Run in silent mode

env2$xx = 2
obj_find("xx")                # "env2"


\dontshow{
# Define function g inside environment env3 whose parent is the baseenv environment
# In this case, we need to explicitly call the functions in package 'envnames' with envnames::<function>
with(env3,
     g <- function(x) {
       # Setup for environment tracking
#       env_address = envnames::setup_env(); on.exit(envnames::close_env(env_address))
       
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

### Cleanup: delete environments
rm(list=c("env1", "env2", "env3", "env_of_envs", "env_table_env_of_envs"))
