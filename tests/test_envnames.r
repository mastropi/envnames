# Created:      20-Oct-2014
# Author:       Daniel Mastropietro
# Description:  Test the envnames package
#

library(envnames)

# Clear workspace
rm(list=ls())

# Compile the R code present in the package directory
files = list.files(path="R/", pattern=".r$")
for (f in files) {
  cat("Compiling...", f, "\n");
  source(file.path("R/",f))
}


env1 = new.env()
env2 = new.env()
env3 = new.env(parent=baseenv())
with(env1, f <- function(x) x + 1)
with(env2, g <- function(x) x*pi)

# Start: Create a lookup table containing address-name pairs of environments
setup_envmap()

# Retrieve the environment name
environment_name(quote(env1))
environment_name(quote(env2))
environment_name(quote(env3))
environment_name(quote(env9))

### 2. Create another environment that holds other environments in turn
env_of_envs = new.env()
with(env_of_envs, env11 <- new.env())
with(env_of_envs, env12 <- new.env())
with(env_of_envs$env11, h <- function(x) x*2)

# Start: Create a lookup table containing address-name pairs of environments
env_lookup_1 = setup_envmap(envir=env_of_envs)

# Retrieve the environment name
environment_name(quote(env11), env_lookup_1, envir=env_of_envs)
environment_name(quote(env12), env_lookup_1, envir=env_of_envs)
environment_name(quote(env13), env_lookup_1, envir=env_of_envs)




### 3.- Test this inside a function (Andrea's wish) -------------------------
# Setup
# Lookup table to store the address-name pairs of the execution environments
lut = as.data.frame( matrix(nrow=0, ncol=2))
names(lut) = c("address", "name")

# Called function
with(env2,
     g <- function(x) {
        # Setup: get information about the calling environment
        #env_parent = parent.env(env_current)
        #cat("Parent environment of current function\n")
        #print(env_parent)
        env_current = environment()
        env_calling = parent.frame()
        # Memory address of the calling function as text
        # Note that we can find environment 'env_calling' from within env_current (i.e. the currently executing environment)
        # because we have just defined env_calling in the execution environment!!)
        calling_address = get_env_address(quote(env_calling), envir=env_current)
        calling_name = as.character(lut[lut[,1]==calling_address,2])
        # Retrieve the name associated to calling_address
        # This is not necessary because 
        #calling_name = environment_name(calling_address, lut, envir=env_calling)
        ## NOTE: The above is equivalent to:
        ## env_parent_calling = parent.env(env_calling)
        ## cat("Parent environment of calling function:\n")
        ## print(env_parent_calling)
        ## calling_address = get_env_address(env_calling, envir=env_parent_calling)
        ## calling_name = environment_name(calling_address, lut, envir=env_parent_calling)
        #cat("Calling address:\n")
        #print(calling_address)
        cat("Calling name:\n")
        print(calling_name)
        
        # Start
        x = x + 1;
        
        return(x)
        }
)

# Calling function
with(env1,
     f <- function(x) {
  #------------------------- Move to function setup_env() --------------------------
  # Setup: prepare lookup table to hold the address of the execution environment
  op.warn = options("warn")
  options(warn=-1); on.exit(options(warn=op.warn))
  env_current = environment()
  env_parent = parent.env(env_current)
  env_parent2 = parent.env(env_parent)
  
  # Check if env_parent has a name or is just a memory address in order to define the type of address
  # to look for in the environment_name() function
  env_parent_address = get_env_address(env_parent)
  try_asnumeric = try( as.numeric( substr(env_parent_address, 2, nchar(env_parent_address)-1) ), silent=TRUE )
  if (!is.na(try_asnumeric)) {
    type = "variable"
  } else {
    type = "package"
  }
  env_parent_name = environment_name(env_parent, type=type, envir=env_parent2)
  #print(env_parent)
  # The following trick of changing the environment of function was tried when the inspect() call returned infinite information... (never ending)
  # but then I found the silent=TRUE option in inspect()!! --and therefore the trick is on longer necessary.
  #env_tmp = new.env()
  #environment(f) <- env_tmp
  #env_address = get_env_address(env_current, envir=env_tmp)
  env_address = get_env_address(env_current, envir=env_parent)
  ## IMPORTANT: env_current should NOT be quoted with quote(), otherwise env_current is not found in the parent environment, because it does not have a name!
  ## HOWEVER THERE IS STILL A PROBLEM WITH THIS... The fact that the inspect() function being called inside
  ## obj_inspect() --which is called get_env_address()-- generates what is apparently an infinite output!!
  ## UNLESS WE USE THE silent=TRUE in internal function inspect()!!!! --> GREAT!!
  #print(env_address)
  # Add the entry for the curent function environment to the lut
  lut <<- rbind(lut, data.frame(address=env_address, name=paste(env_parent_name, "env_f", sep=":")))
  cat("Following pair added to the lookup table lut:\n")
  print(lut[nrow(lut),])
  #------------------------- Move to function setup_env() --------------------------

  #---------------- Replace the argument of on.exit() with a function --------------
  on.exit(lut <<- lut[-which(lut[,1] == env_address), ])
  #---------------- Replace the argument of on.exit() with a function --------------

  # Start
  x = 0;

  #browser()
  gval = env2$g(x)
  
  cat("Removing following pair from the lookup table:\n")
  print(lut[which(lut[,1] == env_address), ])
  
  invisible(return(gval))
}
)
env1$f

# Test the tracking of the environments
env1$f(7)

# PROBLEMS ENCOUNTERED AND SOLVED:
# - need to pass the environment using quote() when checking the name of an environment NOT in .GlobalEnv.
# - inspect() returned an infinite amount of information (never ending) --> [SOLVED] used silent=TRUE as option
# - the address of the environment is not always found at the first line of the captured.output() of inspect()
#   - in the normal situation (most common situation of just finding the name of an environment) it is found on the first line
#   - in the less usual situation of the f calls g situation (Andrea's wish) the address is stored in the second line.
#   The distinction between the two cases was easily done by checking that lengt(obj.inspect) == 2 (which occurs in the second case),
#   o.w. length(obj.inspect) > 2 (usually 5?)
#



### Example 2: Make use of the setup_env() and close_env() functions
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

# Let's define a function h in environment env3 which has 'base' as its parent environment... (as opposed to .GlobalEnv)
# In this case, we need to explicitly call the functions in package 'envnames' with envnames::<function>
# Note however, that any call INSIDE those functions do NOT need to explicitly specify the package name
# because the parent environment of any function inside the package is envanmes (since they are defined in envnames) 
# Therefore, any call to a function in the package will be found.
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
with(env1,
     f <- function(x, envir) {
       # Setup for environment tracking
       env_address = setup_env(); on.exit(close_env(env_address))
       
       # Start
       assign("x", x, envir)
       xval = with(envir, g(x))
       return(invisible(xval))
     }
)

env1$f(3, env3)


# Retrieve the environment name of an object
obj_find("f")


test = function(obj, env=.GlobalEnv) {
  # Get the current environment
  # This is important because the obj.inspect below is executed on the calling environment where env (defined in the evaluation environment) does NOT exist!
  env_current <- environment()
  env_parent <- parent.env(env_current)
  env_calling <- parent.frame()
  cat("Current environment:\n")
  print(env_current)
  cat("Parent environment:\n")
  print(env_parent)
  cat("Calling environment:\n")
  print(env_calling)
  cat("\n")
  
  # Need to store the environment passed as argument in the calling environment, because there is where the object inspection must take place!
  env0 <<- env
  
  eval(print(env0), envir=env_calling)
  invisible(NULL)
}
test(f, env=env1)
