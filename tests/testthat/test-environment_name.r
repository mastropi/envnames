library(envnames)
context("Environment names")

# 1.- Prepare the workspace -----------------------------------------------
# Create new environments
env1 = new.env()                      # Environment in .GlobalEnv
env2 = new.env()                      # Environment in .GlobalEnv
env3 = new.env(parent=baseenv())      # Environment in base environment
env_of_envs = new.env()               # User-defined environment that contains other environments
with(env_of_envs, env11 <- new.env()) # Environment defined inside environment \code{env_of_envs}


# 2.- Address-name pairs lookup table -------------------------------------
# Create the table containing the address-name pairs of existing environments
#debugonce(setup_envmap)
#trace(get_env_names, tracer=quote(cat(sprintf("tracing get_env_names(*, env_resolves=)\n", env_resolves))))
setup_envmap()
#env_table = get_env_names()
#.envmap = env_table
#untrace(get_env_names)

setup_envmap(envir=env_of_envs)

cat("ENVMAP:\n")
print(.envmap)
## STRANGE: WHEN RUNNING VIA devtools::test() THE .envmap TABLE IS CORRECTLY BUILT
## BUT WHEN RUNNING VIA CTRL+SHIFT+T IN RSTUDIO, THE .envmap TABLES DOES NOT CONTAIN ANY OF THE ENVIRONMENTS JUST DEFINED!!
## HOWEVER, THE .envmap TABLE INSIDE THE env_of_envs ENVIRONMENT IS CORRECT... IT CONTAINS THE env11 ENVIRONMENT.
## IT SEEMS IT HAS TO DO WITH HOW THE GLOBAL ENVIRONMENT IS REFERRED BY WHEN CALLING RTERM.EXE?
## Here is the call run by CTRL+SHIFT+T in RStudio:
## "C:/PROGRA~1/R/R-32~1.0/bin/x64/Rterm.exe" --vanilla --slave -f "testthat.R"
cat("ENVMAP 2:\n")
print(env_of_envs$.envmap)

# 3.- TEST! ---------------------------------------------------------------
# Note the use of quote() to enclose the environment variable
test_that("environment names are correctly returned", {
  expect_equal(environment_name(quote(env1)), "env1")
  expect_equal(environment_name(quote(env3)), "env3")
#  cat("env3:\n", environment_name(quote(env3)))
  expect_equal(environment_name(quote(env9)), NULL)   # env9 does not exist
#  cat("env_of_envs:\n", environment_name(quote(env_of_envs)))
  expect_equal(environment_name(quote(env_of_envs)), "env_of_envs")
  expect_equal(environment_name(quote(env11), envir=env_of_envs), "env11")
})


# 4.- Cleanup -------------------------------------------------------------
rm(list=c("env1", "env2", "env3", "env_of_envs"))
