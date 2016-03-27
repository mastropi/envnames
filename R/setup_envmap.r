#' TBD
#'
#' Function that creates a lookup table called .envmap containing the address-name pairs of environments
#' existing in the envir environment AND all the environments returned via search()
#' If the lookup table .envmap already exists, it is overridden.
#
# Should this function be exported??
setup_envmap <- function(envir=.GlobalEnv) {
  env_table = get_env_names(envir=envir)
  
  # Create the .envmap global lookup table in the envir environment
  assign(".envmap", env_table, envir=envir)
}
