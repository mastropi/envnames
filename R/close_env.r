#' Remove an address from the global address-name lookup table
#' 
#' Remove the \code{env_address} entry from the global address-name lookup table \code{.envmap}
#' defined in the global environment.
#' 
#' @param env_address memory address given as a character string to remove from the \code{.envmap} lookup table.
close_env = function(env_address)
{
  assign(".envmap", .envmap[-which(.envmap[,1] == env_address),], envir=.GlobalEnv)
}
