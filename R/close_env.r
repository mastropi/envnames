# Removes the env_address entry (given as charater value) from the address-names lookup table .envmap (defined in .GlobalEnv)
close_env = function(env_address)
{
  assign(".envmap", .envmap[-which(.envmap[,1] == env_address),], envir=.GlobalEnv)
}
