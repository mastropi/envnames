# Removes the env_address entry (given as charater value) from the address-names lookup table .lut (defined in .GlobalEnv)
close_env = function(env_address)
{
  assign(".lut", .lut[-which(.lut[,1] == env_address),], envir=.GlobalEnv)
}
