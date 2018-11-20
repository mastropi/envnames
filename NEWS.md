# envnames 0.4.0 (10-Nov-2018)
## Major changes
* The Empty environment is now listed in the address-name lookup table created by `get_env_names()` and thus retrieved by the `environment_name()` function.  
* User environments defined in packages are now found (as long as they are exported), including environments nested in other user environments. In the previous release, only user environments defined in the global environment (or nested within other user environments) were found.

## Minor changes
* The `locationaddress` column of the address-name lookup table created by `get_env_names()` is now filled with the address of the `location` environment where user-defined environments live (instead of being equal to `NA` in those cases).

## Bug fixes
* Solved the problem of not finding user environments when their parent environment is the Empty environment.

## Internal
* Created a (visible) environment in the package (`testenv`) used to test package functionalities related to finding environments defined in packages.

# envnames 0.3.0 (22-Jul-2018)
Initial submission to CRAN.
