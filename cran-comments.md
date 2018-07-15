## Test environments
* ubuntu 18.04 LTS on Windows 10 R 3.4.4 (64-bit)
* win-builder (devel)

### There were no errors or warnings.
### There were 2 NOTES:

#### Note 1
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Daniel Mastropietro <mastropi@uwalumni.com>'

New submission

#### Note 2
* checking dependencies in R code ... NOTE
There are ::: calls to the package's namespace in its code. A package
  almost never needs to use ::: for its own objects:
  'address' 'check_object_exists' 'check_object_with_path'
  'clean_up_matching_environments' 'crawl_envs'
  'destandardize_env_name' 'error_NotValidEnvironment'
  'extract_last_member' 'get_namespace_addresses'
  'get_obj_addresses_from_obj_names' 'is_logical' 'is_memory_address'
  'is_string' 'parse_memory_address' 'standardize_env_name'
  'unlist_with_names'
 