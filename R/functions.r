# Auxiliary functions for the envnames package

#' Crawl environments in search of other environments
#' 
#' Function that crawls a set of environments to search for environments defined within each of them
#' 
#' @param env_names array with the environment names where the search for environments is wished
#' (careful: should not contain the environment *objects* but their *names*!).
#' @param path array with environment names leading to the current set of environments listed in \code{env_names}.
#' @param env_path_list array or list of environment names found so far including their path
#' (e.g. an element of \code{env_path_list} could be \code{"env1$env2$env"}).
#' The first time this function is called it should contain the empty array or the empty list.
#' @param envir environment used as starting point for the environment search process.
#' 
#' @details
#' This function crawls the environments defined in the \code{envir} environment and any environments
#' within those environments until no environment has been left without visit, making up an environments tree.
#' The path to each environment found is stored using the \code{$} notation as in \code{env1$env12$envx}.
#' 
#' Instead of setting the \code{envir} parameter to the root environment where the search should start,
#' a similar result is obtained by setting the \code{path} variable to the environment chain leading to
#' the environment that should be passed to \code{envir}, as in e.g. \code{c("env_of_envs", "env11")}.
#' The difference is that such environment will appear as part of the paths to the environments passed,
#' and in addition, it is assumed that such environment is an actual environment.
#' environment given by the chained names passed in \code{path} is an actual environment, and 
#' 
#' @return An array containing the path to each environment found inside the \code{envir} environment
#' and its sub-environments. This array is concatenated to whatever paths are already listed in the
#' \code{env_path_list} object passed to the function.
#' 
#' @keywords internal
crawl_envs = function(env_names, path, env_path_list, envir=.GlobalEnv) {
  # Before going through the different environments in env_names, add the current path to the environment list
  # (because we need to keep all the nodes of the environments tree, not only the leaves of the tree)
  if (!is.null(path)) {
    env_path_list = c(env_path_list, paste(path, collapse="$", sep=""))
  }

  # Crawl on the environments listed in env_names
  for (env_name in env_names) {
		# IMPORTANT: Check if the environment referenced by env_name does NOT coincide with any of the environments
		# already seen and stored in path, o.w. we would go through an infinite recursion!
		if (!(env_name %in% path)) {
			# Look for other environments in the currently analyzed environment
			result = get_envs(env_name, path, env_path_list, envir=envir)

			# Check the result returned by get_envs() which tells us whether:
			# - result = NULL: the environment given in env_name is not an environment
			#   => nothing to do
			# - result is not logical: more environments were found in the environment referenced by env_name and they
			# were added to 'result' by recursion
			#   => 'result' should be the updated env_path_list
			# - result is logical (TRUE): no more environments were found inside the environment referenced by env_name
			#   => the env_name environment should be added to env_path_list
			if (!is.null(result)) {
				if (!is.logical(result)) {
					# The result of get_envs() is the updated env_path_list which means that we are in the middle of the tree
					env_path_list = result
				} else {
					# The result of get_envs() is TRUE, meaning that we reached a leaf of the environments tree
					# => Add the path to the currently analyzed environment as a string (e.g. "env1$env3$thisenv")
					env_path_list = c(env_path_list, paste(c(path, env_name), collapse="$", sep=""))
				}
			}
		}
	}

	return(env_path_list)
}

#' Crawl environments in search of user-defined environments
#' 
#' @param envs environment to search in. Note that these should be an environment, and NOT an environment name.
#' 
#' @return An array containing strings that represent the path to each environment found inside the \code{env}
#' environment, and its sub-environments.
#' 
#' @keywords internal
crawl_envs_in_env = function(env) {
	env_names = try( with(env, Filter(function(x) "environment" %in% class(get(x)), ls())), silent=TRUE )
	#env_names = try( Filter(function(x) "environment" %in% class(get(x, envir=env, inherits=FALSE)), ls(env)), silent=TRUE )
	# Crawl the environments defined inside each environment listed in envs
	env_path_list = crawl_envs(env_names, c(), c(), envir=env)

	return(env_path_list)
}

#' Find environments in a given environment
#' 
#' Function that looks for the environments defined within a given environment
#' 
#' @param env_name name of the environment where the search for environments should be carride out.
#' @param path array with environment names leading to the \code{env_name} environment.
#' @param env_path_list array or list of environment names found so far including their path.
#' @param envir root environment where the search for environments started.
#' 
#' @return Either an updated \code{env_path_list} with the environments found so far in the environments tree
#' or TRUE, which indicates that a leaf in the environments tree found inside the \code{envir} environment
#' has been reached.
#' 
#' @keywords internal
get_envs = function(env_name, path, env_path_list, envir=.GlobalEnv) {
	# Update the path array by adding the name of the currently analyzed environment
	path_to_here = c(path, env_name)

	# Name of the environment (with the full path, so that it can be converted to an environment with eval())
	env_path = paste(path_to_here, collapse="$", sep="")
	
	# Get the environment referenced by the env_path name in the envir environment
	# IMPORTANT NOTES:
	# - we use get() instead of 'env = eval(parse(text=env_path), envir=envir)' to get the environment
	# 'env' because eval() does NOT accept the inherits=FALSE argument, which is needed to analyze the
	# environment ONLY if it is inside the 'envir' environment.
	# (get() gives an error if the variable referenced by the env_path text is not found in 'envir'
	# therefore we enclose it in a try block)
	# - in order to 'get' the variable value, the variable name needs to be on its own (e.g. it cannot be
	# passed as env1$env) o.w. it is not found. This means that we need to make up the evaluation environment
	# concatenating the envir environment with the environments chained in 'path'. This is the environment
	# stored in envir_full.
	# - the envir_full can be constructed by using "envir" explicitly in the parse argument inside the eval()
	# function... I don't know why this works... (since we should not concatenate "envir" per se but
	# the name of the environment **stored** in 'envir'... but I think it has to do with the fact that
	# eval() tries to evaluate an expression in all possible environments...
	tryOutput = try(
	  {
  	  envir_full = eval( parse(text=paste(c("envir", path), collapse="$")) )	# This is weird... why it works!
      env = get(env_name, envir=envir_full, inherits=FALSE)
  	}
    , silent=TRUE)

  if (inherits(tryOutput, "try-error")) {
    return(NULL)
  } else {
    # List the environments present inside 'env'
    # Note that the following Filter() returns the environment names!! (i.e. as string, nice!)
    env_names = try( with(env, Filter(function(x) "environment" %in% class(get(x)), ls())), silent=TRUE )
    
    # Check if any environments were found
    if (length(env_names) > 0) {
      # Environments were found inside 'env' => recurse on them
      env_path_list = crawl_envs(env_names, path_to_here, env_path_list, envir=envir)
      # Return the updated list of environments found via crawl_envs() including their path (e.g. "env1$env2$thisenv")
      return(env_path_list)
    } else {
      # No environments were found => return TRUE to indicate that we reached a leaf in the environments tree
      return(TRUE)
    }
  }
}

#' Get the object addresses given their object names
#' 
#' @param obj_names array containing the name of the objects to retrieve.
#' They can be given as full path names (e.g. "env1$x") and they can also be
#' system or package environments given as e.g. ".GlobalEnv" or "R_GlobalEnv" or
#' "baseenv()" or "package:base" or "package:stats", etc. 
#' @param envir environment where the objects to convert exist (considering the
#' object names are given with their full path to the object). This information is not
#' used when the object is a system or package environment.
#' 
#' @return An array containing the memory address of the objects given in \code{obj_names}
#' or NULL if there is a problem evaluating the corresponding object in the given environment
#' with \code{eval()}.  
#' 
#' @keywords internal
get_obj_addresses_from_obj_names = function(obj_names, envir=.GlobalEnv) {
	# Get the objects referenced by the obj_names names
	# NOTE that we use eval() instead of get()
	# (in which case we would have used envs = lapply(env_full_names, get, envir=envir))
	# because eval() can evaluate environments given as e.g. env_of_envs$env11
	# but get() cannot... get() does NOT accept the '$' sign, but accepts only calls of the form
	# get("env11", envir=env_of_envs).
	objects = lapply(obj_names, function(x) {
				x_eval = try( eval(parse(text=x), envir=envir), silent=TRUE )
				if (inherits(x_eval, "try-error")) {
					# Check if x can be evaluated as an environment (e.g. when x = ".Globalenv()" or "package:base")
					x_eval = try( as.environment(destandardize_env_name(x)), silent=TRUE )
					if (inherits(x_eval, "try-error"))
						return(NULL) 
				}
				return(x_eval)
			})
	obj_addresses = eval( unlist( lapply(objects, get_obj_address, envir) ), envir=envir)
	return(obj_addresses)
}

#' Get the objects defined in a given package's namespace
#' 
#' @param package_name string containing the package name (e.g. "envnames") of interest.
#'
#' @return An array containing the objects defined in a package namespace, as obtained
#' by \code{ls(asNamespace(package_name))}, or NULL if the package does not exist. 
#' 
#' @keywords internal
get_objects_in_package = function(package_name) {
	objects = try( ls(asNamespace(package_name)), silent=TRUE )
	if (inherits(objects, "try-error"))
		return(NULL)
	return(objects)
}

#' Extract the last member in a string representing an object
#' 
#' Function that extracts the last name in a string representing an object as in e.g. \code{obj$x$y}.
#' It returns a list containing both the piece before the last occurrence of \code{$} and the name coming after it.
#' The object may or may not exist.
#' 
#' @param full_name character scalar from which we should keep the piece that comes after the last \code{$} sign.
#' 
#' @return A list with two members: \code{root} containing the path to the last name of the object expression
#' and \code{name}, containing the last name in the object expression.
#' The \code{root} is empty when there is no \code{$} in the input string.
#' Ex: \code{extract_last_member("obj$x$y")} returns \code{list(root="obj$x", name="y")}.
#' 
#' @keywords internal
extract_last_member = function(full_name) {
	if (!is.character(full_name)) {
		return(NULL)
	} else {
		# Find the position of the last $ sign (e.g. when the function call contains environment names which can
		# be possibly chained as in env1$env11$f)
		pos_dollar = unlist(gregexpr("\\$", full_name))		# this is function is 'g-regexpr()' i.e. "g" stands for "global" regular expression search; gregexpr() returns -1 when no match is found.
		pos_last_dollar = pos_dollar[length(pos_dollar)]	# get the position of the last $ sign
		if (pos_last_dollar > 0) {
			name = substr(full_name, pos_last_dollar+1, nchar(full_name))
			root = substr(full_name, 1, pos_last_dollar-1)
		} else {
			name = full_name
			root = ""
		}
	}
	return(list(root=root, name=name))
}

#' Clean up the list of environments matching a memory address in an environment map.
#'
#' @param envmap data frame with the name-address pairs of environments having at least a column
#' called "type" that specifies the type of environment, for which "function" is used to indicate
#' a function execution environment.  
#' @param indfound array containing the indices in envmap giving the environments to clean up
#' (typically these are the environments that match a given memory address).
#' @return an array containing the indices in \code{indfound} after cleanup.
#' 
#' @details
#' A clean list of matched environments from \code{envmap} should either:
#' \itemize{
#' \item contain ONLY ONE function execution environment,
#' \item contain one ore more user-defined or named environments.
#' }
#' If none of the above is the case, all function execution environments are removed from the list
#' of matching environments, i.e. removed from the \code{indfound} array. 
#' 
#' @keywords internal
clean_up_matching_environments = function(envmap, indfound) {
	# If the memory address of the matched environments correspond to a function execution environment,
	# there should be only ONE occurrence in indfound...
	# If this is not the case it means that the execution environment of the function is a proper environment
	# (i.e. a user environment or named environment) and that environment should be the one to retrieve
	# => we should eliminate all the matches of function execution environments from indfound)
	# So far I observed this when eval() is one of the functions in the calling chain for which the global environment
	# is the execution environment (probably because the eval() function is asked to evaluate an expression in the global environment)
	if ("function" %in% envmap[indfound,"type"] && length(indfound) > 1 ) {
		# Keep in indfound just the index of the environment that is NOT a function execution environment
		# because that's the one we want to retrieve (e.g. the global environment)
		indfound = indfound[ envmap[indfound, "type"] != "function" ]
	}

	return(indfound)
}

#' Check whether a string corresponds to the name of an environment
#' 
#' The input string is checked as a valid environment name. The environment can be a named environment
#' (system or package) or a user-defined environment.
#' 
#' @param x string to evaluate that may represent an environment
#' @param envir environment where \code{x} should be evaluated first. If it is not found there
#' it still searched for in the whole workspace.
#'
#' @return A list with two elements:
#' \itemize{
#' \item{\code{found}} whether the string contained in \code{x} is the name of an existing environment
#' in the workspace
#' \item{\code{env_name}} the name of the environment (after stripping out any system environments such
#' as globalenv(), baseenv() or a package environment) (for instance "globalenv()$env$env1" is returned
#' as "env$env1"), or NULL if no environment was found corresponding to the name given in \code{x}.
#' }
#' 
#' @keywords internal
check_environment = function(x, envir) {
  # Initialize the output variables
  found = FALSE
  env_name = NULL

  # The check whether the string x is an environment name is done via an envmap lookup table computed
  # on the whole workspace.
  # Note that we need to do all this and cannot simply check whether is.environment(x) is TRUE
  # because x is a string!! (not an environment per se, only what the string refers to may be
  # an environment).

  # Get the address of the "presumed" environment in environment 'envir'
  x_address = try( address( eval(parse(text=x)), envir=envir ), silent=TRUE )
  if (inherits(x_address, "try-error")) {
    # Search for the environment in the whole workspace
    # (to make sure that the environment does not exist...
    # i.e. maybe it does not exist in the envir environment
    # but it exists somewhere in the whole workspace)
    x_address = try( address( eval(parse(text=x)) ), silent=TRUE )

    if (!inherits(x_address, "try-error")) {
      # Search for the address in the envmap lookup table constructed for the whole workspace
      # (note that we should create the table on the whole workspace because the object may be referred to
      # as e.g. globalenv()$y or baseenv()$y or as.environment("package:envnames")$e, etc., and creating
      # the lookup table for the calling environment (+n levels) would NOT include any packages in the lookup table)
      envmap_all = get_env_names()
      ind = which(envmap_all[,"address"] == x_address)
  		# Clean up the matched environments: in the case both "function" and "proper" environments matched, keep just the "proper" environments
  		ind = clean_up_matching_environments(envmap_all, ind)		
      if (length(ind) > 0) {
        env_name = envmap_all[ind[1], "pathname"]
          ## NOTE: ind[1] means: keep just the first occurrence found. There could more than one occurrence when several variables
          ## point to the same environment (and therefore they have the same memory address). So, here we just keep the
          ## first match found. Otherwise, multiple matches could generate a problem with the processing done outside.
        found = TRUE
      }
    }
  }

  return(list(found=found, env_name=env_name))
}

#' Check if an object name contains a valid environment path
#' 
#' Check if a string may represent a valid object name with full environment path to the object
#' as in \code{globalenv()$env$x}. The string should \emph{not} end with \code{]} or \code{)} because
#' that makes the whole expression an invalid name and therefore it should not be considered as a name
#' (e.g. \code{globalenv()$v[1]} refers to element 1 of array v and such thing is not the name of an object).
#' 
#' Optionally a check of whether the path points to a valid environment inside a given environment
#' is performed by calling \code{}.
#' 
#' @param x string to be checked.
#' @param checkenv flag indicating whether the environment path should be checked for valid environment
#' as well. Defaults to FALSE.
#' @param envir environment where \code{x} should be evaluated when also checking the environment.
#' Only used when \code{checkenv=TRUE}.
#'
#' @return A list containing the following elements:
#' 
#' When \code{checkenv=FALSE}:
#' \itemize{
#' \item{\code{ok}} boolean indicating whether the string may be a valid object name
#' \item{\code{path}} path to the name
#' \item{\code{name}} the name itself with no path.
#' }
#' 
#' When \code{checkenv=TRUE} and when the string given in \code{x} is deemed to be a possible valid object,
#' the following additional elements:
#' \itemize{
#' \item{\code{env_found}} flag indicating whether the string indicating the path to the object name
#' referenced by \code{x} is the name of an existing environment in the workspace
#' \item{\code{env_name}} the name of the environment (after stripping out any system environments such
#' as globalenv(), baseenv() or a package environment) (for instance "globalenv()$env$env1" is returned
#' as "env$env1"), or NULL if no environment is found corresponding to the path given in \code{x}.
#' }
#' 
#' @keywords internal
check_object_with_path = function(x, checkenv=FALSE, envir=NULL) {
  # Outpu variables
  ok = FALSE
  x_root = NULL
  x_name = NULL
  check_path = list(env_found=FALSE, env_name=NULL)

  # Check if the object name contains '$' separators (as in globalenv()$env1$x) but it does NOT end
  # with "]" or ")" which would mean that it actually refers to an array
  # or function and we should not consider it as a valid name (e.g. globalenv()$env1$v[1])
  # (because it refers to something else, e.g. element 1 of an array)
  x_name_ok = grep("\\$.*[^]|)]$", x)
  if (length(x_name_ok) > 0) {
    # Extract the presumed environment name from the expression (e.g. from "globalenv()$env1$x")
    x_root_and_name = extract_last_member(x)
    x_root = x_root_and_name$root
    x_name = x_root_and_name$name
    ok = TRUE
    
    if (checkenv) {
      # Check now whether x_root is a valid environment name
      check_path = check_environment(x_root, envir)
    }
  }

  if (checkenv) {
    return(list(ok=ok, path=x_root, name=x_name, env_found=check_path$found, env_name=check_path$env_name))
  } else {
    return(list(ok=ok, path=x_root, name=x_name))
  }
}

#' Check if an object exists in a given environment
#' 
#' Check if an object exists in a given environment or any parent environment from the given environment in
#' the way that the \link{eval} function does by default.
#' 
#' @param obj object to check. It can be given as a symbol or as a string.
#' @param envir environment where its existence is checked.
#' 
#' @details
#' If the object is not found in the \code{envir} environment it is searched in any parent environment
#' of \code{envir}.
#' 
#' @return A list containing three elements:
#' \itemize{
#' \item{\code{found}} flag indicating whether the object was found
#' \item{\code{eval}} result of the evaluation of \code{obj} either in \code{envir} or in a parent environment
#' where it was found.
#' \item{\code{address}} memory address of the object found. IMPORTANT: This memory address is NOT the object's memory
#' address when the object is given as a string, as in that case the memory address contains the memory address of the
#' string! (which varies every time, even if the string is the same, because every time the string allocates a different
#' memory address)
#' }
#' 
#' @keywords internal
check_object_exists = function(obj, envir=globalenv()) {
  # Check first if the object is NULL or NA, in which case we should return that the object does not exist
  # We must enclose the check expression in a try() block because obj may not exist in the evaluation environment, although
  # it may exist in other environments and this is what this function is all about --i.e. about finding where the object exists)
  is_obj_null_na = is_null_or_na(obj)
  if (is_obj_null_na) {
    obj_eval = NULL
    obj_address = NULL
    found = FALSE
  } else {
    # The existence of the object is checked by trying to evaluate the object in the 'envir' environment
    # We first evaluate the object before retrieving its memory address to be on the safer side, because
    # the memory address may have a value even if the object does not exist or the evaluated argument is not
    # an object (for instance a number has a memory address)
    # Note that NULL and NA values have been already screened above
    obj_eval = try( eval(substitute(obj), envir=envir), silent=TRUE )
    if (!inherits(obj_eval, "try-error")) {
      found = TRUE
      obj_address = try( eval( parse(text=paste("address(", deparse(substitute(obj)), ")")), envir=envir ), silent=TRUE )
    } else {
      # Try evaluating the object without restricting it to 'envir'
      # We would get here if the user e.g. called obj_find() using a with() statement but
      # explicitly referring to e.g. the global environment as in:
      #   with(env1, obj_find(globalenv()$env2$x))
  		# We implement this behaviour because if we run with(env1, globalenv()$env2$x)
  		# we will get the value of x inside the globalenv()$env2 environment.
  		obj_eval = try( eval(substitute(obj)), silent=TRUE )
  		if (!inherits(obj_eval, "try-error")) {
  		  found = TRUE
  		  obj_address = try( eval( parse(text=paste("address(", deparse(substitute(obj)), ")")) ), silent=TRUE )
  		} else {
        # No more options to try at this point!
        obj_eval = NULL
        obj_address = NULL
        found = FALSE
      }
    }
  }

  return(list(found=found, eval=obj_eval, address=obj_address))
}

#' Return the memory address of namespace environments
#' 
#' This function returns the memory address of all the namespace environments of the packages found
#' in the \code{search()} path.
#' 
#' @return Array containing the namespace addresses as values and the package names as names (in the form
#' of e.g. "package:base")
#' 
#' @keywords internal
get_namespace_addresses = function() {
  unlist( sapply(search(), function(x) { 
    if(length(grep("^package:", x)) > 0) {
      namespace = getNamespace(gsub("^package:", "", x))
      namespace_address = get_obj_address(namespace, envir=.GlobalEnv)
      return(namespace_address)
    }
  }) )
}

#' Check whether a string is a memory address
#' 
#' Check whether an object represents a valid memory address. If the object does not exist or is not of the
#' correct type FALSE is returned, no error is raised. 
#' 
#' @param x object to check.
#' @return boolean indicating whether the given object represents a valid memory address.
#' 
#' @details
#' Valid memory addresses depend on the architecture. For instance for:
#' - for Windows 32-bit systems, an 8-bit representation (since 2^32 = 16^8)
#' - for Windows 64-bit systems, a 16-bit representation (since 2^64 = 16^16)
#' - for Linux Debian 64-bit systems, a 12-bit representation seems to be the case...
#' (ref: Ubuntu 18.04 LTS on Windows)
#' 
#' Example of valid memory addresses for Windows 64-bit systems:
#' "<(16-digit-code)>" (e.g. "<000000000974E880>")
#' "<0x(16-digit-code)>" (e.g. "<0x000000000974E880>")
#' "<environment: 0x(16-digit-code)>" (e.g. "<environment: 0x000000000974E880>")
#' 
#' @keywords internal
is_memory_address = function(x) {
	result = FALSE

	ischaracter = try( is.character(x), silent=TRUE )
	if (!inherits(ischaracter, "try-error") && ischaracter) {
		# Check if x contains a string that is a memory address
		# We allow for one or more spaces at the beginning or end of the string as in "   <0x00000000119dba68>  "
		# Note that the blank space at the beginning or end of the pattern includes tabs (checked).
		# Note also that if we want to use PERL regular expression we should use double escape to represent
		# special characters as in grep("^\\s*<", obj, perl=TRUE)
		isaddress = grep( paste("^ *<[0-9a-f]{8,16}> *$", sep=""), x, ignore.case=TRUE) ||
		            grep( paste("^ *[0-9a-f]{8,16} *$", sep=""), x, ignore.case=TRUE) ||
		            grep( paste("^ *<0x[0-9a-f]{8,16}> *$", sep=""), x, ignore.case=TRUE) ||
		            grep( paste("^ *0x[0-9a-f]{8,16} *$", sep=""), x, ignore.case=TRUE) ||
          		  grep( paste("^ *<environment: 0x[0-9a-f]{8,16}> *$", sep=""), x, ignore.case=TRUE)
		if (!is.na(isaddress) && isaddress) {
			result = TRUE
		} else {
			result = FALSE
		}
	}

	return(result)
}

#' Parse a string that represents a memory address
#' 
#' Parse a string representing a memory address so that the address is returned in the way it is stored
#' in the 'address' column of the data frame returned by get_env_names(). 
#' 
#' @param x string to parse.
#' @return string containing the memory address represented by the input string after stripping any
#' extraneous pieces of string, namely : "0x" and "environment: " and after enclosing it in '<>'.
#' For 32-bit architecture the string would be of the form "<xxxxxxxx>" where x represents
#' digits between 0 and 9 and letters between "a" and "f". Ex: "<07830f40>"
#' For 64-bit architecture the string would be of the form "<xxxxxxxxxxxxxxxx>" where x represents
#' digits between 0 and 9 and letters between "a" and "f". Ex: "<07830f40>"
#' 
#' @keywords internal
parse_memory_address = function(x) {
  if (!is_memory_address(x)) {
    return(NULL)
  } else {
    x = gsub("<environment: ", "<", x)
    x = gsub("0x", "", x)
    if (length( grep("^ *<", x) ) == 0) {
      # Enclose the memory address in '< >'
      x = paste("<", x, ">", sep="")
    }
    return(x)
  }
}

#' Check whether an object contains a valid logical value
#' 
#' @param x object to check. A scalar is assumed.
#' @return boolean indicating whether the scalar object contains a valid logical value (i.e. TRUE or FALSE)
#' and is not NA nor NULL, and has positive length.
#' 
#' @keywords internal
is_logical = function(x) {
  return(!is.null(x) && !is.na(x) && is.logical(x) && length(x) > 0)
}

#' Check whether an object is NULL or NA.
#' 
#' This function silently handles special cases for which is.null() and is.na() may return a warning, such as functions objects.
#' (e.g. the warning "Warning message: In is.na(g) : is.na() applied to non-(list or vector) of type 'closure')"
#' 
#' @param x object to check.
#' @return boolean indicating whether the object is NULL or NA.
#' 
#' @keywords internal
is_null_or_na = function(x) {
  op.warn = options("warn")$warn; on.exit( options(warn=op.warn) )
  options(warn=-1)
  output = try( is.null(x) || (is_logical(is.na(x)) && is.na(x)), silent=TRUE )
  if (inherits(output, "try-error")) output = FALSE
  return(output)
}

#' Check whether an object is a string.
#' 
#' WARNING: This function fails when the value of x is "x"!! (i.e. it returns TRUE even when object 'x' is NOT a string per se --i.e. it was not passed as "x")
#' 
#' The result of this function is different from is.character(x) since this function returns TRUE
#' for an array of character values!
#' 
#' @param x object to check.
#' @return boolean indicating whether the object is a string.
#' 
#' @keywords internal
is_string = function(x) {
  # TODO: Fix the WARNING mentioned above... is it possible?
  # Note that we evaluate the object name two environments up, this is because we want to get the name of the object
  # *stored* in the variable used to call is_string(). Ex: if we call is_string(obj), we don't want the name of 'obj'
  # (which would be the result if we used n=1) but the name of the object *stored* in 'obj' (e.g. "env1$x")... and this
  # is obtained by using n=2.
  return(get_obj_name(x, n=2, eval=FALSE) == x)
}

#' Standardize the name of an environment
#' 
#' This function standardizes the name of an environment so that environment names are consistent
#' with the output of base function \link{environmentName}.
#' It only has an effect when the environment is the global environment, the empty environment,
#' or the base environment, which have different ways of being referring to, namely:
#' \code{globalenv()}, \code{.GlobalEnv}, \code{emptyenv()},
#' \code{baseenv()}, \code{as.environment("package:base")}
#' 
#' @param env_name environment name to standardize.
#'
#' @return The name of the environment, where the global environment is represented as "R_GlobalEnv",
#' the empty environment as "R_EmptyEnv", and the base environment as "base".
#' 
#' @keywords internal
standardize_env_name = function(env_name) {
	# Use:
  # "R_GlobalEnv" for the global environment
  # "R_EmptyEnv" for the empty environment
  # "base" for the base environment
	# to be consistent with the output of environmentName()
	env_name = gsub("\\.GlobalEnv|globalenv\\(\\)|globalenv", "R_GlobalEnv", env_name)
	env_name = gsub("\\.EmptyEnv|emptyenv\\(\\)|emptyenv", "R_EmptyEnv", env_name)
	env_name = gsub("package:base|baseenv\\(\\)|baseenv", "base", env_name)
	return(env_name)
}

#' De-standardize the name of an environment
#' 
#' This function inverts the process performed by \link{standardize_env_name} that is, it converts
#' the standardized names "R_GlobalEnv", "R_EmptyEnv", and "base" back to names that are recognized by R as actual
#' environments when using function \link{as.environment}, namely to \code{".GlobalEnv"}, \code{".EmptyEnv"},
#' and \code{"package:base"}.
#' 
#' @param env_name environment name to de-standardize.
#'
#' @return The name of the environment, where the global environment is represented as ".GlobalEnv", the empty
#' environment as ".EmptyEnv", and the base environment as "package:base".
#' 
#' @keywords internal
destandardize_env_name = function(env_name) {
	env_name = gsub("R_GlobalEnv", ".GlobalEnv", env_name)
	env_name = gsub("R_EmptyEnv", ".EmptyEnv", env_name)
	env_name = gsub("base", "package:base", env_name)
	return(env_name)
}

#' Call unlist and preserve the names
#' 
#' Function \code{unlist} is called so that the output is an array (whenever possible) whose names
#' attribute contains the names of the elements of the list (unchanged, i.e. without adding a number
#' to identify them as the regular \code{unlist()} function does).
#' 
#' @param alist list to unlist.
#' 
#' @return Whenever possible, an array whose names attribute is set to the names of the elements of the list.
#' 
#' @keywords internal
unlist_with_names = function(alist) {
	# Get the lengths ana names of each list element
	lengths = sapply(alist, FUN=length)					# Number of elements in each list element
	arr_names = rep(names(lengths), lengths)		# Repeat the name of each list element as many times as their lengths so that we can apply them to the array resulting from the unlist of the list

	# Unlist the list into an array and add back the names of each list element as the array names 
	arr = unlist(alist, use.names=FALSE)				# Unlist the list into an array and...
	if (!is.null(arr)) {  # It's important to check this because the array resulting from the call to unlist() above may be NULL (meaning that it is NOT an array!) (this may happen even if 'alist' is not null, for instance when all its elements are NULL!)
	  names(arr) = arr_names										# Use the names to identify where each element came from in the original list.
	}
	
	return(arr)
}
