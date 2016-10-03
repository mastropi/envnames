# Auxiliary functions for the envnames package

#' Crawl environments in search of other environments
#' 
#' Function that crawls a set of environments to search for environments defined within each of them
#' 
#' @param env_names array with the environment names where the search for environments is wished (careful: should not contain the environment *objects* but their *names*!).
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
      # No environments were found => return the TRUE to indicate that we reached a leaf in the environments tree
      return(TRUE)
    }
  }
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

#' Check whether a string is a memory address
#' 
#' Check whether an object represents a valid memory address. If the object does not exist or is not of the
#' correct type FALSE is returned, no error is raised. 
#' 
#' @param x object to check.
#' @return boolean indicating whether the given object represents a valid memory address. 
is_memory_address = function(x) {
	result = FALSE

	ischaracter = try( is.character(x), silent=TRUE )
	if (!inherits(ischaracter, "try-error") && ischaracter) {
		# Check if x contains a string that is a memory address
		# We allow for one or more spaces at the beginning of the string as in "   <0x00000000119dba68>"		
		# Note that the blank space at the beginning of the pattern includes tabs (checked).
		# Note also that if we want to use PERL regular expression we should use double escape to represent
		# special characters as in grep("^\\s*<", obj, perl=TRUE)
		isaddress = grep("^ *<[0-9a-f]{16}>$", x, ignore.case=TRUE)
		if (length(isaddress) > 0) {
			result = TRUE
		} else {
			result = FALSE
		}
	}

	return(result)
}

#' Standardize the name of an environment
#' 
#' This function standardizes the name of an environment so that environment names are consistent
#' with the output of base function \link{environmentName}.
#' It only has an effect when the environment is the global environment or the base environment,
#' which have different ways of referring to them, namely:
#' \code{globalenv()}, \code{.GlobalEnv}, \code{baseenv()}, \code{as.environment("package:base")}
#' 
#' @param env_name environment name to standardize.
#'
#' @return The name of the environment, where the global environment is represented as "R_GlobalEnv" and the
#' base environment is shown as "base".
standardize_env_name = function(env_name) {
	# Use "R_GlobalEnv" for the global environment and "base" for the base environment
	# to be consistent with the output of environmentName()
	env_name = gsub("\\.GlobalEnv|globalenv\\(\\)", "R_GlobalEnv", env_name)
	env_name = gsub("package:base|baseenv\\(\\)", "base", env_name)
	return(env_name)
}

#' De-standardize the name of an environment
#' 
#' This function inverts the process performed by \link{standardize_env_name} that is, it converts
#' the standardized names "R_GlobalEnv" and "base" back to names that are recognized by R as actual
#' environments when using function \link{as.environment}, namely to \code{".GlobalEnv"} and \code{"package:base"}.
#' 
#' @param env_name environment name to de-standardize.
#'
#' @return The name of the environment, where the global environment is represented as ".GlobalEnv" and the
#' base environment is represented as "package:base".
destandardize_env_name = function(env_name) {
	# Use "R_GlobalEnv" for the global environment and "base" for the base environment
	# to be consistent with the output of environmentName()
	env_name = gsub("R_GlobalEnv", ".GlobalEnv", env_name)
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
unlist_with_names = function(alist) {
	# Get the lengths ana names of each list element
	lengths = sapply(alist, FUN=length)					# Number of elements in each list element
	arr_names = rep(names(lengths), lengths)		# Repeat the name of each list element as many times as their lengths so that we can apply them to the array resulting from the unlist of the list

	# Unlist the list into an array and add back the names of each list element as the array names 
	arr = unlist(alist, use.names=FALSE)				# Unlist the list into an array and...
	names(arr) = arr_names											# use the names to identify where each element came from in the original list.
	
	return(arr)
}
