# Auxiliary functions for the envnames package

#' Get the name of an environment when the address-name lookup table has not yet been constructed
#' 
#' @param env environment whose name is of interest
#'
#' @return the name of the environment as given by \code{environmentName()} if it is a named environment
#' or the name of the environment as given by \code{deparse(substitute())} otherwise.
#' This will result in a string representing an expression when 'env' is given as an expression as in
#' \code{parent.env(env1)}.
#' 
#' @keywords internal
get_environment_name = function(env) {
  # First try retrieving the environment name as if it were a named environment
  env_name = environmentName(env)
  if (env_name == "") {
    # This means 'env' is NOT a named environment, but a user environment
    # => deparse it.
    # Note that this will only work when the input parameter 'env' is given
    # with a variable name and not via an expression, as in e.g. 'parent.env(env1)'
    # in which case env_name will result in that same expression "parent.env(env1)".
    # But this is the best we can do... (because this function is intended to be used
    # when the address-name lookup table of environments has NOT yet been constructed)
    env_name = deparse(substitute(env))
  }

  return(env_name)
}

#' Return the system and package environments in the search path
#' 
#' @return a named array containing the addresses of the system and package environments
#' found in the \code{search()} path with the environment names as its names attribute.
#' 
#' @keywords internal
get_searchpath_environment_addresses = function() {
  return( 
    vapply( search(),
            function(x) { get_obj_address(as.environment(x), envir=.GlobalEnv) },
            FUN.VALUE=character(1)
          )
            ## NOTE: FUN.VALUE in the vapply() function is a required parameter.
            ## It specifies the type and length of the value returned by the function called by vapply().
            ## In this case (FUN.VALUE=character(1)) we are saying that the function should return
            ## a vector of length 1 of type character.
  )
}

#' Return the names that can be used to load all the namespace environments in the search() list
#' 
#' This function returns the names of all the namespace environments of the packages found
#' in the \code{search()} path.
#' 
#' @return Array containing the names of the namespace environments as values and the package names as names
#' (in the form of e.g. "package:base"). The names of the namespace environments are the result of splitting
#' the name in the search list by ":" and taking the second part of the string 
#' (e.g. "base" from "package:base" or "rstudio" from "tools:rstudio").
#' The idea is that this name gives the namespace enviroment when used as \code{asNamespace(<name>)} or
#' \code{getNamespace(<name>)}, as in e.g. \code{asNamespace("base")}.
#' 
#' @keywords internal
get_namespace_names = function() {
  unlist( sapply(search(), function(x) { 
    splitted_name = strsplit(x, ":")  # This returns a list with the splitted parts as values of each list element
    # e.g. for "package:base" it returns:
    # [[1]]
    # [1] "package" "base" 
    if(length(splitted_name) > 0 && length(splitted_name[[1]]) > 1) {
      # A ':' was found in x (e.g. "package:base" or "tools:rstudio")
      # => Retrieve the name of the package as the text following ':'
      name = splitted_name[[1]][2]
      return(name)
    }
  }) )
}

#' Return the memory address of namespace environments in the search() list
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

#' Look for user environments defined inside any of the system/package environments
#' of the search path
#' 
#' @return an array containing the names of the user environments found
#' inside any of the system/package environments in the search path (retrieved by \code{search()}).
#' The names attribute of the array is the system/package environment where the
#' environment is found.
#' \code{NULL} is returned if the process fails or no environments are found.
#' 
#' @keywords internal
get_user_environment_names_in_search_path = function() {
  env_user_names = try( {
      # List of user environments defined in each system environment and package
      envs_list = sapply(  	search(),
                            FUN=function(package) {
                              # IMPORTANT: a package called "CheckExEnv" should be excluded from this analysis
                              # because o.w. testing of the package during the Check process will fail with the
                              # error "Error in get(x, envir = as.environment(package)) : F used instead of FALSE"
                              # which is caused by an object called "F" in the CheckExEnv environment created
                              # by the package Check process... (and there is another object called "T", I guess
                              # an alias for TRUE). I don't know what this means...
                              if (package != "CheckExEnv") {
                                Filter(function(x) "environment" %in% class(get(x, envir=as.environment(package))),
                                       ls(envir=as.environment(package)))
                              }
                            }
      )
      envs_array = unlist_with_names(envs_list)
      # Standardize the names of the environment so that the global and the base environments are always shown
      # the same way, regardless of how the 'envir' parameter is passed.
      names(envs_array) = sapply(names(envs_array), FUN=standardize_env_name)
      envs_array
    },
    silent=TRUE)

  if (inherits(env_user_names, "try-error")) {
    return(NULL)
  } else {
    return(env_user_names)
  }
}

#' Get the user environments defined recursively within the given user environments
#' 
#' @param env_user_names names of the user environments where the recursive search is carried out.
#' @param envir user environment where ALL environments in \code{env_user_names} live,
#' or \code{NULL} if those environments may live in different environments. In this case
#' the names of the environment where each \code{env_user_names} live is taken
#' from its names attribute.
#' 
#' @return a list containing the following elements:
#' \itemize{
#' \item{fullnames} the full names of the environments
#' \item{addresses} the addresses of the environments
#' \item{locationaddresses} the addresses of the locations of the environments
#' }
#' 
#' @keywords internal
get_user_environments_in_user_envs_recursively = function(env_user_names, envir) {
  env_fullnames_user_recursive = unlist_with_names(
                                    tapply(env_user_names,
                                           INDEX=names(env_user_names),  # This is the BY group of the analysis
                                           FUN=crawl_envs, c(), c(), envir)
                                 )

  # Get the memory addresses of the environments just retrieved
  env_addresses_user_recursive = get_obj_addresses_from_obj_names(env_fullnames_user_recursive, envir=envir)

  # Add the memory address of the LOCATIONS of all user environments found inside other user environments
  # done-2018/10/21 (the first statement seems to work in all cases): (2017/10/15) Need to make this work for both user and system environments...
  # - the first statement only works for user environments
  # - the second statement only works for system/package environments
  env_locationaddresses_user_recursive = get_obj_addresses_from_obj_names(names(env_fullnames_user_recursive), envir=envir)
  #env_locationaddresses_user_recursive = sapply( sapply( sapply(names(env_fullnames_user_recursive), destandardize_env_name), as.environment), address )

  # Standardize the environment names used as names of the above arrays
  # IMPORTANT: This step should be done AFTER we get their addresses when calling
  # get_obj_addresses_from_obj_names() on those names because otherwise their address
  # MAY become the address of NULL. This happens when the environment name is
  # represents an expression as in getNamespace("envnames") because the standardized name
  # results in "envnames" and this is NOT interpreted as an environment by the
  # aforementioned get_obj_addresses_from_obj_names() function called above
  # to retrieve its address.
  names(env_fullnames_user_recursive) = sapply( names(env_fullnames_user_recursive), standardize_env_name )
  names(env_addresses_user_recursive) = sapply( names(env_addresses_user_recursive), standardize_env_name )
  names(env_locationaddresses_user_recursive) = sapply( names(env_locationaddresses_user_recursive), standardize_env_name )

  return( list( fullnames=env_fullnames_user_recursive,
                addresses=env_addresses_user_recursive,
                locationaddresses=env_locationaddresses_user_recursive )
        )
}

#' Get the object addresses given their object names
#' 
#' @param obj_names array containing the name of the objects to retrieve.
#' They can be given as full path names (e.g. "env1$x") and they can also be
#' system or package environments given as e.g. ".GlobalEnv" or "R_GlobalEnv" or
#' "baseenv()" or "package:base" or "package:stats", etc. 
#' @param envir environment where the objects exist (considering the
#' object names are given with their full path to the object), or \code{NULL}
#' if they exist in the global environment.
#' 
#' @return An array containing the memory address of the objects given in \code{obj_names}
#' or NULL if there is a problem evaluating the corresponding object in the given environment
#' with \code{eval()}.  
#' 
#' @keywords internal
get_obj_addresses_from_obj_names = function(obj_names, envir=NULL) {
  
  #--------------------- Auxiliary functions --------------------------
  get_objects_from_object_names = function(obj_names, envir) {
    return( lapply(obj_names, evaluate_obj_name, envir) )
  }
  
  evaluate_obj_name = function(obj_name, envir) {
    # NOTE that we use eval() instead of get()
    # (in which case we would have used envs = get(obj_name, envir=envir)
    # because eval() can evaluate environments given as e.g. env_of_envs$env11
    # but get() cannot... get() does NOT accept the '$' sign, but accepts only
    # calls of the form get("env11", envir=env_of_envs).
    obj_eval = try( eval(parse(text=obj_name), envir=envir), silent=TRUE )
    if (inherits(obj_eval, "try-error")) {
      # Check if obj_name can be evaluated as an environment (e.g. when obj_name = ".Globalenv" or "package:base")
      obj_eval = try( as.environment(destandardize_env_name(obj_name)), silent=TRUE )
      if (inherits(obj_eval, "try-error"))
        return(NULL) 
    }
    return(obj_eval)
  }
  
  get_object_addresses = function(objects, envir) {
    #return( eval( unlist( lapply(objects, get_obj_address, envir) ), envir=envir ) )
    return( unlist( lapply(objects, address ) ) )
  }
  #--------------------- Auxiliary functions --------------------------
  
  # envir=NULL means that we do all expression and object evaluations in the global environment
  if (is.null(envir)) {
    envir = .GlobalEnv
  }
  
  # Get the objects referenced by the object names
  objects = get_objects_from_object_names(obj_names, envir)
  obj_addresses = get_object_addresses(objects, envir)
  
  return(obj_addresses)
}

#' Look for user environments defined inside an environment
#' 
#' @param envir environment where the search should be carried out.
#' 
#' @return an array containing the names of the user environments found
#' in the \code{envir} environment or NULL if \code{envir} is not an environment.
#' 
#' @keywords internal
get_user_environment_names_in_env = function(envir) {
  if (!is.environment(envir)) {
    error_NotValidEnvironment(deparse(substitute(envir)))
    return(NULL)
  }

  # NOTE: Out of the two statements below, we should prefer the one which does NOT use
  # the with() statement, because that one does NOT work when the envir environment
  # has the empty environment as its parent environment (i.e. when defined as new.env(parent=emptyenv())
  # because it complains that the Filter() function does not exist (because all functions
  # are inherited from parent environments such as the base environment where Filter() is defined)
  # Actually, when the parent environment of an environment is the empty environment "nothing works",
  # when running stuff inside a with() statement... e.g. not even the '<-' operator is found!
  #env_names = try( with(envir, Filter(function(x) "environment" %in% class(get(x)), ls())) )
  env_names = try( Filter(function(x) "environment" %in% class(get(x, envir=envir, inherits=FALSE)), ls(envir)), silent=TRUE )
  if (inherits(env_names, "try-error")) {
    # (2018/11/19) I already experienced a try-error when this function is called
    # by the process of running get_env_names() from within a test_that() block
    # and passing parameter include_functions=TRUE.
    # At some point, the envir parameter of this function happens to be an
    # execution environment with the following elements:
    # "classes"      "expr"         "finally"      "handlers"     "parentenv"    "tryCatchList" "tryCatchOne"
    # The error happens when applying the get() function of the Filter() to:
    # "expr" and "finally"
    # yielding the following errors, respectively:
    # "promise already under evaluation: recursive default argument reference or earlier problems?"
    # "argument "finally" is missing, with no default"
    return(NULL)
  }

  return(env_names)
}

#' Find user environments inside another user environment
#' 
#' Function that looks for user environments defined within a given user environment.
#' 
#' @param env_name name of the user environment where the search for other user environments
#' should be carried out.
#' @param path array containing the user environment names leading to the \code{env_name} environment.
#' @param path_to_envs_found array of environment names found so far including their path 
#' (as in \code{testenv$env1}). Note that the path does NOT include the *location* of the user
#' environments which is actually the system or package environment where the crawling starts
#' defined by parameter \code{rootenvir}.
#' @param rootenvir root environment specifying the location where the crawl of user environmnts
#' starts. This is needed for the \code{eval()} to work when resolving the name of the input environment
#' \code{env_name} into an enviroment. Ex: if we are crawling the environments defined in the envnames package,
#' \code{rootenvir} should be equal to the "package:envnames" environment, and here is where we are going to
#' find all the user environments that are crawled by this process as e.g.:
#' \code{testenv}
#' \code{testenv$env1}
#' \code{...}
#' i.e. all these environments will be \code{eval}uated in the \code{rootenvir} environment which
#' in this case is the "package:envnames" environment.
#' 
#' @return Either an updated \code{path_to_envs_found} with the user environments found so far
#' in the user environments tree or \code{TRUE}, which indicates that a leaf in the user
#' environments tree hanging from \code{rootenvir} has been reached.
#' 
#' @keywords internal
get_envs_in_env = function(env_name, path, path_to_envs_found, rootenvir=.GlobalEnv) {
  # Get the environment referenced by the names in the 'path' array (e.g. "testenv$env1"
  # when path = c("testenv", "env1") when evaluated in the 'rootenvir' environment.
  # IMPORTANT NOTES:
  # - we use get() instead of 'env = eval(parse(text=path), envir=rootenvir)' to get the environment
  # 'env' because eval() does NOT accept the inherits=FALSE argument, which is needed to reach the
  # environment ONLY if it is inside the 'rootenvir' environment (we are not interested in user
  # environments having the same name, but defined in environments other than the one given in 'rootenvir'
  # since we are currently analyzing the 'rootenvir' environment)
  # (get() gives an error if the variable referenced by the path text is not found in 'rootenvir'
  # therefore we enclose it in a try block)
  # - in order to 'get' a variable value, the name of the variable needs to be on its own (e.g. it cannot be
  # passed as testenv$env1) o.w. it is not found. This means that we need to make up the evaluation environment
  # concatenating the rootenvir environment with the environments chained in 'path'. This is the environment
  # stored in envir_full.
  # - the envir_full can be constructed by using "rootenvir" explicitly in the parse argument inside the eval()
  # function... I don't know why this works... (since we should not concatenate "rootenvir" per se but
  # the name of the environment **stored** in 'rootenvir'... but I think it has to do with the fact that
  # eval() tries to evaluate an expression in all possible environments...
  tryOutput = try(
    {
      envir_full = eval( parse(text=paste(c("rootenvir", path), collapse="$")) )	# This is weird... why it works! (see above comment)
      env = get(env_name, envir=envir_full, inherits=FALSE)
      env_names = get_user_environment_names_in_env(env)
    }
    , silent=TRUE)

  # Check if the get() call above gives an error, meaning essentially
  # that 'env_name' is not found in the 'path' inside 'rootenvir' (i.e. inside envir_full)
  if (inherits(tryOutput, "try-error")) {
    return(NULL)
  }

  # Check if any environments were found in env
  if (length(env_names) > 0) {
    # Environments were found inside 'env'
    # => Recurse on them.

    # Update the array whose elements tell us how to reach the user environments
    # found inside 'env' which we are going to crawl next (i.e. in the next recursive call
    # to crawl_envs())
    # (i.e. the updated array gives us the path to reach the environment referenced by 'env_name')
    # Ex: when env_name = "env1" and path = c("test_env")
    # the updated array becomes arr_path_to_env = c("testenv", "env1")
    # which means that the path to reach 'env1' is 'testenv$env1'.
    arr_path_to_env = c(path, env_name)
 
    # Recurse
    path_to_envs_found = crawl_envs(env_names, arr_path_to_env, path_to_envs_found, rootenvir=rootenvir)

    # Return the updated list of environments found via crawl_envs() including their path (e.g. "env1$env2$thisenv")
    return(path_to_envs_found)
  } else {
    # No environments were found => return TRUE to indicate that we reached a leaf in the environments tree
    return(TRUE)
  }
}

#' Crawl environments in search of user environments
#' 
#' Function that crawls a set of environments to search for user environments
#' defined within each of them.
#' 
#' @param env_names array with the environment names where the search for environments is wished
#' (careful: should not contain the environment *objects* but their *names*!).
#' @param path array with user environment names leading to the current set of
#' environments listed in \code{env_names}. Ex: \code{c("env_of_envs", "env11")} which means that the
#' the environments listed in \code{env_names} exist in environment \code{env_of_envs$env11}.
#' @param path_to_envs_found array of user environment names found so far including their path
#' where the path is allowed to include ONLY user environments.
#' (e.g. an element of the \code{path_to_envs_found} array could be \code{"env1$env2$env"}, but NOT
#' \code{"R_GlobalEnv$env1$env2"}, because "R_GobalEnv" is the name of a system environment, NOT a
#' user environment).
#' The first time this function is called it should contain the empty array.
#' @param rootenvir environment used as starting point for the user environment
#' search process. If \code{NULL}, the environment is taken from the names attribute of
#' the \code{env_names} parameter.
#' 
#' @details
#' This function crawls the user environments defined in the \code{rootenvir} environment and
#' any user environments within those user environments until no user environment
#' has been left without visit, making up a tree of nested user environments.
#' The path to each user environment found is stored using the \code{$} notation as in
#' \code{env1$env12$envx}.
#' 
#' Instead of setting the \code{rootenvir} parameter to the root environment where the search should start,
#' a similar result is obtained by setting the \code{path} variable to the environment chain leading to
#' the environments passed in \code{env_names}, as in e.g. \code{c("env_of_envs", "env11")}.
#' The difference is that such environment will appear as part of the paths to the environments passed,
#' and in addition, it is assumed that such environment is an actual environment.
#' 
#' @return An array containing the path to each user environment found inside the \code{rootenvir}
#' environment and all the nested user environments found within each of them.
#' This array is concatenated to whatever paths are already listed in the \code{path_to_envs_found}
#' parameter passed to the function.
#' 
#' @keywords internal
crawl_envs = function(env_names, path, path_to_envs_found, rootenvir=NULL) {
  # Before going through the different environments in env_names, add the current path to the environment list
  # (because we need to keep all the nodes of the environments tree, not only the leaves of the tree)
  if (!is.null(path)) {
    path_to_envs_found = c(path_to_envs_found, paste(path, collapse="$", sep=""))
  }

  # Crawl on the environments listed in env_names
  env_name_locations = names(env_names)
  for (i in seq_along(env_names)) {
    env_name = env_names[i]
		# IMPORTANT: Check if the environment referenced by env_name does NOT coincide with any of the environments
		# already seen and stored in path, o.w. we would go through an infinite recursion!
		if (!(env_name %in% path)) {
			### Look for other environments in the currently analyzed environment 'env_name'
		  
		  # First define the starting environment as the name attribute of the 'env_name' environment
		  # (whenever possible)
		  if (is.null(rootenvir)) {
		    rootenvir_to_search = try( as.environment(destandardize_env_name(env_name_locations[i])), silent=TRUE)
		    if (inherits(rootenvir_to_search, "try-error") || is.null(rootenvir_to_search)) {
		      rootenvir_to_search = .GlobalEnv
		    }
		  } else {
		    rootenvir_to_search = rootenvir
		  }
			result = get_envs_in_env(env_name, path, path_to_envs_found, rootenvir=rootenvir_to_search)

			# Check the result returned by get_envs_in_env() which tells us whether:
			# - result = NULL: the environment given in env_name is not an environment
			#   => nothing to do
			# - result is not logical: more environments were found in the environment referenced by env_name and they
			# were added to 'result' by recursion
			#   => 'result' should be the updated path_to_envs_found array
			# - result is logical (TRUE): no more environments were found inside the environment referenced by env_name
			#   => the env_name environment should be added to path_to_envs_found
			if (!is.null(result)) {
				if (!is.logical(result)) {
					# The result of get_envs_in_env() is the updated path_to_envs_found which means that we are in the middle of the tree
				  # => Store the resulting list into the output variable
					path_to_envs_found = result
				} else {
					# The result of get_envs_in_env() is TRUE, meaning that we reached a leaf of the environments tree
					# => Add the path to the currently analyzed environment as a string (e.g. "env1$env3$thisenv")
					path_to_envs_found = c(path_to_envs_found, paste(c(path, env_name), collapse="$", sep=""))
				}
			}
		}
	}

	return(path_to_envs_found)
}

#' Crawl an environment in search of user environments
#' 
#' @param envir environment to search in.
#' 
#' @return An array containing strings that represent the path to each environment found inside the
#' \code{envir} environment and any user environments nested within.
#' 
#' @keywords internal
crawl_envs_in_env = function(envir) {
  env_names = get_user_environment_names_in_env(envir)
  
  # Crawl the user environments found
  path_to_envs_found = crawl_envs(env_names, c(), c(), rootenvir=envir)
  
  return(path_to_envs_found)
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
#' Ex: \code{extract_root_and_last_member("obj$x$y")} returns \code{list(root="obj$x", name="y")}.
#' 
#' @keywords internal
extract_root_and_last_member = function(full_name) {
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

#' Put together a root name with a member name
#' 
#' This is the opposite operation of \code{extract_root_and_last_member()}:
#' the \code{root} and its supposed \code{member} are put together using the
#' \code{$} separator, as in \code{env_of_envs$env1$x}, where the root and
#' the member could be either \code{env_of_envs$env1} and \code{x} or
#' \code{env_of_envs} and \code{env1$x}.
#'
#' @param root String containing the root name to concatenate. It may be NULL or empty.
#' @param member String containing the member name to concatenate. It may be NULL or empty.
#' 
#' @return A string concatenating the root and the member names with the
#' \code{$} symbol. If any of them is empty or \code{NULL}, the other name is returned
#' or \code{""} if the other name is also empty or \code{NULL}.
#' 
#' @seealso \code{extract_root_and_last_member()}
collapse_root_and_member = function(root, member) {
  if ((length(root) == 0 || root == "") &&
      (length(member) == 0 || member == "")) {
    return("")
  } else if (length(root) == 0 || root == "") {
    return(member)
  } else if (length(member) == 0 || member == "") {
    return(root)
  } else {
    return(paste(root, member, sep="$"))
  }
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
#' \item contain one ore more user or named environments.
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
#' (system or package) or a user environment.
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
  if (!is.character(x)) {
    return(NULL)
  }

  # Initialize the output variables
  found = FALSE
  env_name = NULL

  # The check whether the string x is an environment name is done via an envmap lookup table computed
  # on the whole workspace.
  # Note that we need to do all this and cannot simply check whether is.environment(x) is TRUE
  # because x is a string!! (not an environment per se, only what the string refers to may be
  # an environment).

  # Get the address of the "presumed" environment in environment 'envir'
  env_address_found = FALSE
  x_address = try( address( eval(parse(text=x), envir=envir) ), silent=TRUE )
  if (!inherits(x_address, "try-error")) {
    env_address_found = TRUE
  } else {
    # Search for the environment in the whole workspace
    # (to make sure that the environment does not exist...
    # i.e. maybe it does not exist in the envir environment
    # but it exists somewhere in the whole workspace)
    x_address = try( address( eval(parse(text=x)) ), silent=TRUE )

    if (!inherits(x_address, "try-error")) {
      env_address_found = TRUE
    }
  }

  if (env_address_found) {
    # Search for the address just retrieved in the envmap lookup table constructed for the whole workspace
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
    x_root_and_name = extract_root_and_last_member(x)
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
#' @param envir environment where the existence of \code{object} is checked.
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

#' Check whether a string is a memory address
#' 
#' Check whether an object represents a valid memory address. If the object does not exist or is not of the
#' correct type FALSE is returned, no error is raised. 
#' 
#' @param x object to check.
#' @return boolean indicating whether the given object represents a valid memory address.
#' 
#' @details
#' Valid memory addresses depend on the architecture. For instance:
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

	# Minimum and maximum number of hexadecimal digits in valid memory address
	nhexdigits_min = 7    # This is the case for some Unix systems like Fedora, Solaris, and some Debian distributions
	                      # (inferred from CHECK run by CRAN when submitting the package)
	nhexdigits_max = 16   # Number of hexadecimal digits in a 64-bit system

	ischaracter = try( is.character(x), silent=TRUE )
	if (!inherits(ischaracter, "try-error") && ischaracter) {
		# Check if x contains a string that is a memory address
		# We allow for one or more spaces at the beginning or end of the string as in "   <0x00000000119dba68>  "
		# Note that the blank space at the beginning or end of the pattern includes tabs (checked).
		# Note also that if we want to use PERL regular expression we should use double escape to represent
		# special characters as in grep("^\\s*<", obj, perl=TRUE)
		isaddress = grep( paste("^ *<[0-9a-f]{", nhexdigits_min, ",", nhexdigits_max, "}> *$", sep=""), x, ignore.case=TRUE) ||
		            grep( paste("^ *[0-9a-f]{", nhexdigits_min, ",", nhexdigits_max, "} *$", sep=""), x, ignore.case=TRUE) ||
		            grep( paste("^ *<0x[0-9a-f]{", nhexdigits_min, ",", nhexdigits_max, "}> *$", sep=""), x, ignore.case=TRUE) ||
		            grep( paste("^ *0x[0-9a-f]{", nhexdigits_min, ",", nhexdigits_max, "} *$", sep=""), x, ignore.case=TRUE) ||
          		  grep( paste("^ *<environment: 0x[0-9a-f]{", nhexdigits_min, ",", nhexdigits_max, "}> *$", sep=""), x, ignore.case=TRUE)
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
#' FALSE is returned if the object does not exist.
#' 
#' @details
#' This function silently handles special cases for which is.null() and is.na() (called by this function)
#' may return a warning, such as functions objects or environments.
#' (e.g. the warning "Warning message: In is.na(g) : is.na() applied to non-(list or vector) of type 'closure')"
#' 
#' @keywords internal
is_logical = function(x) {
  output = try( !is.null(x) && !is.environment(x) && !is.symbol(x) && !is.na(x) && is.logical(x) && length(x) > 0, silent=TRUE )
  if (inherits(output, "try-error")) output = FALSE
  return(output)
}

#' Check whether an object is NULL or NA.
#' 
#' This function silently handles special cases for which is.null() and is.na() may return a warning,
#' such as functions objects or environments.
#' (e.g. the warning "Warning message: In is.na(g) : is.na() applied to non-(list or vector) of type 'closure')"
#' 
#' @param x object to check.
#' @return boolean indicating whether the object is NULL or NA.
#' FALSE is returned if the object does not exist.
#' 
#' @keywords internal
is_null_or_na = function(x) {
  output = try( is.null(x) || (!is.environment(x) && !is.symbol(x) && is.na(x)), silent=TRUE )
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

#' Standardize the name of a named environment
#' 
#' This function standardizes the name of a named environment so it is consistent
#' with the output of the base function \link{environmentName}.
#' For instance \code{globalenv()}, \code{.GlobalEnv} becomes \code{"R_GlobalEnv"}
#' \code{emptyenv()} becomes \code{"R_EmptyEnv"}
#' \code{baseenv()} or \code{as.environment("package:base")} becomes \code{"base"}.
#' All other named environments such as packages and namespaces are also converted
#' to the standard name used by R.
#' 
#' @param env_name environment name to standardize.
#'
#' @return Standardized name of the environment. If the environment is NOT a named
#' environment and it does not contain an expression that resolves to a named environment
#' it returns the input name as is. This includes strings that represent non-existing objects.
#' 
#' @keywords internal
standardize_env_name = function(env_name) {
  # Parse the environment name to see if it can be resolved to a named environment
  # NOTE that this converts expressions like globalenv(), baseenv(), etc. to the standard
  # ways that R uses to refer to the names of these environments.
  # For instance:
  # "globalenv()" resolves to "R_GlobalEnv"
  # "emptyenv()" resolves to "R_EmptyEnv"
  # "baseenv()" resolves to "base"
  # These same results could be obtained with the following gsub() calls:
  #	- gsub("\\.GlobalEnv|globalenv\\(\\)|globalenv", "R_GlobalEnv", env_name)
  # - gsub("\\.EmptyEnv|emptyenv\\(\\)|emptyenv", "R_EmptyEnv", env_name)
  #	- gsub("\\.BaseNamespaceEnv|package:base|baseenv\\(\\)|baseenv", "base", env_name)
  #	BUT calling environmentName() as done below is more general because it ALSO resolves the following things:
  # "as.environment(\"package:envnames\")" resolves to "package:envnames"
  # "getNamespace(\"envnames\")" resolves to "envnames"
  # and so forth with all other packages and namespaces.
  
  # Try different ways of parsing the environment name...
  # 1) This works when env_name = ".GlobalEnv", etc.
  # or an expression such as getNamespace("envnames") or as.environment("package:envnames")
  env_name_parsed = try( environmentName( eval(parse(text=env_name)) ), silent=TRUE )
  if (inherits(env_name_parsed, "try-error")) {
    # 2) This works when env_name is a package name as in "package:envnames"
    env_name_parsed = try( environmentName( eval(parse(text=paste("as.environment(\"", env_name, "\")", sep=""))) ), silent=TRUE )
  }
  if (inherits(env_name_parsed, "try-error")) {
    # 3) This works when env_name is given already as a STANDARDIZED name as in "R_GlobalEnv"
    env_name_parsed = try( environmentName( eval(parse(text=paste("as.environment(\"", destandardize_env_name(env_name), "\")", sep=""))) ), silent=TRUE )
  }
  if (inherits(env_name_parsed, "try-error") || env_name_parsed == "") {
    # env_name either is something that cannot be evaluated or it represents a user environment
    # => return the original input name given
    # (which may be an expression resolving to a user environment, in which
    # case the expression will be returned as a string)
    return(env_name)
  } else {
    # env_name is a named environment
    # => return the parsed name
    return(env_name_parsed)
  }
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

#' Set the "warn" options to -1 to avoid warning messages. The hidden variable .option_warn
#' defined in global_definitions.r has already been set to the original value of the "warn" option,
#' at the moment when the package is loaded, so that we can reset it later.
#' 
#' @keywords internal
set_option_warn_to_nowarning = function() {
  options(warn=-1)
}

#' Resets the "warn" option to the value stored in the hidden variable .option_warn
#' (set in global_definitions.r to the original value of the "warn" option when the package is loaded).
#' 
#' @keywords internal
reset_option_warn = function() {
  options(warn=.option_warn)
}
