get_obj_name_new = function(obj, n=0, eval=FALSE, silent=TRUE) {

  convert_to_character = function(obj) {
    # Ex: "x", 3, NA
    obj_name = try( as.character(obj), silent=TRUE )
    if (inherits(obj_name, "try-error")) {
      # Ex: user-defined environment
      obj_name = deparse(substitute(obj, parent.frame()))   # parent.frame() because we need to substitute obj in the calling environment (this assumes that the variable name there is also object!!)
    }
    return(obj_name)
  }

  parse_symbol_or_call = function(obj, n, eval=FALSE) {
    if (eval) {
      # First evaluate the object before retrieving its name
      # Ex: v[1], alist$x
      obj_eval = try( eval(obj, parent.frame(n+2)), silent=TRUE )  # when n = 0, we have parent.frame(2) => evaluate obj in the function calling get_obj_name()

      if (!inherits(obj_eval, "try-error")) {
        if (is.symbol(obj_eval) || is.call(obj_eval)) {
          obj_name = parse_symbol_or_call(obj_eval, n+1, eval=eval)
        } else {
          obj_name = convert_to_character(obj_eval[[1]])
        }
      }
    } else {
      obj_names = as.character(obj)
      if (length(obj_names) == 1) {
        # Ex: f()
        obj_name = obj_names
      } else if (length(obj_names) == 3) {
        # Ex: env1$env2$x
        obj_name = paste(obj_names[2], obj_names[1], obj_names[3], sep="")
      } else {
        obj_name = ""
      }
    }

    return(obj_name)    
  }

  # FUNCTION STARTS ---------------------------------------------------------
  obj_parent = substitute(obj)

  obj_name = ""
  if (is.environment(obj)) {
    obj_name = environmentName(obj)
  } else if (is.symbol(obj)) {
    # Ex: as.name("x")
    obj_name = as.character(obj)
  } else if (is.symbol(obj_parent) || is.call(obj_parent)) {
    # Ex: x, f(), env1$x
    obj_name = parse_symbol_or_call(obj_parent, n, eval=eval)
  }

  # Check if obj_name could not be set yet
  # Note that we check length(obj_name) > 0 before checking obj_name == "" because
  # the latter gives problems when obj_name is NA or NULL or is EMPTY
  # Note that length(NA) > 0 but length(NULL) == 0
  if (!is.null(obj_name) && !is.na(obj_name) && length(obj_name) > 0 && obj_name == "") {
    obj_name = convert_to_character(obj)
  } else if (length(obj_name) == 0) {   # This happens when the obj_name is character(0)
    obj_name = NULL
  }

  comment = function() {
    obj_parent = substitute(obj)
    if (n > 0)
      obj_parent = eval.parent(obj_parent, n)
    
    obj_name = deparse(obj_parent)
    if (obj_name == "<environment>") {
      # This means that obj_parent is the global environment or a package and
      # therefore we need to use environmentName() to get its name
      obj_name = environmentName(obj_parent)
    }
    #  print(class(obj_parent))

    if (is.call(obj_parent)) {
      obj_names = as.character(obj_parent)
      #    print(obj_names)
      if (length(obj_names) == 1) {
        # Ex: f()
        obj_name = obj_names
      } else if (length(obj_names) == 2) {
        # Ex: as.name("x"), as.environment("package:stats")
        # Get the symbol
        obj_symbol = eval(obj_parent)
        # Get the name as a string
        if (is.environment(obj_symbol)) {
          obj_name = environmentName(obj_symbol)
        } else {
          obj_name = as.character(obj_symbol)
        }
      } else if (length(obj_names) == 3) {
        # Ex: env1$env2$x
        obj_name = paste(obj_names[2], obj_names[1], obj_names[3], sep="")
      } else {
        obj_name = ""
      }
    } else {
      # Ex: x, 3, NA
      obj_name = as.character(obj_parent)
    }
  }
  
  return(obj_name)
}