# Returns the captured output of the inspection of an object existing in a given environment
obj_inspect = function(obj, envir=.GlobalEnv)
# If object is directly given as its memory address, there is no need to specify the environment.
{
  # Try evaluating the object in the environment, so that if it does not exist no error is generated
  try_eval = try( eval(obj, envir=envir), silent=TRUE )
  
  # Inspect the object if it was found in the environment
  if (inherits(try_eval, "try-error")) {
    obj.inspection = NULL
  } else {
    obj.inspection = capture.output(.Internal( inspect( eval(obj, envir=envir), silent=TRUE) ))
  }
  
  return(obj.inspection)
}
