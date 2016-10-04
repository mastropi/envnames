# Functions that show error messages
#
# Purpose: Concentrate all error messages in one place. 
#
# Error functions should start with prefix 'error_'
# Warning functions should start with prefix 'warning_'
# etc.
#
# The function name should then sum up what type of message is shown using Camel notation (e.g. NotValidEnvironment)

error_NotValidEnvironment = function(envir_name) {
	fun_name = get_fun_name(n=1)
  tryCatch(cat("ERROR: (", fun_name, ") '", envir_name, "' is not a valid environment.\n", sep=""), error=function(e) cat("The variable passed is not a valid environment\n"), silent=TRUE)
}

error_NotValidExpression = function(expr) {
	fun_name = get_fun_name(n=1)
	tryCatch(cat("ERROR: (", fun_name, ") '", expr, "' is not a valid expression to evaluate.\n", sep=""), error=function(e) cat("The expression is not valid.\n"), silent=TRUE)
}
