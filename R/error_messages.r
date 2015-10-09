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
  cat("ERROR: '", envir_name, "' is not a valid environment.\n", sep="")
}
