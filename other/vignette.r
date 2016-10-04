
 
# EXAMPLE TO SHOW IN THE VIGNETTE... 
env1 <- new.env()
env2 <- new.env()

# Build a chain of function calls ('->' means "calls"): env1$f -> env2$g -> h
#### FUNCTION h()
h <- function(x, silent=TRUE) {
  fun_calling_chain = get_fun_calling_chain()
  fun_calling = get_fun_calling()
  
  # Check if calling function is env1$f or env2$f
  if (fun_calling == "env1$f") { x = x + 1 }
  else if (fun_calling == "env2$f") { x = x + 2 }
  
  cat("sys.call:\n")
  print(sys.call(-1))
  cat("str\n")
  print(str(sys.call(-1)))
  #print(deparse(sys.call(1)))
  cat("function name:", gsub(pattern="^([A-Za-z0-9]+)(\\({1})(.*)(\\){1})$",replacement="\\1",x=deparse(sys.call(-1))), "\n")
    ## Ref: http://stackoverflow.com/questions/15595478/how-to-get-the-name-of-the-calling-function-inside-the-called-routine
  print(sapply(sys.calls(), "[[", 1)[[2]])
  print(class(sapply(sys.calls(), "[[", 1)[[2]]))
  # if (grep("env1$f", sys.call(sys.parent(1)))) { x = x + 1 }
  # else if (grep("env2$f", sys.call(sys.parent(1)))) { x = x + 2 }

  if (!silent) {
    # Show calling environment without using envnames package (i.e. using environmentName()) and using envnames::get_fun_calling()
    cat("\nNow inside function (using sys.call() the output is a call object):\n")
    print(sys.call(sys.parent(0)))
    cat("\nNow inside function (using envnames::get_fun_calling() the output is a string):", get_fun_calling(0), "\n")
    cat("Environment name of calling function (using environmentName() function):  \"", environmentName(parent.frame()), "\"\n", sep="")
    cat("Environment name of calling function as returned by get_fun_calling(): ", fun_calling, "\n", sep="")
    cat("Calling chain inside function:\n")
    print(fun_calling_chain)
  }
  
  return(x)
}

#### FUNCTION g()
with(env2,
  f <- function(x, silent=TRUE) {
    fun_calling_chain = get_fun_calling_chain()
    
    if (!silent) {
      cat("\nCalling chain inside function", get_fun_calling(0), ":\n")
      print(fun_calling_chain)
    }

    return(h(x, silent=silent))
  }
)

#### FUNCTION f()
with(env1,
  f <- function(x, silent=TRUE) {
    fun_calling_chain = get_fun_calling_chain()

    if (!silent) {
      cat("\nCalling chain inside function", get_fun_calling(0), ":\n")
      print(fun_calling_chain)
    }

    return(h(x, silent=silent))
  }
)

silent = FALSE
x = 0
cat("When h(x) is called by env1$f(x=", x, ") the output is: ", env1$f(x, silent=silent), "\n", sep="")
#cat("When h(x) is called by env2$f(x=", x, ") the output is: ", env2$f(x, silent=silent), "\n", sep="")

xx = c(rnorm(100), NA, NA)
plot.cdf(xx)
