# Created:      26-Mar-2016
# Author:       Daniel Mastropietro
# Description:  Try compiling a C function and calling it from R
# CONCLUSION:   We can use the inline package to create a C function on the fly and then call it directly from R.
#               IMPORTANT: In order for this to work, we need to edit the $R_HOME/etc/x64/Makeconf file and
#               comment out the line LOCAL_SOFT which refers to a non-existent include directory that is listed
#               when cfunction() calls g++.
#               We can also use the Rcpp package and the cppFunction() therein to compile a C++ code.
# Ref:          http://adv-r.had.co.nz/C-interface.html (Hadley Wickham, very clear guide)
#               http://dirk.eddelbuettel.com/code/rcpp/Rcpp-FAQ.pdf
#

library(inline)

code <- "
char address_str[30];
snprintf(address_str, sizeof(address_str), \"<%p>\", (void *)x);
//printf(\"%s\", address_str);

// Option 1: create an SEXP object to hold the memory address
//SEXP address = PROTECT(allocVector(STRSXP, 1));
//SET_STRING_ELT(address, 0, mkChar(address_str));
//UNPROTECT(1);
//return address;

// Option 2: this is the way it is done in data.table::address() defined in wrappers.c
return(ScalarString(mkChar(address_str)));
//return R_NilValue;
"
## ScalarString: taken from data.table::address() whose source code is in wrappers.c
## See also Section 5.9.9 of R-exts.pdf
## HOWEVER, it still doesn' work: the R session crashes either when I use ScalarString or SET_STRING_ELT()...
## NOTE that R DOESN'T crash when I run the C program in Linux (Kubuntu 12.04 LTS in a VM)!
## Linux runs R-3.1.1
## => I think it has something to do with the RTools installation in Windows, since when I installed it
## I didn't install the Cygwin DLLs because I already had cygwin installed.
## However, it seems that we need to have the version of the cygwin DLLs that come with RTools!
## (Ref: https://cran.r-project.org/doc/manuals/R-admin.html#The-Windows-toolset)

code <- "Rprintf(\"<%p>\", (void *)x);"
## NOTE: If we don't use '(void *)' R crashes
# SET_STRING_ELT taken from https://cran.r-project.org/doc/manuals/R-exts.pdf, section 5.12
# Ref for snprintf which is used to store a formatted string in a variable
# http://stackoverflow.com/questions/10285596/how-to-store-the-resulting-formatted-c-string
address <- cfunction(c(x="SEXP"), code, verbose=TRUE)
x = 3
address(x)
## OK! but it returns <0x00000000085c3bf0>.Primitive("")
## that is there is this ".Primitive()" thing added at the end... what's that!

# Ref: http://stackoverflow.com/questions/14939474/rcpp-inline-package-error-in-compilecode
# Using the inline library
myroot2 <- rcpp(signature(xs="numeric"), body='double x=as<double>(xs); return wrap(::sqrt(x));')

# Using the Rcpp library
library(Rcpp)
myroot <- cppFunction('double myroot(double x) { return ::sqrt(x); }', verbose=TRUE)
myroot(4)
testfun <- cxxfunction(signature(), paste(readLines("E:/Daniel/Projects/R/packages/envnames/src/testfun.c"), collapse="\n"), plugin="Rcpp", verbose=TRUE)
## Use of readLines() as suggested in Section 2.2.1 of the Rcpp FAQ referenced above
## Not tried.



code <- "
SEXP c = PROTECT(allocVector(REALSXP,1));
REAL(c)[0] = asReal(a) + asReal(b);
UNPROTECT(1);
return c;
"
mysum <- cfunction(c(a="double", b="double"), code, verbose=TRUE)
a = 3;
b = 2;
mysum(a, b)
## Works

# 2016/07/21: Test the address() function defined in src/address.c
code <- "
char address[20];
snprintf(address, sizeof(address), \"<%p>\", (void *)&x);
return(ScalarString(mkChar(address)));
"
address <- cfunction(c(x="SEXP"), code, verbose=TRUE)
x = 3
address(x)
## Note: that defining the address() function as "Rprintf("<%p>", (void *)x);"
## (and compiling with R CMD SHLIB loading it with dyn.load() and trying it out with .Call("address", x))
## although it returns the right memory address string, it makes R hang...
