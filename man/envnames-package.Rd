\docType{package}
\name{envnames-package}
\alias{envnames-package}
\alias{envnames}
\title{Track user-defined and execution environment names}
\description{The main goal of this package is to overcome the limitation of the built-in \code{\link{environmentName}} function of the base package which cannot retrieve the name of an environment unless it is a package or the global environment.

The envnames package solves this problem by creating a lookup table that maps environment names to their memory addresses. Using this lookup table, it is possible to retrieve the name of any environment where an object resides or to retrieve the environment of the calling function.

All function in this package follow the underscore_separated all lower case naming convention.
}
\details{
The package currently contains 10 functions. Following is a guideline on how to use the package to track user-defined environment names and function environments:

1) Use environment_name() to retrieve the name of an environment created with \code{\link{new.env}}.

2) Use \code{\link{obj_find}} to find the environments where a given object is defined.
}
\author{Daniel Mastropietro

Maintainer: Daniel Mastropietro <mastropi@uwalumni.com>
}
\references{N/A}
\keyword{ package }
\keyword{ environment }
\seealso{ \code{\link{environmentName} } in the base package for the built-in function that retrieves environment names of packages.}
\examples{
}
