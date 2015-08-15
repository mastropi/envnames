# envnames
R package to track environment names

## Description
The main goal of this package is to overcome the limitation of the built-in \code{\link{environmentName}} function of
the base package which cannot retrieve the name of an environment unless it is a package or the global environment.

The envnames package solves this problem by creating a lookup table that maps environment names to their memory addresses.
Using this lookup table, it is possible to retrieve the name of any environment where an object resides or to retrieve
the environment of the calling function.
