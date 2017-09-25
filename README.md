# envnames
R package to retrieve user-defined environment names as well as function execution environments

## Description
The main goal of this package is to overcome the limitation of the built-in environmentName() function from the base package which does not return the name of an environment unless it is a package, a namespace or a named environment (e.g. the global environment, the base environment, the empty environment).

The envnames package solves this problem by creating a lookup table that maps environment names to their memory addresses.
Using this lookup table, it is possible to retrieve the _name_ of any environment where an object resides, be it a package, namespace, named environment, user-defined environment, or function execution environment.

See the following post as motivation for building the package:
https://stat.ethz.ch/pipermail/r-help/2010-July/245646.html
