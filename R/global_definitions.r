### Environment with "global" definitions
# This is a hidden environment (because it starts with dot)
.pkgenv <- new.env(parent=emptyenv())
# Strings used by variable R.version or version as architecture value for 32-bit and 64-bit architectures 
.pkgenv$ARCH_32BIT = "i386"
.pkgenv$ARCH_64BIT = "x86_64"

# Define an environment to test the search for environments that are defined in a package
# Note that we cannot use the hidden environment just defined for this function because
# that environment is hidden and therefore it is not seen!
testenv <- new.env()
with(testenv, env1 <- new.env(parent=emptyenv()))
