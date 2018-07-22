## Test environments
* ubuntu 18.04 LTS on Windows 10 R 3.4.4 (64-bit) (see below how I tested on this system)
* win-builder (http://win-builder.r-project.org/) (devel, release)

### There were no errors or warnings.
### There were 2 NOTES:

#### Note 1
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Daniel Mastropietro <mastropi@uwalumni.com>'

New submission

### How I tested the package in Linux
Used Ubuntu on Windows and followed these steps:  

1) Installed littler
sudo apt-get install littler

2) Installed other dependences (libcurl) that are needed to install devtools package
sudo apt-get update
sudo apt-get install libcurl4-gnutls-dev librtmp-dev

Ref: https://stackoverflow.com/questions/20236726/unable-to-install-devtools-package-for-r-studio-mounted-on-linux-redhat-server

3) Opened R by typing 'R' and installed needed packages
install.packages("devtools")
install.packages("testthat")

Note: packages are installed in user location:
~/R/x86_64-pc-linux-gnu-library/3.4

4) Installed my envnames package by going to the location of the tar.gz file
cd /mnt/e/Daniel/Projects/R/packages
R CMD INSTALL envnames

Note: package installed to the same user location indicated above.

Ref: http://kbroman.org/pkg_primer/pages/build.html
