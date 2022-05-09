# Comment from the authors

First of all: Please again excuse the high submission frequency! 

We have again approached the issue in our tests on M1mac. Thanks for informing us about macbuilder - we were not aware that this service exists. On macbuilder all checks, incl tests now pass and we are fairly confident that we should also pass on CRAN.


In this version (2.4.5) we address:
- issues with the tests on M1mac
- update the maintainer email address from "raluca.gui@business.uzh.ch" to "raluca.gui@gmail.com" because she has successfully completed her PhD studies


This submission also contains the changes for the previous version 2.4.4 which did not make it to cran. They fix the documentation to conform to HTML5. This has previously been reported by CRAN as an R CMD check error but not anymore. Still, we consider it good practice to fix this.



## Test environments
* macbuilder 
* local install on OS X 11.6.5, R 4.2.0, devtools 2.4.3
* github actions: win, osx, linux (release and devel)
* win-builder: release and devel
* rhub: check_for_cran

## R CMD check results 
There were 0 ERRORs, 0 WARNINGs, and 1-3 insignificant NOTEs (mis-spellings, invalid DOI).

## Downstream dependencies
We are not aware of any downstream dependencies.
