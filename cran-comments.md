# Comment from the authors
This is version 2.3.1 which includes no changes to its previous version (2.3.0).

Reason for this submission is that the REndo package was archived on 2019-12-21 because we could not fix an issue on time. We were notified by email that our package fails checks on r-devel (debian) due to the fact that matrix objects newly also inherit from class array.

We could not, and still cannot, reproduce this error, neither locally, nor on any of rhub's *-r-devel platforms with standard and cran settings (rhub::check_for_cran()).
Based on this we conlcude that the tests in our package have previously failed likely because of packages which we depend on and not because of the code in this package.
We would therefore like to re-submit this package unchanged (with new version number) in order to make it available on CRAN again.

## Test environments
* local install on OS X 10.15.3, R 3.6.2, devtools 2.2.1
* win-builder (rdevel, release)
* rhub (all)

## R CMD check results 
There were 0 ERRORs, 0 WARNINGs, and 1-3 insignificant NOTE (mis-spellings, invalid DOI, runtime of examples >5s).

## Downstream dependencies
We are not aware of any downstream dependencies.
