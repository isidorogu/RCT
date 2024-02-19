## Test environments
* local MacOS install, R 4.3.2
* mcaos 12.7.3 (R-release) on Github actions (GHA)
* Microsoft Windows Server 2022 (R-release) on Github actions 
* Microsoft Windows x86_64-w64-mingw32 R-devel from (Rhub)
* ubuntu 22.04.03 (R-devel) on GHA 
* ubuntu 22.04.03 (R-release) on GHA
* ubuntu 22.04.03 (R-oldrel-1) on GHA

## R CMD check results
There were no ERRORs, WARNINGs Nor  NOTEs.


## Downstream dependencies
There are currently no downstream dependencies for this package

## Resubmission
This is a resubmission. In this version I have:

*Changed the impact_eval function for it not to depend on lfe package 