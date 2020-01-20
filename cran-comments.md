cran-comments
================

## miceRanger 1.2.0  
Detailed Package Info: https://github.com/farrellday/miceRanger


### BUG FIX  
* Fixed problem with plot arrangement. Only 1 numeric + 1 categorical variable would fail.
* Created unit tests with testthat to cover most possible scenarios.

### Changes
* Ability to specify imputation method by variable.
* Ability to specify predictors for each variable.
* Added BugReports and URL to DESCRIPTION.

### Test Environments  
* Local Windows 10 x64, R 3.6.2  
* Windows Server 2008 R2 SP1 32/64 bit (Rhub)  
* Ubuntu Linux 16.04 LTS (Rhub)  
* Fedora Linux (Rhub)  
* Ubuntu Linux 16.04 LTS (Travis CI)  
  

### R CMD check results

#### Local
```
library("devtools")
devtools::check()
```
There were no ERRORs, WARNINGs or NOTEs  
  


#### Windows devel
```
devtools::check_win_devel()

...
Maintainer: 'Sam Wilson <samwilson303@gmail.com>'

Days since last update: 1
...
```  
I apologize for the rapid update - there was a bug in a few of the the plotting functions. I have added unit tests to cover many different scenarios.  



#### Rhub
```
devtools::check_rhub()

...
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Sam Wilson <samwilson303@gmail.com>’

Days since last update: 1
```  
Same note as windows - I appologize for the rapid update.