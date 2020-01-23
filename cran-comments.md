cran-comments
================

## miceRanger 1.2.1  
Detailed Package Info: https://github.com/farrellday/miceRanger


### Changes
This submission is to fix the ERROR's occuring here:  
https://cran.r-project.org/web/checks/check_results_miceRanger.html

Unit tests are failing because of a difference in seed behavior on those systems.
Removed the reliance on RNG to pass unit test.

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