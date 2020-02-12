cran-comments
================

## miceRanger 1.3.0
Detailed Package Info: https://github.com/farrellday/miceRanger


### Changes
miceRanger was removed from CRAN because it was taking ~3000 seconds to build on Solaris. I have stopped the unit tests (a significant source of time).

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
  


#### Windows
```
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()
```  
There were no ERRORs, WARNINGs or NOTEs  



#### Rhub
```
devtools::check_rhub()
```  
There were no ERRORs, WARNINGs or NOTEs  
