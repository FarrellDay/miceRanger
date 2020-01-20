cran-comments
================

Detailed Package Info: https://github.com/farrellday/miceRanger


## Changes  
* Ability to specify imputation method by variable
* created unit tests with testthat






## Test Environments  
* Local Windows 10 x64, R 3.6.2  
* Windows Server 2008 R2 SP1 32/64 bit (Rhub)  
* Ubuntu Linux 16.04 LTS (Rhub)  
* Fedora Linux (Rhub)  
  

## R CMD check results

### Local
```
library("devtools")
devtools::check()
```
There were no ERRORs, WARNINGs or NOTEs


### Windows devel
```
devtools::check_win_devel()

...

Maintainer: 'Sam Wilson <samwilson303@gmail.com>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  Buhlmann (16:3)
  Buuren (14:3)
  Stekhoven (15:44)
  
...
```  
The mis-spelled words listed are names.


### Rhub
```
devtools::check_rhub()

...

* checking CRAN incoming feasibility ... NOTE

Maintainer: 'Sam Wilson <samwilson303@gmail.com>'
New submission
Possibly mis-spelled words in DESCRIPTION:
  Buhlmann (16:3)
  Buuren (14:3)
  Stekhoven (15:44)
```
The mis-spelled words are names.
