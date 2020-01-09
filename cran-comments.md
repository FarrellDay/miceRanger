cran-comments
================

Initial Submission.  
Detailed Package Info: https://github.com/farrellday/miceRanger

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
  Buhlmann (14:73)
  Buuren (13:40)
  OOB (16:13)
  Stekhoven (14:59)
  parallelizable (17:3)
  
...
```  
The mis-spelled words listed are not a problem.


### Rhub
```
devtools::check_rhub()

...

* checking CRAN incoming feasibility ... NOTE

Maintainer: 'Sam Wilson <samwilson303@gmail.com>'
New submission
Possibly mis-spelled words in DESCRIPTION:

  Buhlmann (14:73)
  Buuren (13:40)
  OOB (16:13)
  Stekhoven (14:59)
  parallelizable (17:3)

The Description field should not start with the package name,
  'This package' or similar.
```
The description does not contain the package name.  
The mis-spelled words listed are not a problem.
