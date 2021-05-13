NEWS
================

Initial Submission to CRAN and public Github Repo: 2020-01-08  
Accepted by CRAN: 2020-01-19


## Package Updates  

#### 1.4.0
Default max.depth parameter for ranger is now 10. Originally, this was the default
unlimited. However, this causes ranger models to take up a lot of memory. Setting
max.depth to 10 saves space and still allows for accurate models.

#### 1.3.5
Fixed error in calculation of expected time in print.miceDefs.
Raw data cast to data.table in miceDefs and impDefs objects.

#### 1.3.4  
Use utils::tail() to get finalError last row doesn't fail for single var imputations.

#### 1.3.3  
Added comprehensive unit test  
Code clean up

#### 1.3.2
Fixed bug in plotImputationVariance() if characters were passed in original data. (#2)  
Fixed bug in completeData() around valueSelector. (#3)  
If a numeric variable is being imputed with valueSelector 'value', it doesn't need a meanMatchCandidates entry (#4)  

#### 1.3.0  
Implemented 'impute' function.

#### 1.2.1  
Changed minor formatting in print method.  
Seed issues were causing unit tests to fail on certain OS. Unit tests no longer rely on RNG.

#### 1.2.0  
Added the ability to specify mean matching or prediction value for each variable.  
Added the ability to specify predictors for each variable to impute.  
Improved plotting  
