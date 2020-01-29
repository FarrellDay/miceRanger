
## Benchmarking miceRanger

All scripts to create the following charts can be found in scripts/.

Using artificial data, the time and performance of miceRanger, mice
(`method = "rf"`) and missForest were recorded. parlmice was used to run
`mice` in parallel, and a parallel back end was set up for missForest.
All runs used 5 cores. miceRangerPar refers to miceRanger being run with
`parallel = TRUE`.

### Timing - Small and Medium Data

<img src="graphics/timeBenchmarks.png" width="650px" />

Scripts used:

  - mediumData.R  
  - smallData.R

### Timing - Rows x Trees

Data used was 10 columns, 7 numeric and 3 factors. Time recorded is in
seconds.  
<img src="graphics/rowXtreesTimeTable.png" width="500px" />

Script used:

  - rowXtrees.R

### Performance - Imputing New Data

![](graphics/impAccXMissingness.png)<!-- -->

This chart was created to show the behavior of imputation performance
when new data is imputed with different levels of missingness.  
The chart above consisted of the following procedure:

1)  A miceDefs object is created with 50 imputed datasets on a dataset
    with 25% missing values.  
2)  All 50 datasets are used to impute 9 new datasets, each with
    different levels of missingness  
3)  The performance (R-squared for numerics, accuracy for categoricals)
    is recorded for all 450 datasets

Script used:

  - imputeNew.R

### To Do

Imputation accuracy benchmarking with MAR, MCAR, MNAR, skewed,
multimodal data.
