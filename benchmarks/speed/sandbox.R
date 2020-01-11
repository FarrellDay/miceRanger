rm(list = ls())
require(data.table)
library(miceRanger)
require(doParallel)
require(mice)
set.seed(1991)

cat("This machine has:",parallel::detectCores(),"cores.\n")

# Load dataset. Duplicate rows to get 10,000 samples.
data(iris)
toImpute <- iris[sample(1:nrow(iris),size=10000,replace=TRUE),]
toImpute <- amputeData(toImpute)

##### No Parallel Setup - Default Forest Settings:
mrTime <- system.time(
  miceRangerObj <- miceRanger(
    toImpute
    , m = 5
    , maxiter = 5
  )
)

miceTime <- system.time(
  miceObj <- mice(
    toImpute
    , m = 5
    , maxit = 5
    , method = "rf"
  )
)

miceParTime <- system.time(
  miceParObj <- parlmice(
    toImpute
    , m = 5
    , n.core = 5
    , n.imp.core = 1
    , method = "rf"
  )
)

##### No Parallel Setup - Same Forest Settings:
mrTime <- system.time(
  miceRangerObj <- miceRanger(
    toImpute
    , m = 5
    , maxiter = 5
    , num.trees = 5
  )
)

miceTime <- system.time(
  miceObj <- mice(
    toImpute
    , m = 5
    , maxit = 5
    , method = "rf"
    , ntree = 5
  )
)

miceParTime <- system.time(
  miceParObj <- parlmice(
    toImpute
    , m = 5
    , n.core = 5
    , n.imp.core = 1
    , method = "rf"
  )
)