
rm(list = ls())
require(data.table)
library(miceRanger)
require(doParallel)
require(mice)
set.seed(1991)
timeDT <- data.table()

cat("This machine has:",parallel::detectCores(),"cores.\n")

# Load dataset. Duplicate rows to get 10,000 samples.
data(iris)
toImpute <- iris[sample(1:nrow(iris),size=10000,replace=TRUE),]
toImpute <- amputeData(toImpute)

########################################################################################################
# Function that returns the trees, datasets, iterations, and time.
getTime <- function(trees,datasets,iterations) {
  
  mrTime <- system.time(
    miceRangerObj <- miceRanger(
      toImpute
      , m = datasets
      , maxiter = iterations
      , num.trees = trees
      , verbose=FALSE
    )
  )

  miceTime <- system.time(
    miceObj <- mice(
      toImpute
      , m = datasets
      , maxit = iterations
      , method = "rf"
      , ntree = trees
      , printFlag = FALSE
    )
  )
  
  return(
    list(
      trees = trees
      , datasets = datasets
      , iterations = iterations
      , mrTime = mrTime[[3]]
      , miceTime = miceTime[[3]]
      #, miceParTime = miceParTime[[3]]
    )
  )
  
}

# Something bizarre happens with function scoping which prevents you from
# passing arguments to parlmice from a parameter in a function. This is done manually
# in the for loop.

combinations <- data.table(
  trees = c(5,10,15,25,50,100,5,10,15,25,50,100)
  , datasets = c(5,5,5,5,5,5,5,5,5,5,5,5)
  , iterations = c(5,5,5,5,5,5,12,12,12,12,12,12)
  , runPar = c(1,1,1,1,1,1,1,1,1,1,0,0)
)

apply(combinations,2,function(x) )


for (i in 1:nrow(combinations)) {
  
  cat("Beginning run:",i,"\n")
  
  trees <- combinations[i,]$trees
  datasets <- combinations[i,]$datasets
  iterations <- combinations[i,]$iterations
  
  if (combinations[i,]$runPar == 1) {
    miceParTime <- system.time(
      miceParObj <- parlmice(
        toImpute
        , m = datasets
        , maxit = iterations
        , n.core = iterations
        , n.imp.core = 1
        , method = "rf"
        , ntree = trees
      )
    )
  } else miceParTime <- NA
  
  timeDT <- rbind(
      timeDT
    , as.data.table(
      c(
        getTime(
          trees = trees
          ,datasets = datasets
          ,iterations = iterations
        )
      , miceParTime = miceParTime[[3]]
      )
    )
  )
  
}  
  saveRDS(timeDT,file="support/data/timeDT10000.RDS")
gc()
closeAllConnections()
registerDoSEQ()
