
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
toImpute <- amputeData(iris)

trees <- c(5,10,25,50,100,500,1000,10000)

# This must be done in for loops, parlmice cannot accept
# function parameters as arguments.

miceRangerList <- list()
miceList <- list()
miceRangerTimes <- as.numeric()
miceTimes <- as.numeric()
for (i in 1:length(trees)) {
  
  miceTimes[[i]] <- system.time(
    miceList[[i]] <- mice(
        toImpute
      , m = 5
      , maxiter = 5
      , method = "rf"
      , ntree = trees[[i]]
    )
  )[[3]]

  miceRangerTimes[[i]] <- system.time(
    miceRangerList[[i]] <- miceRanger(
        toImpute
      , m = 5
      , maxiter = 5
      , num.trees = trees[[i]]
    )
  )[[3]]

}

plot(log(trees),miceTimes)
plot(log(trees),miceRangerTimes)

naWhere <- miceRangerList[[1]]$naWhere

miceCompList <- lapply(miceList,complete)
miceRangerCompList <- lapply(miceRangerList,completeData)

vars <- names(iris)
var <- vars[[1]]
getMiceErrors <- function(i,var) {
  actuals <- iris[[var]][naWhere[,var]]
  if (var == "Species") {
    return(apply(miceList[[i]]$imp[[var]] == as.character(actuals),2,sum)/15)
  } else {
    return(apply((miceList[[i]]$imp[[var]] - actuals)^2,2,sum))
  }
}

getMiceRangerErrors <- function(i,var) {
  actuals <- iris[[var]][naWhere[,var]]
  if (var == "Species") {
    return(apply(sapply(miceRangerList[[i]]$finalImps,function(x) x[[var]]) == as.character(actuals),2,sum)/15)
  } else {
    return(apply((miceRanger:::getVarImpMatrix(miceRangerList[[i]],var=var) - actuals)^2,2,sum))
  }
}

sapply(1:8,function(x) getMiceErrors(x,vars[[1]]))
sapply(1:8,function(x) getMiceRangerErrors(x,vars[[1]]))

getMiceErrors(4,vars[[2]])
getMiceRangerErrors(4,vars[[2]])

temp <- getVarImpMatrix(miceRangerList[[i]],var=var)

miceObj <- miceRanger(
  toImpute
  , m = 5
  , maxiter = 5
  , num.trees = trees[[i]]
)

