
rm(list = ls())
require(data.table)
library(miceRanger)
require(doParallel)
require(mice)
require(missForest)
set.seed(1991)

cat("This machine has:",parallel::detectCores(),"cores.\n")
cat("Original benchmarking machine had 12 cores and 16GB RAM.\n")

# Helper function for procuring factors that are correlated with
# different columns.
getFactorVar <- function(ind,dat) {
  apply(
    dat
    , MARGIN = 1
    , function(x) {
      x[1:7] <- as.numeric(x[1:7])
      sample(
        LETTERS[ind]
        , size=1
        , prob = abs(as.numeric(x[ind]))/sum(abs(as.numeric(x[ind])))
      )
    }
  )
}

getArtData <- function(nrws) {
  dat <- data.table(a = runif(nrws))
  dat$b <- dat$a + rnorm(nrws)
  dat$c <- dat$b + rnorm(nrws)
  dat$d <- dat$c + rnorm(nrws)
  dat$e <- dat$d + rnorm(nrws)
  dat$f <- dat$e + rnorm(nrws)
  dat$g <- dat$f + rnorm(nrws)
  dat$h <- factor(getFactorVar(1:2,dat))
  dat$i <- factor(getFactorVar(3:4,dat))
  dat$j <- factor(getFactorVar(5:7,dat))
  
  # Ampute the data
  ampDat <- amputeData(dat,perc=0.2)
  return(ampDat)
}


# We run these benchmarks on different forest sizes.
treeVec <- c(4,8,16,32,64,128,256)

# We run the process for each of these number of rows.
nrowVec <- c(100,200,400,800,1600,3200,6400,12800)

runs <- expand.grid(trees = treeVec,nrow = nrowVec)

# Begin growing forests for each package. This is done
# in the global environment because parlmice cannot be
# passed arguments from a function, or it runs into
# variable scoping issues.

# There is no way to keep missForest from printing output.
# Sink is used.
sinkFile <- file()

# pre-define 
timeDT <- data.table()
mrList <- list()
mrvList <- list()
mfList <- list()


timeVec <- apply(
    runs
  , 1
  , function(trial) {
    
    tre <- trial['trees']
    ampDat <- getArtData(trial['nrow'])

    cat("Running miceRanger (PMM) with",tre,"trees\n")
    mrTime <- system.time(
      obj <- miceRanger(
        ampDat
        , maxiter=5
        , num.trees=tre
        , verbose=FALSE
        , num.threads = 5
      )
    )[[3]]
    
    # Set up parallel back end for missforest and miceRanger
    # Set up in lapply to simulate getting multiple datasets from missForest.
    cl <- makeCluster(5)
    registerDoParallel(cl)
    
    cat("Running missForest with",tre,"trees\n")
    sink(file=sinkFile)
    mfTime <- system.time(
      obj <- lapply(
        1:length(treeVec)
        , function(x) {
          missForest(
            ampDat
            , maxiter=5
            , ntree=tre
            , verbose=FALSE
            , parallelize = "forests"
          )
        }
      )
    )[[3]]
    while (sink.number() > 0) sink()
    
    cat("Running miceRanger (parallel) with",tre,"trees\n\n")
    mrPTime <- system.time(
      obj <- miceRanger(
          ampDat
        , maxiter=5
        , num.trees=tre
        , verbose=FALSE
        , num.threads = 1
        , parallel = TRUE
      )
    )[[3]]
    stopCluster(cl)
    registerDoSEQ()
    
    return(
      data.table(
          trees = tre
        , nrow = trial['nrow']
        , miceRanger = mrTime
        , miceRangerPar = mrPTime
        , missForest = mfTime
      )
    )
  }
)

timeDT <- rbindlist(timeVec)


