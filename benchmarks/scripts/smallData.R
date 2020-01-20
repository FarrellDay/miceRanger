
rm(list = ls())
require(data.table)
library(miceRanger)
require(doParallel)
require(mice)
require(missForest)
set.seed(1991)

# Loads functions we will use to keep the code clean.
source("benchmarks/scripts/helperFuncs.R")

cat("This machine has:",parallel::detectCores(),"cores.\n")
cat("Original benchmarking machine had 12 cores and 16GB RAM.\n")

# Helper function for procuring factors that are correlated with
# different columns.
getFactorVar <- function(ind) {
  apply(
    dat
    , MARGIN = 1
    , function(x) {
      x[1:6] <- as.numeric(x[1:6])
      sample(
        LETTERS[ind]
        , size=1
        , prob = abs(as.numeric(x[ind]))/sum(abs(as.numeric(x[ind])))
      )
    }
  )
}

# Create artificial data
nrows <- 1000
dat <- data.table(a = runif(nrows))
dat$b <- dat$a + rnorm(nrows)
dat$c <- dat$b + rnorm(nrows)
dat$d <- dat$c + rnorm(nrows)
dat$e <- dat$d + rnorm(nrows)
dat$f <- dat$e + rnorm(nrows)
dat$g <- factor(getFactorVar(1:2))
dat$h <- factor(getFactorVar(3:4))
dat$i <- factor(getFactorVar(1:6))

# Ampute the data
ampDat <- amputeData(dat,perc=0.2)


# We run these benchmarks on different forest sizes.
treeVec <- c(4,8,16,32,64,128,256)

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
miceList <- list()
mfList <- list()


for (i in 1:length(treeVec)) {
  
  tre <- treeVec[[i]]
  
  cat("Running miceRanger (PMM) with",tre,"trees\n")
  mrTime <- system.time(
    mrList[[i]] <- miceRanger(
      ampDat
      , maxiter=5
      , num.trees=tre
      , verbose=FALSE
      , num.threads = 5
    )
  )[[3]]
  
  # Run miceRanger again without mean matching.
  cat("Running miceRanger (value) with",tre,"trees\n")
  mrvList[[i]] <- miceRanger(
    ampDat
    , maxiter=5
    , valueSelector = "value"
    , num.trees=tre
    , verbose=FALSE
    , num.threads = 5
  )
  
  cat("Running mice with",tre,"trees\n")
  miceTime <- system.time(
    miceList[[i]] <- parlmice(
      ampDat
      , method = "rf"
      , n.core=5
      , n.imp.core = 1
      , ntree=tre
    )
  )[[3]]
  
  # Set up parallel back end for missforest and miceRanger
  # Set up in lapply to simulate getting multiple datasets from missForest.
  cl <- makeCluster(5)
  registerDoParallel(cl)
  
  cat("Running missForest with",tre,"trees\n")
  sink(file=sinkFile)
  mfTime <- system.time(
    mfList[[i]] <- lapply(
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
    mpP <- miceRanger(
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
  
  timeDT <- rbind(
    timeDT
    , data.table(trees = tre, miceRanger = mrTime,miceRangerPar = mrPTime,mice = miceTime,missForest = mfTime)
  )
  
}


require(ggplot2)
plotDT <- melt(timeDT,id.vars = "trees")
p <- ggplot(plotDT,aes(x=trees,y=value,color=variable)) +
  geom_point() +
  geom_line() +
  xlab("Trees in Forest") +
  ylab("Time (Seconds)") +
  scale_x_continuous(trans='log2',labels=treeVec,breaks=treeVec) +
  ggtitle("1,000 Rows, 9 columns")
p$labels$colour <- "Package"


