#' @title addDatasets
#' @description Add datasets to a current miceDefs object. 
#' Adds the same number of iterations as other datasets.
#' @param miceObj A miceDefs object created by \code{miceRanger}.
#' @param datasets The number of iterations to run the MICE algorithm on each dataset.
#' @param parallel Should the process run in parallel? This process will take advantage of any cluster 
#' set up when \code{miceRanger} is called.
#' @param verbose should progress be printed?
#' @param ... other parameters passed to \code{ranger()} to control model building.
#' @return a miceDefs object with additional iterations.
#' @examples
#' data("sampleMiceDefs")
#' miceObj <- addIterations(
#'     sampleMiceDefs
#'   , iters = 2
#'   , verbose = FALSE
#'   , num.threads = 1
#'   , num.trees=5
#' )
#' @export
addDatasets <- function(
    miceObj
  , datasets = 3
  , parallel = FALSE
  , verbose = TRUE
  , ...
)
{
  
  dat <- copy(miceObj$data)
  ds <- crayon::make_style("#4B8E78")
  varn <- names(miceObj$callParams$vars)
  varp <- unique(unlist(miceObj$callParams$vars))
  vara <- unique(c(varn,varp))
  
  # Define parallelization setup
  ParMethod <- function(x) if(x) {`%dopar%`} else {`%do%`}
  `%op%` <- ParMethod(parallel)
  if(parallel) Workers <- getDoParWorkers() else Workers <- 1
  mco <- list(preschedule=FALSE)
  if (parallel & (getDoParWorkers() == 1)) stop("parallel is set to TRUE but no back end is registered.")
  if (!parallel & (getDoParWorkers() > 1)) if (verbose) message("parallel is set to FALSE but there is a back end registered. Process will not be run in parallel.\n")
  
  
  # Apply the same changes as in miceRanger()
  rawClasses <- sapply(dat[,vara,with=FALSE],class)
  toFactr <- names(rawClasses[rawClasses=="character"])
  toNumer <- names(rawClasses[rawClasses=="integer"])
  if (any(rawClasses == "character")) {
    dat[,(toFactr) := lapply(.SD,factor),.SDcols=toFactr]
  }
  if (any(rawClasses == "integer") & miceObj$callParams$valueSelector == "value") {
    dat[,(toNumer) := lapply(.SD,as.double),.SDcols=toNumer]
  }
  modelTypes <- ifelse(miceObj$newClasses[varn] == "factor","Classification","Regression")
  # Fill missing data with random samples from the nonmissing data.
  fillMissing <- function(vec) {
    vec[is.na(vec)] <- sample(vec[!is.na(vec)],size = sum(is.na(vec)),replace=TRUE)
    return(vec)
  }
  dat[,(names(dat)) := lapply(.SD,fillMissing),.SDcols=names(dat)]
  
  # Add specified datasets.
  startTime <- Sys.time()
  if (verbose) cat("\nProcess started at",as.character(startTime),"\n")
  datSetList <- runIterations(
      dat = dat
    , m = datasets
    , maxiter = miceObj$callParams$maxiter
    , vars = miceObj$callParams$vars
    , naWhere = miceObj$naWhere
    , valueSelector = miceObj$callParams$valueSelector
    , meanMatchCandidates = miceObj$callParams$meanMatchCandidates
    , modelTypes = modelTypes
    , verbose = verbose
    , ParMethod = ParMethod
    , parallel = parallel
    , mco = mco
    , miceObj = NULL
    , oldm = miceObj$callParams$m
    , oldIt = 0
    , startTime
    , ...
  )
  endTime <- Sys.time()
  
  # See if foreach produced errors.
  errorIndx <- sapply(datSetList,function(x) any(class(x) %in% c("simpleError","error")))
  if (any(errorIndx)) {
    stop(paste0("Evaluation failed with error:",as.character(datSetList[errorIndx][[1]])))
  }
  
  allImps <- if (datasets>1) lapply(datSetList,function(x) x$dsImps) else list(datSetList$dsImps)
  allImport <- if (datasets>1) lapply(datSetList,function(x) x$dsImport) else list(datSetList$dsImport)
  allError <- if (datasets>1) lapply(datSetList,function(x) x$dsError) else list(datSetList$dsError)
  rm(datSetList)
  
  # And finally, adjust names.
  datSetNames <- paste0("Dataset_",(miceObj$callParams$m+1):(miceObj$callParams$m+datasets))
  names(allImps) <- datSetNames
  names(allImport) <- datSetNames
  names(allError) <- datSetNames
  
  
  # Combine new iterations with old ones:
  allImps <- c(miceObj$allImps,allImps)
  allImport <- c(miceObj$allImport,allImport)
  allError <- c(miceObj$allError,allError)
  
  # Take necessary info from last iteration.
  finalImps <- lapply(allImps,function(x) x[[length(x)]])
  finalImport <- lapply(allImport,function(x) x[[length(x)]])
  finalError <- rbindlist(lapply(allError,function(x) x[nrow(x)]))
  setnames(finalError,"iteration","dataset")
  finalError[,`:=` ("dataset" = 1:(miceObj$callParams$m+datasets))]
  
  # Make necessary adjustments to miceObj
  miceObj$callParams$m <- miceObj$callParams$m+datasets
  miceObj$allImps <- allImps
  miceObj$allImport <- allImport
  miceObj$allError <- allError
  miceObj$finalImps <- finalImps
  miceObj$finalImport <- finalImport
  miceObj$finalError <- finalError
  miceObj$imputationTime <- miceObj$imputationTime + round(as.numeric(endTime - startTime))
  
  return(miceObj)
  
}

