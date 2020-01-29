#' @title addDatasets
#' @description Add datasets to a current miceDefs object. 
#' Adds the same number of iterations as other datasets.
#' @param miceObj A miceDefs object created by \code{miceRanger}.
#' @param datasets The number of datasets to add.
#' @param parallel Should the process run in parallel? This process will take advantage of any cluster 
#' set up when \code{miceRanger} is called.
#' @param verbose should progress be printed?
#' @param ... other parameters passed to \code{ranger()} to control model building.
#' @return an updated miceDefs object with additional datasets.
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
  
  # Setup
  dat <- copy(miceObj$data)
  m <- miceObj$callParams$m
  ds <- crayon::make_style("#4B8E78")
  varn <- names(miceObj$callParams$vars)
  varp <- unique(unlist(miceObj$callParams$vars))
  vara <- unique(c(varn,varp))
  valueSelector <- miceObj$callParams$valueSelector
  returnModels <- miceObj$callParams$returnModels
  modelTypes <- ifelse(miceObj$newClasses[varn] == "factor","Classification","Regression")
  
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
  if (any(rawClasses == "character")) {
    dat[,(toFactr) := lapply(.SD,factor),.SDcols=toFactr]
  }
  intToDouble <- rawClasses[varn] == "integer" & valueSelector[varn] == "value"
  if (any(intToDouble)) {
    if(verbose) message("valueSelector == 'value', so interpolation is possible. Converting integers to doubles so precision isn't lost.")
    intToDouble <- names(intToDouble[intToDouble])
    dat[,(intToDouble) := lapply(.SD,as.double),.SDcols=intToDouble]
  }
  
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
    , valueSelector = valueSelector
    , meanMatchCandidates = miceObj$callParams$meanMatchCandidates
    , modelTypes = modelTypes
    , verbose = verbose
    , ParMethod = ParMethod
    , parallel = parallel
    , mco = mco
    , miceObj = NULL
    , oldm = m
    , oldIt = 0
    , startTime
    , returnModels = returnModels
    , ...
  )
  endTime <- Sys.time()
  
  # Foreach won't combine into a list if m = 1.
  # This keeps the extraction below much more simple.
  if (datasets == 1) datSetList <- list(Dataset_1 = datSetList)
  
  # See if foreach produced errors.
  errorIndx <- sapply(datSetList,function(x) any(class(x) %in% c("simpleError","error")))
  if (any(errorIndx)) {
    stop(
      paste0(
        "Evaluation failed with error <",as.character(datSetList[errorIndx][[1]])
        ,">. This is probably our fault - please open an issue at https://github.com/FarrellDay/miceRanger/issues"
        ," with a reproduceable example."
      )
    )
  }
  
  # Format and merge dataset info we just obtained.
  datSetNames <- paste0("Dataset_",1:datasets+m)
  allImps <- lapply(datSetList,function(x) x$dsImps)
  names(allImps) <- datSetNames
  allImps <- c(miceObj$allImps,allImps)
  
  allImport <- lapply(datSetList,function(x) x$dsImport)
  names(allImport) <- datSetNames
  allImport <- c(miceObj$allImport,allImport)
  
  allError <- lapply(datSetList,function(x) x$dsError)
  names(allError) <- datSetNames
  allError <- c(miceObj$allError,allError)
  
  if (returnModels) {
    finalModels <- lapply(datSetList,function(x) x$dsMod)
    names(finalModels) <- datSetNames
    finalModels <- c(miceObj$finalModels,finalModels)
  }
  rm(datSetList)
  
  # Take necessary info from last iteration.
  finalImps <- lapply(allImps,function(x) x[[length(x)]])
  finalImport <- lapply(allImport,function(x) x[[length(x)]])
  finalError <- rbindlist(lapply(allError,function(x) x[nrow(x)]))
  setnames(finalError,"iteration","dataset")
  finalError[,`:=` ("dataset" = 1:(m+datasets))]
  
  # Make necessary adjustments to miceObj
  miceObj$callParams$m <- m+datasets
  miceObj$allImps <- allImps
  miceObj$allImport <- allImport
  miceObj$allError <- allError
  miceObj$finalImps <- finalImps
  miceObj$finalImport <- finalImport
  miceObj$finalError <- finalError
  if (returnModels) miceObj$finalModels <- finalModels
  miceObj$imputationTime <- miceObj$imputationTime + round(as.numeric(endTime - startTime))
  
  return(miceObj)
  
}
