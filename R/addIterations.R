#' @title addIterations
#' @description Add iterations to a current miceDefs object. Adds iterations for all datasets.
#' @param miceObj A miceDefs object created by \code{miceRanger}.
#' @param iters The number of iterations to add to each dataset.
#' @param parallel Should the process run in parallel? This process will take advantage of any cluster 
#' set up when \code{miceRanger} is called.
#' @param verbose should progress be printed?
#' @param ... other parameters passed to \code{ranger()} to control model building.
#' @return an updated miceDefs object with additional iterations.
#' @examples
#' data("sampleMiceDefs")
#' miceObj <- addIterations(
#'     sampleMiceDefs
#'   , iters=2
#'   , verbose=FALSE
#'   , num.threads = 1
#'   , num.trees=5
#' )
#' @export
addIterations <- function(
    miceObj
  , iters = 5
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
  valueSelector <- miceObj$callParams$valueSelector
  modelTypes <- ifelse(miceObj$newClasses == "factor","Classification","Regression")

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
  

  # Run iterations for specified iterations. Continue from miceObj.
  startTime <- Sys.time()
  if (verbose) cat("\nProcess started at",as.character(startTime),"\n")
  datSetList <- runIterations(
      dat = NULL
    , m = miceObj$callParams$m
    , maxiter = iters
    , vars = miceObj$callParams$vars
    , naWhere = miceObj$naWhere
    , valueSelector = valueSelector
    , meanMatchCandidates = miceObj$callParams$meanMatchCandidates
    , modelTypes = modelTypes
    , verbose = verbose
    , ParMethod = ParMethod
    , parallel = parallel
    , mco = mco
    , miceObj = miceObj
    , oldm = 0
    , oldIt = miceObj$callParams$maxiter
    , startTime
    , ...
  )
  endTime <- Sys.time()
  
  allImps <- if (miceObj$callParams$m>1) lapply(datSetList,function(x) x$dsImps) else list(datSetList$dsImps)
  allImport <- if (miceObj$callParams$m>1) lapply(datSetList,function(x) x$dsImport) else list(datSetList$dsImport)
  allError <- if (miceObj$callParams$m>1) lapply(datSetList,function(x) x$dsError) else list(datSetList$dsError)
  rm(datSetList)
  
  # And finally, adjust names.
  names(allImps) <- paste0("Dataset_",1:miceObj$callParams$m)
  names(allImport) <- paste0("Dataset_",1:miceObj$callParams$m)
  names(allError) <- paste0("Dataset_",1:miceObj$callParams$m)
  
  # Combine new iterations with old ones:
  for (i in 1:miceObj$callParams$m) {
    allImps[[i]] <- c(miceObj$allImps[[i]],allImps[[i]])
    allImport[[i]] <- c(miceObj$allImport[[i]],allImport[[i]])
    allError[[i]] <- rbind(miceObj$allError[[i]],allError[[i]])
  }
  
  # Take necessary info from last iteration.
  finalImps <- lapply(allImps,function(x) x[[length(x)]])
  finalImport <- lapply(allImport,function(x) x[[length(x)]])
  finalError <- rbindlist(lapply(allError,function(x) x[nrow(x)]))
  setnames(finalError,"iteration","dataset")
  finalError[,`:=` ("dataset" = 1:miceObj$callParams$m)]
  
  # Make necessary adjustments to miceObj
  miceObj$callParams$maxiter <- miceObj$callParams$maxiter + iters
  miceObj$allImps <- allImps
  miceObj$allImport <- allImport
  miceObj$allError <- allError
  miceObj$finalImps <- finalImps
  miceObj$finalImport <- finalImport
  miceObj$finalError <- finalError
  miceObj$imputationTime <- miceObj$imputationTime + round(as.numeric(endTime - startTime))
  
  return(miceObj)
  
}
