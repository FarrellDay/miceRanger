#' @title miceRanger
#' @description Performs Multiple Imputation with Chained Equations (MICE).
#' Creates a miceDefs object, which contains information about the imputation process.
#' @param data The data to be imputed. Can contain variables that are not going to be imputed, which you
#' want to use as features.
#' @param m The datasets to produce
#' @param maxiter The number of iterations to run for each dataset.
#' @param vars A character vector of the variables to impute. 
#' The order of this vector will determine the order in which 
#' the variables are imputed. Default is all of the variables 
#' in \code{data}, in the same order they exist in the data.frame.
#' Any variable in this list that contains no missing values will
#' be removed, but will continue to be used as a feature. Right now,
#' there is no way to stop a feature being used to impute another variable.
#' @param valueSelector How to select the value. Can either use mean matching or
#' use the output from the regression itself. 
#' \itemize{
#'   \item {"meanMatch"} Mean matching for regression will select one of \code{meanMatchCandidates} 
#'   values. These candidates had a predicted value that is closest to the predicted value for that missing sample.
#'   Mean matching for classification will select a level based on a random sampling weighted by the class
#'   probability output by the random forest.
#'   \item {"value"} Returns the predicted output from the random forest. This will be the predicted
#'   class for classification, and the value for regression. Allows interpolation in regression. Not
#'   recommended, unless you have a specific reason.
#' }
#' @param meanMatchCandidates Used for regression. Take a random value from the most similair N values
#' in the dataset. Defaults to 1 percent of the rows in the dataset, with a minimum of 5.
#' @param parallel Should the process run in parallel? This process will take advantage of any cluster 
#' set up when \code{miceRanger} is called. Use carefully, a copy of \code{data} is sent to
#' each back end, and then another working copy is created to keep track of imputations. 
#' Therefore 1 copy exists in the \code{miceRanger} function scope, and 2 copies exist in each 
#' back end when running in parallel.
#' @param verbose should progress be printed?
#' @param ... other parameters passed to \code{ranger()} to control forest growth.
#' @importFrom data.table as.data.table rbindlist setcolorder setnames copy setDT
#' @importFrom ranger ranger
#' @importFrom stats predict
#' @importFrom crayon make_style
#' @import foreach
#' @return a miceDefs object, containing the following:
#' \item{callParams}{The parameters of the object.}
#' \item{data}{The original data provided by the user.}
#' \item{naWhere}{Logical index of missingness, same dimensions as data.}
#' \item{missingCounts}{The number of missing values for each variable}
#' \item{rawClasses}{The original classes provided in \code{data}}
#' \item{newClasses}{The new classes of the returned dataset. Classes can be changed if necessary.}
#' \item{allImps}{The imputations of all variables at each iteration, for each dataset.}
#' \item{allImport}{The variable importance metrics at each iteration, for each dataset.}
#' \item{allError}{The OOB model error for all variables at each iteration, for each dataset.}
#' \item{finalImps}{The final imputations for each dataset.}
#' \item{finalImport}{The final variable importance metrics for each dataset.}
#' \item{finalError}{The final model error for each variable in every dataset.}
#' \item{imputationTime}{The total time in seconds taken to create the imputations for the 
#'   specified datasets and iterations. Does not include any setup time.}
#' @examples
#' # Using Mice to create 5 imputed datasets
#' data(iris)
#' 
#' ampIris <- amputeData(iris)
#' 
#' miceObj <- miceRanger(
#'   ampIris
#'   , m = 2
#'   , maxiter = 3
#'   , verbose=FALSE
#' )
#' 
#' \dontrun{
#' # Run in parallel
#' library(doParallel)
#' cl <- makeCluster(2)
#' registerDoParallel(cl)
#' 
#' # Perform mice 
#' parTime <- system.time(
#'   miceObjPar <- miceRanger(
#'     ampIris
#'     , m=2
#'     , parallel = TRUE
#'     , verbose = FALSE
#'   )
#' )
#' stopCluster(cl)
#' registerDoSEQ()
#' }
#' @export
miceRanger <- function(
    data
  , m = 5
  , maxiter = 5
  , vars = names(data)
  , valueSelector = c("meanMatch","value")
  , meanMatchCandidates = pmax(round(nrow(data)*0.01),5)
  , parallel = FALSE
  , verbose = TRUE
  , ...
)
{
  
  # Create a copy of dataset so original is not modified.
  # This dataset is used to assign by reference throughout the process.
  # Fastest method, at the expense of duplicating the dataset.
  dat <- copy(data)
  setDT(dat)
  
  # Miscellaneous
  valueSelector <- valueSelector[[1]]

  # Define parallelization setup
  ParMethod <- function(x) if(x) {`%dopar%`} else {`%do%`}
  `%op%` <- ParMethod(parallel)
  mco <- list(preschedule=FALSE)
  if (parallel & (getDoParWorkers() == 1)) stop("parallel is set to TRUE but no back end is registered.")
  if (!parallel & (getDoParWorkers() > 1)) if (verbose) message("parallel is set to FALSE but there is a back end registered. Process will not be run in parallel.")
  
  
  # Missing Value Fidelity
  naWhere <- is.na(dat)
  missingCounts <- apply(naWhere,MARGIN = 2,sum)
  if (all(missingCounts == 0)) stop("There are no missing values in this dataset.")
  if (any(missingCounts == nrow(dat))) stop(paste0(paste0(names(missingCounts[missingCounts==nrow(dat)]),collapse=",")," contain all missing values. Cannot impute."))
  if (any(missingCounts/nrow(dat) >= 0.9) & verbose) message("At least one variable contains less than 10% nonmissing values.")
  takeOut <- missingCounts == 0 & names(missingCounts) %in% vars
  takeOut <- names(takeOut[takeOut]) # That's obnoxious
  if (length(takeOut) > 0) {
    if(verbose) {
      message(
          "One or more of the specified variables to impute contains no missing values. "
        , "These will remain as a predictor, however they will not be imputed. "
      )
    }
    vars <- setdiff(vars,takeOut)
  }
  
  # Ranger requires factors.
  # If valueSelector == 'value', integer targets will be interpolated. Need to
  # make integers doubles for this reason.
  rawClasses <- sapply(dat[,vars,with=FALSE],class)
  toFactr <- names(rawClasses[rawClasses=="character"])
  toNumer <- names(rawClasses[rawClasses=="integer"])
  if (any(rawClasses == "character")) {
    if(verbose) message("Converting characters to factors.")
    dat[,(toFactr) := lapply(.SD,factor),.SDcols=toFactr]
  }
  if (any(rawClasses == "integer") & valueSelector == "value") {
    if(verbose) message("valueSelector == 'value', so interpolation is possible. Converting integers to doubles so precision isn't lost.")
    dat[,(toNumer) := lapply(.SD,as.double),.SDcols=toNumer]
  }
  
  # Fill missing data with random samples from the nonmissing data.
  fillMissing <- function(vec) {
    vec[is.na(vec)] <- sample(vec[!is.na(vec)],size = sum(is.na(vec)),replace=TRUE)
    return(vec)
  }
  dat[,(names(dat)) := lapply(.SD,fillMissing),.SDcols=names(dat)]
  
  # Keep track of our new classes and the type of model.
  newClasses <- sapply(dat[,vars,with=FALSE],class)
  modelTypes <- ifelse(newClasses == "factor","Classification","Regression")
  
  startTime <- Sys.time()
  # Begin Iteration.
  datSetList <- runIterations(
      dat
    , m
    , maxiter
    , vars
    , naWhere
    , valueSelector
    , meanMatchCandidates
    , modelTypes
    , verbose
    , ParMethod
    , parallel
    , mco
    , oldm = 0
    , oldIt = 0
    , ...
  )
  endTime <- Sys.time()

  # See if foreach returned any errors.
  errorIndx <- sapply(datSetList,function(x) any(class(x) %in% c("simpleError","error")))
  if (any(errorIndx)) {
    stop(paste0("Evaluation failed with error:",as.character(datSetList[errorIndx][[1]])))
  }
  
  # Extract parts we are interested in from returned list.
  allImps <- if (m>1) lapply(datSetList,function(x) x$dsImps) else list(datSetList$dsImps)
  allImport <- if (m>1) lapply(datSetList,function(x) x$dsImport) else list(datSetList$dsImport)
  allError <- if (m>1) lapply(datSetList,function(x) x$dsError) else list(datSetList$dsError)
  rm(datSetList)
  
  # And finally, adjust names.
  names(allImps) <- paste0("Dataset_",1:m)
  names(allImport) <- paste0("Dataset_",1:m)
  names(allError) <- paste0("Dataset_",1:m)
  
  # Take necessary info from last iteration.
  finalImps <- lapply(allImps,function(x) x[[length(x)]])
  finalImport <- lapply(allImport,function(x) x[[length(x)]])
  finalError <- rbindlist(lapply(allError,function(x) x[nrow(x)]))
  setnames(finalError,"iteration","dataset")
  finalError[,`:=` ("dataset" = 1:m)]
  
  miceDefs <- list()
  miceDefs$callParams <- list()
  miceDefs$callParams$m <- m
  miceDefs$callParams$maxiter <- maxiter
  miceDefs$callParams$vars <- vars
  miceDefs$callParams$valueSelector <- valueSelector
  miceDefs$callParams$meanMatchCandidates <- meanMatchCandidates
  miceDefs$data <- data
  miceDefs$naWhere <- naWhere
  miceDefs$missingCounts <- missingCounts
  miceDefs$rawClasses <- rawClasses
  miceDefs$newClasses <- newClasses
  miceDefs$allImps <- allImps
  miceDefs$allImport <- allImport
  miceDefs$allError <- allError
  miceDefs$finalImps <- finalImps
  miceDefs$finalImport <- finalImport
  miceDefs$finalError <- finalError
  miceDefs$imputationTime <- round(as.numeric(endTime - startTime))
  
  class(miceDefs) <- "miceDefs"
  
  return(miceDefs)
  
}

