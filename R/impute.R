#' Impute New Data With Existing Models
#' 
#' Impute data using the information from an existing \code{miceDefs} object. 
#' 
#' This capability is experimental, but works well in 
#' \href{https://github.com/FarrellDay/miceRanger/tree/master/benchmarks}{benchmarking}. 
#' The original data and random forests (if returnModels = TRUE) are returned when \code{miceRanger} 
#' is called. These models can be recycled to impute a new dataset in the same fashion as \code{miceRanger},
#' by imputing each variable over a series of iterations. Each dataset created in \code{miceObj} 
#' can be thought of as a different imputation mechanism, with different initialized values
#' and a different associated random forests. Therefore, it is necessary to choose the datasets
#' which will be used to impute the data. When mean matching a numeric variable, the candidate 
#' values are drawn from the original data passed to \code{miceRanger}, not the \code{data} passed 
#' to this function.
#' 
#' @param data The data to be imputed. Must have all columns used in the imputation of miceDefs.
#' @param miceObj A miceDefs object created by \code{miceRanger()}.
#' @param datasets A numeric vector specifying the datasets with which to impute \code{data}.
#' See details for more information.
#' @param iterations The number of iterations to run. 
#' By default, the same as the number of iterations currently in \code{miceObj}.
#' @param verbose should progress be printed?
#' @return An object of class impDefs, which contains information about the imputation process.
#' \item{callParams}{The parameters of the object.}
#' \item{data}{The original data provided by the user.}
#' \item{naWhere}{Logical index of missing data, having the same dimensions as \code{data}.}
#' \item{missingCounts}{The number of missing values for each variable.}
#' \item{imputedData}{A list of imputed datasets.}
#' 
#' @examples
#' ampDat <- amputeData(iris)
#' miceObj <- miceRanger(ampDat,1,1,returnModels=TRUE,verbose=FALSE)
#' 
#' newDat <- amputeData(iris)
#' newImps <- impute(newDat,miceObj)
#' @export
impute <- function(
    data
  , miceObj
  , datasets = 1:miceObj$callParams$m
  , iterations = miceObj$callParams$maxiter
  , verbose = TRUE
) {
  
  if (class(miceObj) != "miceDefs") stop("miceObj must be a miceDefs object.")
  if (!miceObj$callParams$returnModels) stop("returnModels must be set to TRUE to impute new data.")
  if (any(datasets > miceObj$callParams$m)) stop("dataset specified was never created in miceObj")
  
  data = copy(data)
  setDT(data)
  dat <- sapply(
      as.character(datasets)
    , function(x) copy(data)
    , USE.NAMES = TRUE
    , simplify = FALSE
  )
  
  # Extract data from miceObj and set up variables.
  ds <- crayon::make_style("#4B8E78")
  m <- miceObj$callParams$m
  vars <- miceObj$callParams$vars
  varn <- names(vars)
  varp <- unique(unlist(vars))
  vara <- unique(c(varn,varp))
  valueSelector <- miceObj$callParams$valueSelector
  returnModels <- miceObj$callParams$returnModels
  meanMatchCandidates <- miceObj$callParams$meanMatchCandidates
  modelTypes <- ifelse(miceObj$newClasses[varn] == "factor","Classification","Regression")
  
  if (!all(vara %in% names(data))) stop("Columns used in the original imputation process are missing from this dataset.")

  
  # Missing Value Fidelity.
  naWhere <- is.na(data)
  if (any(apply(naWhere,2,sum) == nrow(data)) & verbose) warning("At least 1 column in data contains all missing values. Imputations may be questionable.")
  missingCounts <- apply(data[,vara,with=FALSE],MARGIN = 2,function(x) sum(is.na(x)))
  if (all(missingCounts == 0)) stop("There are no missing values in this dataset.")
  
  # Only attempt to impute columns that actually have missing values in this dataset:
  varn <- intersect(names(missingCounts[missingCounts > 0]),varn)
  
  # Fill missing data with random samples from the original nonmissing data.
  for (d in dat) for (v in vara) {
    set(d,i=which(naWhere[,v]),j=v,value=fillMissing(missingCounts[[v]],miceObj$data[[v]]))
  }
  
  # Convert any integer variables to double if we aren't mean matching.
  # If we don't, data.table will complain when we try to complete the data.
  # Numeric stored with 0 precision is fine.
  intToDouble <- miceObj$rawClasses[varn] == "integer" & valueSelector[varn] == "value"
  if (any(intToDouble)) {
    if(verbose) message("valueSelector == 'value', so interpolation is possible. Converting integers to doubles so precision isn't lost.")
    intToDouble <- names(intToDouble[intToDouble])
    for (d in dat) d[,(intToDouble) := lapply(.SD,as.double),.SDcols=intToDouble]
  }
  
  # For-loops used because of assignment by reference.
  for (d in as.character(datasets)) {

    dIndx <- paste0("Dataset_",d)
    
    # We only need to create this dataset if we are performing mean matching
    # on a regression model. Throw a message about missing predictors, if necessary.
    if (any(modelTypes == "Regression" & valueSelector == "meanMatch")) {
      compDat <- completeData(miceObj,as.numeric(d))[[1]][,vara,with=FALSE]
      if (!all(complete.cases(compDat))) {
        if (d == as.character(datasets)[[1]] & verbose) {
          message(
            paste0(
               "Predictors were assigned which were not imputed."
              ," For the purpose of mean matching, these predictors with missing values will be filled"
              ," in randomly, and then remain static. To fix this message in the future,"
              ," impute any variables that contain missing values which will be used as predictors."
            )
          )
        }
        for (v in names(compDat)) {
          set(
              compDat
            , i=which(miceObj$naWhere[,v])
            , j=v
            , value=fillMissing(sum(miceObj$naWhere[,v]),miceObj$data[[v]])
          )
        }
      }
    }
    
    if (verbose) cat(ds("\ndataset",d,"\n"))
    
    for (iter in 1:iterations) {

      if(verbose) cat("iteration",iter,"\t")
      
      for (impVar in varn) {

        if(verbose) cat(" |",impVar)
        
        oldMissIndx <- which(miceObj$naWhere[,impVar])
        newMissIndx <- which(naWhere[,impVar])
        
        # Get predictions of current variable in new data.
        pred <- predict(
            miceObj$finalModels[[dIndx]][[impVar]]
          , dat[[d]][newMissIndx,]
        )$predictions
        
        # Get predictions of the prior if we need them.
        # Don't need to waste time on this if not mean matching regression model.
        regMeanMatch <- valueSelector[impVar] == "meanMatch" & modelTypes[impVar] == "Regression"
        if (regMeanMatch) {
          priorPreds <- predict(
              miceObj$finalModels[[dIndx]][[impVar]]
            , compDat
          )$predictions
          prior <- compDat[[impVar]]
          mmc <- meanMatchCandidates[[impVar]] 
        } else {
          priorPreds <- NULL
          prior <- NULL
          mmc <- NULL
        }
        
        # Update values in corresponding data.table
        set(
            dat[[d]]
          , which(naWhere[,impVar])
          , impVar
          , value = imputeFromPred(
              pred = pred
            , modelType = modelTypes[[impVar]]
            , valueSelector = valueSelector[[impVar]]
            , meanMatchCandidates = mmc
            , prior = prior
            , priorPreds = priorPreds
          )
        )
      
      }
      
      if (verbose) cat("\n")
      
    }
    
  }
  
  names(dat) <- paste0("Dataset_",datasets)
  
  impDefs <- list()
  impDefs$callParams <- list()
  impDefs$callParams$vars <- vars
  impDefs$callParams$datasets <- datasets
  impDefs$callParams$iterations <- iterations
  impDefs$data <- data.table(data)
  impDefs$naWhere <- naWhere
  impDefs$missingCounts <- missingCounts
  impDefs$imputedData <- dat
  
  class(impDefs) <- "impDefs"
  
  return(impDefs)
  
}
