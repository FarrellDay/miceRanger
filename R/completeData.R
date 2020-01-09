#' @title completeData
#' @description Create a impDefs object, which contains information about the imputation process.
#' @param miceObj an object of class miceDefs.
#' @param datasets a vector of the datasets you want to return.
#' @importFrom stats complete.cases
#' @return A list of imputed datasets.
#' @examples
#' data(iris)
#' 
#' ampIris <- amputeData(iris)
#' 
#' miceObj <- miceRanger(
#'   ampIris
#'   , m = 2
#'   , maxiter = 2
#'   , verbose=FALSE
#' )
#' imputedList <- completeData(miceObj)
#' @export
completeData <- function(
    miceObj
  , datasets = 1:miceObj$callParams$m
) {
  
  if (class(miceObj) != "miceDefs") stop("miceObj should be an object of class miceDefs.")
  
  dat <- copy(miceObj$data)
  
  intVar <- names(miceObj$rawClasses[miceObj$rawClasses == "integer"])
  if(any(miceObj$rawClasses == "integer") & miceObj$callParams$valueSelector == "value") {
    warning(paste0(paste0(intVar,collapse = ","))," are integers, and will be converted to double because valueSelector = 'value'")
    dat[,(intVar) := lapply(.SD,as.double),.SDcols=intVar]  
  }
  
  completeSets <- sapply(
      datasets
    , function(x) {
      dat <- copy(dat)
      for (v in miceObj$callParams$vars) {
        dat[miceObj$naWhere[,v],(v) := miceObj$finalImps[[x]][[v]]]
      }
      return(dat)
    }
    , USE.NAMES = TRUE
    , simplify = FALSE
  )
  
  names(completeSets) <- paste0("Dataset_",datasets)
  
  if (any(sapply(completeSets,function(x) mean(complete.cases(x))) != 1)) stop("There was a problem completing the dataset. Please report this with a reproduceable example to https://github.com/farrellday/miceRanger/issues")
  
  return(completeSets)
  
}