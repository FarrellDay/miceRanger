#' @title completeData
#' @description Return the completed datasets.
#' @param miceObj an object of class miceDefs.
#' @param datasets a vector of the datasets you want to return.
#' @param verbose a warning is thrown if integers are converted to doubles.
#' To suppress this warning, set to \code{FALSE}.
#' @importFrom stats complete.cases
#' @return A list of imputed datasets.
#' @examples
#' data("sampleMiceDefs")
#' imputedList <- completeData(sampleMiceDefs)
#' @export
completeData <- function(
    miceObj
  , datasets = 1:miceObj$callParams$m
  , verbose = TRUE
) {
  
  if (class(miceObj) != "miceDefs") stop("miceObj should be an object of class miceDefs.")
  
  dat <- copy(miceObj$data)
  varn <- names(miceObj$callParams$vars)
  rawClasses <- miceObj$rawClasses
  
  intToDouble <- rawClasses[varn] == "integer" & miceObj$callParams$valueSelector[varn] == "value"
  if(any(intToDouble)) {
    intToDouble <- names(intToDouble[intToDouble])
    if (verbose) message(paste0(paste0(intToDouble,collapse = ","))," are integers, and will be converted to double because valueSelector = 'value'")
    dat[,(intToDouble) := lapply(.SD,as.double),.SDcols=intToDouble]  
  }
  
  completeSets <- sapply(
      datasets
    , function(x) {
      dat <- copy(dat)
      for (v in varn) {
        dat[miceObj$naWhere[,v],(v) := miceObj$finalImps[[x]][[v]]]
      }
      return(dat)
    }
    , USE.NAMES = TRUE
    , simplify = FALSE
  )
  
  names(completeSets) <- paste0("Dataset_",datasets)
  
  if (
    any(
      sapply(
          completeSets
        , function(x) mean(
            complete.cases(
              x[,varn,with=FALSE]
            )
        )
      ) != 1)
    ) stop("There was a problem completing the dataset. Please report this with a reproduceable example to https://github.com/farrellday/miceRanger/issues")
  
  return(completeSets)
  
}