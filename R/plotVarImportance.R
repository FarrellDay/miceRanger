#' @title plotVarImportance
#' @description Create a impDefs object, which contains information about the imputation process.
#' @param miceObj an object of class miceDefs, created by the miceRanger function.
#' @param display How do you want to display variable importance?
#' \itemize{
#'   \item {"Relative"} Scales the importance measure between 0-1 for each variable.
#'   \item {"Absolute"} Displays the variable importance as is. May be highly skewed.
#' }
#' @param dataset The dataset you want to plot the importance of.
#' @param ... Other arguments passed to corrplot.
#' @importFrom corrplot corrplot
#' @return nothing.
#' @examples 
#' data("sampleMiceDefs")
#' plotVarImportance(sampleMiceDefs)
#' @export
plotVarImportance <- function(
    miceObj
  , display = c("Relative","Absolute")
  , dataset = 1
  , ...
) {
  
  # Fidelity
  display <- display[[1]]
  dataset <- dataset[[1]]
  if (!display %in% c("Relative","Absolute")) stop("display value not recognized, must be 'Relative' or 'Absolute'")
  if (dataset > miceObj$callParams$m) stop("dataset is not available")

  mat <- as.matrix(miceObj$finalImport[[dataset]][,-"variable"])
  
  # If displaying relative, scale values to between 0-1 for each variable.
  if (display == "Relative") mat <- t(apply(mat,MARGIN=1,function(x) x/max(x,na.rm = TRUE)))

  rownames(mat) <- miceObj$finalImport[[dataset]]$variable
  corrplot(
      mat
    , is.corr = FALSE
    , na.label = "X"
    , method = "number"
    , ...
  )
}

