#' @title plotVarImportance
#' @description Plot the variable importance for each imputed variable.
#' @param miceObj an object of class miceDefs, created by the miceRanger function.
#' @param display How do you want to display variable importance?
#' \itemize{
#'   \item {"Relative"} Scales the importance measure between 0-1 for each variable.
#'   \item {"Absolute"} Displays the variable importance as is. May be highly skewed.
#' }
#' @param dataset The dataset you want to plot the variable importance of.
#' @param ... Other arguments passed to corrplot.
#' @importFrom corrplot corrplot
#' @importFrom grDevices colorRampPalette
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
  Args <- as.list(match.call())
  cpal <- colorRampPalette(c("white","#D8DEFF","#002BFF"))

  corrplot(
      mat
    , is.corr = FALSE
    , col = if(any(names(Args) == "col")) Args$col else cpal(200)
    , na.label = if(any(names(Args) == "na.label")) Args$na.label else "X"
    , method = if(any(names(Args) == "method")) Args$method else "number"
    , ...
  )
}





