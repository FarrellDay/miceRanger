#' @title plotModelError
#' @description Plot the Out Of Bag model error for specified variables over all tierations.
#' @param miceObj an object of class miceDefs, created by the miceRanger function.
#' @param vars the variables you want to plot. Default is to plot all variables. Can be a vector of
#' variable names, or one of 'allNumeric' or 'allCategorical'
#' @param pointSize passed to \code{geom_point}, allows user to change dot size.
#' @param ... other arguments passed to \code{grid.arrange()}
#' @importFrom data.table data.table
#' @importFrom ggplot2 ggplot geom_line
#' @importFrom gridExtra grid.arrange
#' @return an object of class \code{ggarrange}.
#' @examples 
#' data("sampleMiceDefs")
#' plotModelError(sampleMiceDefs)
#' @export
plotModelError <- function(
    miceObj
  , vars = names(miceObj$callParams$vars)
  , pointSize = 1
  , ...
) {
  
  varn <- names(miceObj$callParams$vars)
  newClasses <- miceObj$newClasses[varn]
  
  if (vars[[1]] == 'allCategorical') vars <- names(newClasses[newClasses == "factor"])
  if (vars[[1]] == 'allNumeric') vars <- names(newClasses[newClasses != "factor"])
  
  pList <- lapply(
      vars
    , function(var) {
      metric <- if (miceObj$newClasses[var] == "factor") "(OOB Accuracy)" else "(OOB R-Squared)"
      mat <- data.table(matrix(sapply(miceObj$allError,function(x) x[[var]]),ncol=1))
      mat$dataset <- factor(sort(rep(1:miceObj$callParams$m,miceObj$callParams$maxiter)))
      mat$iteration <- factor(rep(1:miceObj$callParams$maxiter,miceObj$callParams$m))
      setnames(mat,"V1",var)
      return(
        ggplot(mat,aes_string(x="iteration",y=var,group="dataset")) +
          geom_line() +
          geom_point(size=pointSize) +
          ylab(paste0(var,"\n",metric))
      )
    }
  )

  ggarrange(plotlist = pList,...)
  
}

