#' @title plotModelError
#' @description Create a impDefs object, which contains information about the imputation process.
#' @param miceObj an object of class miceDefs, created by the miceRanger function.
#' @param vars the variables you want to plot. Default is to plot all variables. Can be a vector of
#' variable names, or one of 'allNumeric' or 'allCategorical'
#' @param pointSize passed to \code{geom_point}, allows user to change dot size.
#' @param ... other arguments passed to \code{grid.arrange()}
#' @importFrom data.table data.table
#' @importFrom ggplot2 geom_line
#' @importFrom gridExtra grid.arrange
#' @return nothing.
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
  
  
  if (vars[[1]] == 'allCategorical') vars <- names(miceObj$newClasses[miceObj$newClasses == "factor"])
  if (vars[[1]] == 'allNumeric') vars <- names(miceObj$newClasses[miceObj$newClasses != "factor"])
  
  pList <- lapply(
      vars
    , function(var) {
      metric <- if (miceObj$newClasses[var] == "factor") "(OOB Accuracy)" else "(OOB R-Squared)"
      mat <- data.table(matrix(sapply(miceObj$allError,function(x) x[[var]]),ncol=1))
      mat$dataset <- factor(sort(rep(1:miceObj$callParams$m,miceObj$callParams$maxiter)))
      mat$iteration <- factor(rep(1:miceObj$callParams$maxiter,miceObj$callParams$m))
      setnames(mat,"V1",var)
      return(
        ggplotGrob(
          ggplot(mat,aes_string(x="iteration",y=var,group="dataset")) +
            geom_line() +
            geom_point(size=pointSize) +
            ylab(paste0(var,"\n",metric))
        )
      )
    }
  )

  grid.arrange(grobs = pList,...)
  
}

