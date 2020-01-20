#' @title plotCorrelations
#' @description Plot the correlation of imputed values between every combination of datasets for each variable.
#' @param miceObj an object of class miceDefs, created by the miceRanger function.
#' @param vars the variables you want to plot. Default is to plot all variables. Can be a vector of
#' variable names, or one of 'allNumeric' or 'allCategorical'
#' @param factCorrMetric The correlation metric for categorical variables. Can be one of:
#' \itemize{
#'   \item {\code{"CramerV"}} Cramer's V correlation metric.
#'   \item {\code{"Chisq"}} Chi Square test statistic.
#'   \item {\code{"TschuprowT"}} Tschuprow's T correlation metric.
#'   \item {\code{"Phi"}} (Binary Variables Only) Phi coefficient.
#'   \item {\code{"YuleY"}} (Binary Variables Only) Yule's Y, also known as coefficient of colligation
#'   \item {\code{"YuleQ"}} (Binary Variables Only) Yule's Q, related to Yule's Y by Q=2Y/(1+Y^2)
#' }
#' @param numbCorrMetric The correlation metric for numeric variables. Can be one of:
#' \itemize{
#'   \item {\code{"pearson"}} Pearson's Correlation Coefficient
#'   \item {\code{"spearman"}} Spearman's Rank Correlation Coefficient
#'   \item {\code{"kendall"}} Kendall's Rank Correlation Coefficient
#'   \item {\code{"Rsquared"}} R-squared
#' }
#' @param ... Other arguments to pass to grid.arrange()
#' @importFrom ggplot2 ggplot geom_point ylab aes theme aes_string geom_boxplot
#' @importFrom utils combn
#' @importFrom ggpubr ggarrange theme_classic2
#' @return an object of class \code{ggarrange}.
#' @examples 
#' data("sampleMiceDefs")
#' plotCorrelations(sampleMiceDefs)
#' @export
plotCorrelations <- function(
    miceObj
  , vars = names(miceObj$callParams$vars)
  , factCorrMetric = "CramerV"
  , numbCorrMetric = "pearson"
  , ...
) {
  
  varn <- names(miceObj$callParams$vars)
  newClasses <- miceObj$newClasses[varn]
  
  if (vars[[1]] == 'allCategorical') vars <- names(newClasses[newClasses == "factor"])
  if (vars[[1]] == 'allNumeric') vars <- names(newClasses[newClasses != "factor"])
  
  selTheme <- theme_classic2()
  m <- miceObj$callParams$m
  maxiter <- miceObj$callParams$maxiter
  
  if (m < 3) stop("Cannot plot correlations with 2 or less datasets. This functionality will be implemented at a later time.")
  
  # Define Correlation Metrics:
  catCorrMet <- getFactCorrMetric(factCorrMetric)
  numCorrMet <- getNumbCorrMetric(numbCorrMetric)
  
  pList <- lapply(
    vars
    , function(var) {
      # Type of plot depends on variable type.
      if (newClasses[var] == "factor") {
        varClass <- "factor"
        ylb <- paste0(var,catCorrMet$ylb)
      } else {
        varClass <- "numeric"
        ylb <- paste0(var,numCorrMet$ylb)
      }

      # Calculate correlation metric for every combination
      # of datasets at each iteration.
      iterFlipped <- as.data.table(
        sapply(
          1:maxiter
          , function(x) {
            #x <- 1
            mat <- apply(
              combn(1:m,m=2)
              , MARGIN=2
              , function(ind) {
                #ind <- combn(1:m,m=2)[,1]
                i <- ind[[1]]
                j <- ind[[2]]
                if(varClass == "factor") {
                  return(
                    catCorrMet$corrMet(
                        miceObj$allImps[[i]][[x]][[var]]
                      , miceObj$allImps[[j]][[x]][[var]]
                    )
                  )
                } else {
                  return(
                    numCorrMet$corrMet(
                        miceObj$allImps[[i]][[x]][[var]]
                      , miceObj$allImps[[j]][[x]][[var]]
                    )
                  )
                }
                
              }
            )
            return(mat)
          }
        )
      )
      
      setnames(iterFlipped,as.character(1:maxiter))
      iterFlipped <- melt(iterFlipped,measure.vars = names(iterFlipped))
      iterFlipped$variable <- factor(as.numeric(iterFlipped[,get("variable")]))
      
      return(
          ggplot(iterFlipped,aes_string(x="variable",y="value")) + 
            geom_boxplot() +
            ylab(ylb) +
            selTheme +
            xlab("Iteration")
      )
    }
  )
  
  ggarrange(plotlist = pList,...)
  
}
