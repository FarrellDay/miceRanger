#' @title plotDistributions
#' @description Plots the distribution of the original data beside the imputed data.
#' @param miceObj an object of class miceDefs, created by the miceRanger function.
#' @param vars the variables you want to plot. Default is to plot all variables. Can be a vector of
#' variable names, or one of 'allNumeric' or 'allCategorical'
#' @param title The title of the plot. Default is no title.
#' @param dotsize Passed to geom_dotplot(). Depending on the number of graphs plotted, you may want to
#' change the dot size for categorical variables.
#' @param ... additional parameters passed to ggarrange when combining numeric and categorical
#' plots.
#' @importFrom stats density
#' @importFrom data.table melt setnames .N
#' @importFrom ggplot2 ggplot geom_density aes_string ggplotGrob geom_dotplot geom_bar
#' @return nothing.
#' @examples 
#' data(iris)
#' ampIris <- amputeData(iris)
#' miceObj <- miceRanger(
#'   ampIris
#'   , m = 2
#'   , maxiter = 3
#'   , verbose=FALSE
#' )
#' plotDistributions(miceObj)
#' @export
plotDistributions <- function(
    miceObj
  , vars = miceObj$callParams$vars
  , title=NULL
  , dotsize = 0.5
  , ...
) {
  
  #vars <- "Foundation"
  #vars <- "allNumeric"
  #vars <- "allCategorical"
  #require(ggplot2)
  #require(ggpubr)
  
  if (vars[[1]] == 'allCategorical') vars <- names(miceObj$newClasses[miceObj$newClasses == "factor"])
  if (vars[[1]] == 'allNumeric') vars <- names(miceObj$newClasses[miceObj$newClasses != "factor"])
  facVars <- miceObj$newClasses[vars][miceObj$newClasses[vars] == "factor"]
  numVars <- miceObj$newClasses[vars][miceObj$newClasses[vars] != "factor"]

  if (length(facVars) > 0) {
    facList <- lapply(names(facVars), function(var) {
      dat <- as.data.table(lapply(miceObj$finalImps,function(x) x[[var]]))
      dat <- melt(dat,measure.vars = names(dat))
      setnames(dat,"value",var)
      agg <- dat[,.("Percentage" = .N/sum(miceObj$naWhere[,var])),by=c("variable",var)]
      rawAgg <- miceObj$data[!is.na(get(var))][,.("Percentage" = .N/sum(!miceObj$naWhere[,var])),by=var]
      return(
        ggplot() + 
          geom_dotplot(data=agg,aes_string(x=var,y="Percentage"),binaxis='y', stackdir='center',dotsize = dotsize,stackratio=0.75,binwidth=1/50) +
          geom_bar(data=rawAgg,aes_string(x=var,y="Percentage"),stat="identity",alpha=0.5)
        )
      }
    )
  } else facList <- NULL
  
  if (length(numVars) > 0) {
    numList <- lapply(names(numVars), function(var) {
      dat <- as.data.table(lapply(miceObj$finalImps,function(x) x[[var]]))
      dat <- melt(dat,measure.vars = names(dat))
      setnames(dat,"value",var)
      dens <- density(miceObj$data[,get(var)],na.rm=TRUE)
      return(
        ggplot() + 
          geom_density(data=dat,aes_string(var,group="variable"),bw=dens$bw) +
          geom_density(data=miceObj$data[!is.na(get(var))],aes_string(var),size=1,color="red",bw=dens$bw) +
          ylab("Density")
      )
    })
  } else numList <- NULL
  
  
  if (length(facList) == 0) {
    ggarrange(plotlist=numList,...)
  } else if (length(numList) == 0) {
     ggarrange(plotlist=facList,...)
  } else {
    ggarrange(
      plotlist=list(
        if (length(facList) > 0) ggarrange(plotlist=facList) else NULL
        , if (length(numList) > 0) ggarrange(plotlist=numList) else NULL
      )
      , ...
    )
  }
  
}
utils::globalVariables('.')
