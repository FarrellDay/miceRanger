#' @title plotDistributions
#' @description Plots the distribution of the original data beside the imputed data.
#' @param miceObj an object of class miceDefs, created by the miceRanger function.
#' @param vars the variables you want to plot. Default is to plot all variables. Can be a vector of
#' variable names, or one of 'allNumeric' or 'allCategorical'
#' @param dotsize Passed to \code{geom_dotplot()}. Depending on the number of graphs plotted, you may want to
#' change the dot size for categorical variables.
#' @param ... additional parameters passed to \code{ggarrange()}.
#' @importFrom stats density
#' @importFrom data.table melt setnames .N
#' @importFrom ggplot2 ggplot geom_density aes_string ggplotGrob geom_dotplot geom_bar
#' @return an object of class \code{ggarrange}.
#' @examples 
#' data("sampleMiceDefs")
#' plotDistributions(sampleMiceDefs)
#' @export
plotDistributions <- function(
    miceObj
  , vars = names(miceObj$callParams$vars)
  , dotsize = 0.5
  , ...
) {
  
  varn <- names(miceObj$callParams$vars)
  newClasses <- miceObj$newClasses[varn]
  
  if (vars[[1]] == 'allCategorical') vars <- names(newClasses[newClasses == "factor"])
  if (vars[[1]] == 'allNumeric') vars <- names(newClasses[newClasses != "factor"])

  # Only keep class information about variables we are plotting:
  newClasses <- newClasses[vars]
  facVars <- newClasses[newClasses == "factor"]
  numVars <- newClasses[newClasses != "factor"]

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
  
  pList <- c(numList,facList)
  ggarrange(plotlist=pList,...)
  
}
utils::globalVariables('.')
