
rm(list=ls())
library(miceRanger)
library(data.table)
library(ggplot2)
library(ggpubr)
set.seed(1991)

# Setup
data(iris)
setDT(iris)
ampIris <- amputeData(iris,perc=0.25)

# Returns a list of datasets with different levels of missingness.
# The accuracy of imputations will be compared.
newDatMissingVec <- 1:9/10
newDatList <- lapply(
    newDatMissingVec
  , function(x) amputeData(iris,perc=x)
)

#Datasets to make
m <- 50

# Create original miceDefs object.
miceObj <- miceRanger(ampIris,m=m,returnModels = TRUE)

# Use miceDefs object to impute our amputed datasets.
newImpList <- lapply(newDatList,function(x) impute(x,miceObj,verbose=FALSE))

amputations <- sapply(names(iris),function(v) lapply(newImpList,function(i) iris[i$naWhere[,v],get(v)]),USE.NAMES = TRUE,simplify = FALSE)
imputations <- sapply(names(iris),function(v) lapply(newImpList,function(i) getVarImps(i,var=v)),USE.NAMES = TRUE,simplify = FALSE)


# Extract errors for each variable and missingness
errors <- sapply(
  names(iris)
  , function(v) {
    er <- sapply(
      1:length(newDatMissingVec)
      , function(m) {
        mat <- if (v == "Species") {
          amputations[[v]][[m]] == imputations[[v]][[m]]
        } else cor(amputations[[v]][[m]] , imputations[[v]][[m]])^2
        return(colMeans(mat))
      }
    )
    colnames(er) <- as.character(newDatMissingVec*100)
    return(data.table(er))
  }
  , USE.NAMES = TRUE
  , simplify = FALSE
)

melted <- lapply(errors,melt,measure.vars = as.character(newDatMissingVec*100))

ggplots <- lapply(
    names(iris)
  , function(x) {
    ggplot(melted[[x]],aes(x=variable,y=value)) + 
      geom_boxplot() +
      xlab("Percent New Data Missing") +
      ylab(paste0("Imputation ",if(x == "Species") "Accuracy" else "R-Squared")) +
      ggtitle(x)
  }
)

ggarrange(plotlist=ggplots)
