
rm(list = ls())
require(data.table)
library(miceRanger)
set.seed(1991)

cat("This machine has:",parallel::detectCores(),"cores.\n")
cat("Original benchmarking machine had 12 cores and 16GB RAM.\n")

# Helper function for procuring factors that are correlated with
# different columns.
getFactorVar <- function(ind,dat) {
  apply(
    dat
    , MARGIN = 1
    , function(x) {
      x[1:10] <- as.numeric(x[1:10])
      sample(
        LETTERS[ind]
        , size=1
        , prob = abs(as.numeric(x[ind]))/sum(abs(as.numeric(x[ind])))
      )
    }
  )
}

# Create artificial data
createData <- function(nrows) {
  dat <- data.table(a = runif(nrows))
  dat$b <- dat$a + rnorm(nrows)
  dat$c <- dat$b + rnorm(nrows)
  dat$d <- dat$c + rnorm(nrows)
  dat$e <- dat$d + rnorm(nrows)
  dat$f <- dat$e + rnorm(nrows)
  dat$g <- dat$f + rnorm(nrows)
  dat$h <- dat$g + rnorm(nrows)
  dat$i <- dat$h + rnorm(nrows)
  dat$j <- dat$i + rnorm(nrows)
  dat$k <- factor(getFactorVar(1:2,dat))
  dat$l <- factor(getFactorVar(3:4,dat))
  dat$n <- factor(getFactorVar(5:10,dat))
  dat$o <- factor(getFactorVar(1:10,dat))
  dat$p <- factor(getFactorVar(8:10,dat))
  return(dat)
}

origDataSize <- c(100,200,400,800,1600,3200,6400,12800,25600,51200)
miceTimeList <- as.numeric()
miceErrorList <- list()
impErrorList <- list()
newDatTimes <- list()
newDatImpList <- list()

for (i in 1:length(origDataSize)) {

  # Create data
  dat <- createData(origDataSize[[i]])
  
  # Ampute the data
  ampDat <- amputeData(dat,perc=0.25)
  
  # Run miceRanger
  cat("\nRunning miceRanger with",origDataSize[[i]],"rows\n")
  miceTimeList[[i]] <- system.time(
    miceObj <- miceRanger(
      ampDat
      , m = 5
      , returnModels = TRUE
      , verbose = FALSE
      , num.trees = 100
      , max.depth = 10
    )
  )[[3]]
  
  # Get a list of variable imputations by dataset
  imputations <- sapply(
      names(dat)
    , function(v) getVarImps(miceObj,var=v)
    , USE.NAMES = TRUE
    , simplify = FALSE
  )
  
  # Get a list of the amputations we created with amputeData:
  amputations <- sapply(
      names(dat)
    , function(v) dat[miceObj$naWhere[,v],get(v)]
    , USE.NAMES = TRUE
    , simplify = FALSE
  )
  
  # Record the actual performance of the imputations
  # r-squared for numerics
  # accuracy for factors
  miceErrorList[[i]] <- sapply(
    names(dat)
    , function(v) {
      if (v %in% c("k","l","n","o","p")) {
        mat <- imputations[[v]] == amputations[[v]]
        return(colMeans(mat))
      } else {
        mat <- apply(imputations[[v]],2,function(x) cor(x,amputations[[v]])^2)
        return(mat)
      }
    }
  )
  
  # Impute different sized datasets
  newDataSize <- c(100,200,400,800,1600,3200,6400,12800,25600,51200)
  newDatList <- lapply(newDataSize,function(x) createData(x))
  newDatAmpList <- lapply(newDatList,function(x) amputeData(x,perc=0.25))
  
  # Record time and imputations of each new dataset we just created
  newDatTimes[[i]] <- as.numeric()
  for (x in 1:length(newDataSize)) {
    cat("Imputing New Data with",newDataSize[[x]],"rows\n")
    newDatTimes[[i]][[x]] <- system.time(
      newDatImpList[[x]] <- impute(newDatAmpList[[x]],miceObj,verbose = FALSE)
    )[[3]]
  }

  # Get amputations from new data
  amputations <- lapply(
      1:length(newDataSize)
    , function(x) {
      sapply(
        names(dat)
        , function(v) newDatList[[x]][newDatImpList[[x]]$naWhere[,v],get(v)]
        , USE.NAMES = TRUE
        , simplify = FALSE
      )
    }
  )
  
  # Get imputations from new data
  imputations <- lapply(
    1:length(newDataSize)
    , function(x) {
      sapply(
        names(dat)
        , function(v) getVarImps(newDatImpList[[x]],var=v)
        , USE.NAMES = TRUE
        , simplify = FALSE
      )
    }
  )
  
  # Record the actual performance of the new imputations
  # r-squared for numerics
  # accuracy for factors
  impErrorList[[i]] <- lapply(
    1:length(newDataSize)
    , function(x) {
      sapply(
        names(dat)
        , function(v) {
          if (v %in% c("k","l","n","o","p")) {
            mat <- imputations[[x]][[v]] == amputations[[x]][[v]]
            return(colMeans(mat))
          } else {
            mat <- apply(imputations[[x]][[v]],2,function(t) cor(t,amputations[[x]][[v]])^2)
            return(mat)
          }
        }
      )
    }
  )
}

times <- data.table(round(t(matrix(unlist(newDatTimes),ncol=length(origDataSize))),1))
names(times) <- as.character(newDataSize)
times$`MICE Data Rows` <- origDataSize
times$`MICE Time` <- round(miceTimeList,1)

setcolorder(times,c("MICE Data Rows","MICE Time",newDataSize))

kable(times) %>%
  kable_styling("striped") %>%
  add_header_above(
    c(
      " "= 2
      ,"Imputed Rows" = 10
    )
  )


# For the sake of simplicity, we will only look at the cases 
# with the largest numbers of rows.

  # Each element in miceErrorList is different original data rows.
  miceErrors <- data.table(colMeans(miceErrorList[[10]]))
  miceErrors$Var <- names(dat)
  miceErrors$Method <- "MICE"

  # Each element in impErrorList is different new data rows

  impErrors <- data.table(colMeans(impErrorList[[10]][[10]]))
  impErrors$Var <- names(dat)
  impErrors$Method <- "Impute New"
  
  allErrors <- rbind(miceErrors,impErrors)  

  ggplot(allErrors,aes(x=Var,y=V1,fill=Method)) + 
    geom_bar(stat="identity",position="dodge",width=0.5) +
    ylab("R-Squared/Accuracy") +
    xlab("Variable") +
    ggtitle("Imputation Accuracy - MICE vs Impute Method")
  
  ggsave("benchmarks/graphics/accuracyImputeVsMICE.png")
  
  
  
  