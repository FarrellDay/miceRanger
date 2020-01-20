MiceRangerErrors <- function(impVars,ogData,indx,catVars = NA) {
  mrTrueError <- as.data.table(
    sapply(
      impVars
      , function(x) {
        sapply(
          indx
          , function(i) {
            ad <- ogData[naWhere[,x],x,with=FALSE]
            id <- miceRanger::getVarImps(mrList[[i]],var=x)
            if (x %in% catVars) {
              return(sum(apply(id,2,function(d) d==ad)))
            } else {
              return(mean(sweep(id,1,as.matrix(ad))^2))
            }
          }
        )
      }
      , USE.NAMES = TRUE
      , simplify = FALSE
    )
  )
  mrTrueError$trees <- treeVec
  mrTrueError$Method <- "miceRanger\n(Mean Matching)"
  return(mrTrueError)
}

MiceRangerVErrors <- function(impVars,ogData,indx,catVars = NA) {
  mrvTrueError <- as.data.table(
    sapply(
      impVars
      , function(x) {
        sapply(
          indx
          , function(i) {
            ad <- ogData[naWhere[,x],x,with=FALSE]
            id <- miceRanger::getVarImps(mrvList[[i]],var=x)
            if (x %in% catVars) {
              # Some magic
              return(sum(apply(id,2,function(d) d==ad)))
            } else {
              return(mean(sweep(id,1,as.matrix(ad))^2))
            }
          }
        )
      }
      , USE.NAMES = TRUE
      , simplify = FALSE
    )
  )
  mrvTrueError$trees <- treeVec
  mrvTrueError$Method <- "miceRanger\n(Prediction Value)"
  return(mrvTrueError)
}

# Get sum of squared residuals and accuracy of mice for each dataset
miceErrors <- function(impVars,ogData,indx,catVars = NA) {
  miceTrueError <- as.data.table(
    sapply(
      impVars
      , function(x) {
        sapply(
          indx
          , function(i) {
            ad <- ogData[naWhere[,x],x,with=FALSE]
            id <- as.matrix(miceList[[i]]$imp[[x]])
            rownames(id) <- NULL
            if (x %in% catVars) {
              return(sum(apply(id,2,function(d) d==ad)))
            } else {
              return(mean(sweep(id,1,as.matrix(ad))^2))
            }
          }
        )
      }
      , USE.NAMES = TRUE
      , simplify = FALSE
    )
  )
  miceTrueError$trees <- treeVec
  miceTrueError$Method <- "mice"
  return(miceTrueError)
}

missForestErrors <- function(impVars,ogData,indx,catVars = NA) {
  mfTrueError <- as.data.table(
    sapply(
      impVars
      , function(x) {
        sapply(
          indx
          , function(i) {
            ad <- ogData[naWhere[,x],x,with=FALSE]
            id <- as.matrix(sapply(1:5,function(d) mfList[[i]][[d]]$ximp[[x]]))[naWhere[,x],]
            if (x %in% catVars) {
              return(sum(apply(id,2,function(d) d==ad)))
            } else {
              return(mean(sweep(id,1,as.matrix(ad))^2))
            }
          }
          
        )
        
      }
      , USE.NAMES = TRUE
      , simplify = FALSE
    )
  )
  mfTrueError$trees <- treeVec
  mfTrueError$Method <- "missForest"
  return(mfTrueError)
}

