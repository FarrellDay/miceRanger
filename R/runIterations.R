
# Performs mice for specified datasets and iterations.

runIterations <- function(
    dat = NULL
  , m
  , maxiter
  , vars
  , naWhere
  , valueSelector
  , meanMatchCandidates
  , modelTypes
  , verbose
  , parallel
  , miceObj = NULL
  , oldm = 0
  , oldIt = 0
  , startTime
  , returnModels
  , ...
) {
  
  ds <- crayon::make_style("#4B8E78")
  varn <- names(vars)
  varp <- unique(unlist(vars))
  vara <- unique(c(varn,varp))
  
  # Define parallelization setup
  `%op%` <- ParMethod(parallel)
  if (parallel & (getDoParWorkers() == 1)) stop("parallel is set to TRUE but no back end is registered.")
  if (!parallel & (getDoParWorkers() > 1)) if (verbose) message("parallel is set to FALSE but there is a back end registered. Process will not be run in parallel.\n")
  
  # Run iterations
  dsl <- foreach(
      dataSet = 1:m
    , .options.multicore = list(preschedule=FALSE)
    , .combine = list
    , .multicombine = TRUE
    , .inorder = FALSE
    , .errorhandling = 'pass'
    , .packages = c('data.table','ranger','FNN')
    , .verbose = FALSE
  ) %op% {
    
    # Move away from for-loops and towards parLapply.
    dsImport <- list()
    dsError <- list()
    dsImps <- list()
    if (returnModels) dsMod <- list()
    
    # global binding.
    dataSet <- get("dataSet")
    
    # If  adding iterations to currently imputed datasets, start where we left off
    # If adding datasets, start new
    dats <- if (!is.null(miceObj)) copy(completeData(miceObj,datasets=dataSet,verbose=FALSE)[[1]]) else copy(dat)
    
    if(verbose) cat(ds("\ndataset",dataSet + oldm,"\n"))
    
    for (iter in 1:maxiter) {
      
      iterImps <- list()
      iterError <- list()
      iterImport <- list()
      
      if(verbose) cat("iteration",iter + oldIt,"\t")
      
      for (impVar in varn) {

        # Only feed ranger the columns we need for this imputation
        algCols <- c(impVar,vars[[impVar]])

        if(verbose) cat(" |",impVar)
        missIndx <- naWhere[,impVar]
        returnProb <- modelTypes[impVar] == "Classification" & valueSelector[impVar] == "meanMatch"
        model <- ranger(
          data = dats[!missIndx,algCols,with=FALSE]
          , dependent.variable.name = impVar
          , importance = "impurity"
          , probability = returnProb
          , verbose = FALSE
          , ...
        )
        
        # Keep the model if this is the last iteration.
        if (iter == maxiter & returnModels) dsMod[[impVar]] <- model
        
        mmc <- if (modelTypes[impVar] == "Regression" & valueSelector[impVar] == "meanMatch")  {
          meanMatchCandidates[[impVar]] 
        } else NULL
          
        
        # Extract information we need from the model.
        pred <- predict(model,dats)$predictions
        iterImps[[impVar]] <- imputeFromPred(
            pred = if (returnProb) pred[missIndx,] else pred[missIndx]
          , modelTypes[impVar]
          , valueSelector[impVar]
          , mmc
          , prior = dats[!missIndx][,get(impVar)]
          , priorPreds = if (returnProb) pred[!missIndx,] else pred[!missIndx]
        )
        dats[missIndx,(impVar) := iterImps[[impVar]]]
        iterImport[[impVar]] <- as.data.table(as.list(model$variable.importance))
        if(modelTypes[impVar] == "Regression") {
          iterError[[impVar]] <- model$r.squared
        } else {
          iterError[[impVar]] <- 1-model$prediction.error
        }
        
      }
      
      # Now that the models have been run for this iteration...
      
      # Add to iteration importance list. Sort names for pretty plotting.
      dsImport[[iter]] <- rbindlist(iterImport,fill = TRUE)
      toOrder <- names(dsImport[[iter]])
      dsImport[[iter]]$variable <- varn
      setcolorder(dsImport[[iter]],c("variable",toOrder[order(match(toOrder,vara))]))
      
      # Add to iteration model error list
      dsError[[iter]] <- iterError
      
      # Add to iteration imputation list
      dsImps[[iter]] <- iterImps
      
      rm(iterImps,iterError,iterImport)
      
      if(verbose) {
        cat("\n")
        benTime <- Sys.time()
        secDiff <- as.numeric(difftime(benTime,startTime,units="secs"))
        # Only show this message if the expected wait time is over 5 min.
        if (iter == 1 & dataSet == 1 & as.numeric(maxiter*m*secDiff) > 300) {
          cat("\nExpected Time of Completion:",as.character(startTime + as.numeric(maxiter*m*secDiff)),"\n\n")
        }
      }
      
    }
    
    # Adjust names
    names(dsImps) <- paste0("Iteration_",1:maxiter + oldIt)
    names(dsImport) <- paste0("Iteration_",1:maxiter + oldIt)
    dsError <- rbindlist(dsError)
    dsError$iteration <- 1:maxiter + oldIt
    setcolorder(dsError,c("iteration",varn))
    
    return(
      list(
          dsImport = dsImport
        , dsError = dsError
        , dsImps = dsImps
        , dsMod = if(returnModels) dsMod else NULL
      )
    )
  }
  return(dsl)
}
