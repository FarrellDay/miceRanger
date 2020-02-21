test_that(
  
    "miceRanger - complex specifications"
    
  , {
    
    skip_on_cran()
    data(iris)
    ampDat <- amputeData(iris)
    ampDatI <- amputeData(iris)
    
    v <- list(
      Sepal.Width = c("Sepal.Length","Petal.Width","Species")
      , Sepal.Length = c("Sepal.Width","Petal.Width")
      , Species = c("Sepal.Width")
    )
    pmm <- c(
      Sepal.Width = "meanMatch"
      , Sepal.Length = "value"
      , Species = "meanMatch"
    )
    mmc <- c(
      Sepal.Width = 4
      , Species = 10
    )
    
    miceObjCustom <- miceRanger(
      ampDat
      , vars = v
      , valueSelector = pmm
      , meanMatchCandidates = mmc
      , returnModels = TRUE
      , verbose=FALSE
    )
    
    # Check completeData
    compDat <- completeData(miceObjCustom)
    expect_equal(length(compDat),miceObjCustom$callParams$m)
    expect_true(all(sapply(compDat,nrow) == 150))
    expect_true(all(sapply(compDat,function(x) complete.cases(x[,names(v),with=FALSE]))))
    
    # Check impute
    newD <- impute(ampDatI,miceObjCustom,verbose=FALSE)
    expect_equal(length(newD$imputedData),miceObjCustom$callParams$m)
    expect_true(all(sapply(newD$imputedData,nrow) == 150))
    expect_true(all(sapply(newD$imputedData,function(x) complete.cases(x[,names(v),with=FALSE]))))
    
    # Check add datasets
    miceObjCustomT <- addDatasets(miceObjCustom,datasets=1,verbose=FALSE)
    expect_equal(miceObjCustomT$callParams$m,6)
    miceObjCustomT <- addDatasets(miceObjCustom,datasets=2,verbose=FALSE)
    expect_equal(miceObjCustomT$callParams$m,7)
    
    # Check add iterations
    miceObjCustomT <- addIterations(miceObjCustom,iters=1,verbose=FALSE)
    expect_equal(miceObjCustomT$callParams$maxiter,6)
    miceObjCustomT <- addIterations(miceObjCustom,iters=2,verbose=FALSE)
    expect_equal(miceObjCustomT$callParams$maxiter,7)
    
    print(miceObjCustom)
    varImps <- getVarImps(miceObjCustom,var = "Sepal.Width")
    expect_equal(dim(varImps),c(15,5))
    
  }
  
)
