context('Diagnostic Plotting')

test_that(
  "Everything"
  
  , {
    skip_on_cran()
    pCorr <- plotCorrelations(sampleMiceDefs)
    pDist <- plotDistributions(sampleMiceDefs)
    pImpV <- plotImputationVariance(sampleMiceDefs)
    pMoEr <- plotModelError(sampleMiceDefs)
    pVcon <- plotVarConvergence(sampleMiceDefs)
    pVImp <- plotVarImportance(sampleMiceDefs)
  }
  
)

test_that(
  "allNumeric"
  
  , {
    skip_on_cran()
    v <- "allNumeric"
    pCorrN <- plotCorrelations(sampleMiceDefs,vars=v)
    pDistN <- plotDistributions(sampleMiceDefs,vars=v)
    pImpVN <- plotImputationVariance(sampleMiceDefs,vars=v)
    pMoErN <- plotModelError(sampleMiceDefs,vars=v)
    pVconN <- plotVarConvergence(sampleMiceDefs,vars=v)
  }
  
)



test_that(
  "allCategorical"
  
  , {
    skip_on_cran()
    v <- "allCategorical"
    pCorrC <- plotCorrelations(sampleMiceDefs,vars=v)
    pDistC <- plotDistributions(sampleMiceDefs,vars=v)
    pImpVC <- plotImputationVariance(sampleMiceDefs,vars=v)
    pMoErC <- plotModelError(sampleMiceDefs,vars=v)
    pVconC <- plotVarConvergence(sampleMiceDefs,vars=v)
  }
  
)



test_that(
  "Species"
  
  , {
    skip_on_cran()
    v <- "Species"
    pCorrC <- plotCorrelations(sampleMiceDefs,vars=v)
    pDistC <- plotDistributions(sampleMiceDefs,vars=v)
    pImpVC <- plotImputationVariance(sampleMiceDefs,vars=v)
    pMoErC <- plotModelError(sampleMiceDefs,vars=v)
    pVconC <- plotVarConvergence(sampleMiceDefs,vars=v)
  }
  
)

test_that(
  "Petal.Width"
  
  , {
    skip_on_cran()
    v <- "Petal.Width"
    pCorrC <- plotCorrelations(sampleMiceDefs,vars=v)
    pDistC <- plotDistributions(sampleMiceDefs,vars=v)
    pImpVC <- plotImputationVariance(sampleMiceDefs,vars=v)
    pMoErC <- plotModelError(sampleMiceDefs,vars=v)
    pVconC <- plotVarConvergence(sampleMiceDefs,vars=v)
  }
  
)


