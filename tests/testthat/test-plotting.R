context('Diagnostic Plotting')

pCorr <- plotCorrelations(sampleMiceDefs)
pDist <- plotDistributions(sampleMiceDefs)
pImpV <- plotImputationVariance(sampleMiceDefs)
pMoEr <- plotModelError(sampleMiceDefs)
pVcon <- plotVarConvergence(sampleMiceDefs)
pVImp <- plotVarImportance(sampleMiceDefs)

v <- "allNumeric"
pCorrN <- plotCorrelations(sampleMiceDefs,vars=v)
pDistN <- plotDistributions(sampleMiceDefs,vars=v)
pImpVN <- plotImputationVariance(sampleMiceDefs,vars=v)
pMoErN <- plotModelError(sampleMiceDefs,vars=v)
pVconN <- plotVarConvergence(sampleMiceDefs,vars=v)

v <- "allCategorical"
pCorrC <- plotCorrelations(sampleMiceDefs,vars=v)
pDistC <- plotDistributions(sampleMiceDefs,vars=v)
pImpVC <- plotImputationVariance(sampleMiceDefs,vars=v)
pMoErC <- plotModelError(sampleMiceDefs,vars=v)
pVconC <- plotVarConvergence(sampleMiceDefs,vars=v)

v <- "Species"
pCorrC <- plotCorrelations(sampleMiceDefs,vars=v)
pDistC <- plotDistributions(sampleMiceDefs,vars=v)
pImpVC <- plotImputationVariance(sampleMiceDefs,vars=v)
pMoErC <- plotModelError(sampleMiceDefs,vars=v)
pVconC <- plotVarConvergence(sampleMiceDefs,vars=v)

v <- "Petal.Width"
pCorrC <- plotCorrelations(sampleMiceDefs,vars=v)
pDistC <- plotDistributions(sampleMiceDefs,vars=v)
pImpVC <- plotImputationVariance(sampleMiceDefs,vars=v)
pMoErC <- plotModelError(sampleMiceDefs,vars=v)
pVconC <- plotVarConvergence(sampleMiceDefs,vars=v)
