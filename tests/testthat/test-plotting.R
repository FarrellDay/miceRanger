context('Diagnostic Plotting')

pCorr <- plotCorrelations(sampleMiceDefs)
pDist <- plotDistributions(sampleMiceDefs)
pImpV <- plotImputationVariance(sampleMiceDefs)
pMoEr <- plotModelError(sampleMiceDefs)
pVcon <- plotVarConvergence(sampleMiceDefs)
pVImp <- plotVarImportance(sampleMiceDefs)