#' @importFrom FNN knnx.index
imputeFromPred <- function(
    pred
  , modelType
  , valueSelector
  , meanMatchCandidates
  , posterior
  , missIndx
)
{

  if (valueSelector == "value") {
    return(pred[missIndx])
  } else {
    if (modelType == "Classification") {
      lvls <- colnames(pred)
      return(apply(pred[missIndx,],MARGIN=1,function(x) sample(lvls,prob=x,size=1)))
    } else if (modelType == "Regression") {
      # For each prediction of a missing value, find the closest values in the
      # predictions for the non-missing values.
      nearest <- knnx.index(pred[!missIndx],pred[missIndx],k=meanMatchCandidates)
      nearest <- posterior[apply(nearest,MARGIN=1,function(x) sample(x,size=1))]
      return(nearest)
    }
  }
}
