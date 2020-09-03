#' @importFrom FNN knnx.index
imputeFromPred <- function(
    pred
  , modelType
  , valueSelector
  , meanMatchCandidates
  , prior
  , priorPreds
)
{

  # pred - The output from the model of the samples you want to impute
  # modelType - Classification or Regression
  # valueSelector - meanMatch or value
  # meanMatchCandidates - Integer
  # prior - Unaltered values of original nonmissing data
  # priorPreds model predictions associated with prior

  if (valueSelector == "value") {
    return(pred)
  } else {
    if (modelType == "Classification") {

      # boundary case: if a categorical variables has only one missing
      # value, the pred object will be a vector instead of a matrix.
      if(!is.matrix(pred)){
        pred_names <- names(pred)
        pred <- matrix(pred, nrow = 1)
        colnames(pred) <- pred_names
      }

      lvls <- colnames(pred)
      return(apply(pred,MARGIN=1,function(x) sample(lvls,prob=x,size=1)))
    } else if (modelType == "Regression") {
      # For each prediction of a missing value, find the closest values in the
      # predictions for the non-missing values.
      nearest <- knnx.index(priorPreds,pred,k=meanMatchCandidates)
      nearest <- prior[apply(nearest,MARGIN=1,function(x) sample(x,size=1))]
      return(nearest)
    }
  }
}
