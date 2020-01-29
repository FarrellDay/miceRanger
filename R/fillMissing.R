# Draws nk nonmissing values randomly from pullFrom
fillMissing <- function(nk,pullFrom) {
  sample(
      pullFrom[!is.na(pullFrom)]
    , size = nk
    , replace=TRUE
  )
}
