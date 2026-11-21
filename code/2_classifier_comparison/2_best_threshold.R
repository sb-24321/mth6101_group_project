# TODO:
#
# We tried to use Youden's index, which balanced Sensitivity and Specificity but that
# only gave worse thresholds, this is not for finding the best thresholds
# So we will brute force it. (this is gonna take a while)

MSE <- function(FP, FN, P, N){
  return(
    (FP + FN) / (P + N)
  )
}

best_threshold <- function(){
  # This function find the best threshold for the dataset.
  # We will do this by minimising the MSE.
  # We will just brute force ts
}
