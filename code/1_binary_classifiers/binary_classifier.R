#' Creates a binary classifier, that is dependent on a threshold.

# Load `lsa` library =====================================================================
library(lsa) # contains the `cosine` function to find similarity.

# Build the classifier ===================================================================

bin_classifier <- function(x, y, threshold = 0.5){
  as.integer(cosine(x, y) > threshold)
}
