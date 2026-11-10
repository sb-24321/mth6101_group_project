#' Compare the pairs of left-left profiles taken in different years.

# Load the required data and packages ====================================================
source("code/0_load_data.R") # Contains the datasets
library(lsa) # Contains the `cosine` function, which calculates the cosine similarity.

THRESHOLD <- 0.5

# Get all left-sides images ==============================================================

# Gets all left sided images, then removes unimportant columns. Leaves identity and features.
left_features <- subset(features, features$orientation == 'left')[-c(2:4)]

turtle_identities <- unique(left_features$identity)

for (turtle_identity in turtle_identities){

  # Creates a feature of two rows; same turtle, but different years.
  mat_turtle <- (subset(left_features, identity == turtle_identity)[,-1])
  
  # Extract A and B 
  A <- as.numeric(mat_turtle[1,])
  B <- as.numeric(mat_turtle[2,])
  
  cat(
      paste0(
             "\n#############", 
             "\nTurtle: ", turtle_identity, 
             "\nCosine Sim: ", round(cosine(A, B), 3),
             "\nClass: ", as.integer(cosine(A, B) > 0.6),
             "\n#############\n" 
  ))



}





