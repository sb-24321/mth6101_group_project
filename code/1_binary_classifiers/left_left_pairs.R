#' Compare the pairs of left-left profiles taken in different years.

# Load the required data and packages ====================================================
source("code/0_load_data.R") # Contains the datasets
library(lsa) # Contains the `cosine` function, which calculates the cosine similarity.

# Get all left-sides images ==============================================================

#' Function that gets all left-left pairs of the same turtle, calculates the cosine
#' similarity, and then write data to a csv file.

left_left_pairs <- function(threshold = 0.5){

  # Also removes the unimportant columns
  left_features <- subset(features, features$orientation == 'left')[-c(2:4)] 
  turtle_identities <- unique(left_features$identity)
  
  # Stores all the data in a csv
  results <- data.frame(
                        Turtle = character(),
                        Cosine_Sim = numeric(),
                        Class = integer()
  )

  for (turtle_identity in turtle_identities){

    # Creates a feature of two rows; same turtle, but different years.
    same_turtle <- (subset(left_features, identity == turtle_identity)[,-1])

    # Extract A and B 
    A <- as.numeric(same_turtle[1,])
    B <- as.numeric(same_turtle[2,])

    # Adds the results to the dataframe.
    results <- rbind(results, data.frame(
                                         Turtle = turtle_identity,
                                         Cosine_Sim = round(cosine(A, B), 3),
                                         Class = as.integer(cosine(A, B) > threshold)
                                         ))

  }

  # Stores the findings in a csv.
  filepath <- "./outputs/1_binary_classifier_outputs"
  filename <- paste0("/left_left_thresh_", threshold, ".csv")
  dir.create(filepath, showWarnings = FALSE, recursive = TRUE)
  write.csv(results, paste0(filepath, filename), row.names = FALSE)
}

left_left_pairs(threshold = 0.5)











