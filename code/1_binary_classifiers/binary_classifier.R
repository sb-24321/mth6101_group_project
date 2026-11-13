
# Load the required data and packages ===============================================

source("code/0_load_data.R")

library(lsa)
library(dplyr)
library(stringr)


# Binary classifier ==================================================================

# Implements a threshold-dependent binary classifier using cosine similarity.
# Two types of image pairs are compared:
# 1. Left-Left (LL)
# 2. Right-Left (RL)
#
# Only images taken in different years are compared.
# Y_true: 1 = same turtle, 0 = different turtles.
# Y_pred: 1 = predicted same turtle, 0 = predicted different turtles.


classify <- function(data, orientation_1, orientation_2, threshold) {

  cat("Comparing", orientation_1, "-", orientation_2, "pairs...\n")

  # Separate the images according to their orientations
  data_1 <- filter(data, orientation == orientation_1)
  data_2 <- filter(data, orientation == orientation_2)

  # Create an empty results table
  results <- data.frame(
    Turtle_1 = character(),
    Turtle_2 = character(),
    Date_1 = character(),
    Date_2 = character(),
    Orientation_1 = character(),
    Orientation_2 = character(),
    Similarity = numeric(),
    Y_true = integer(),
    Y_pred = integer()
  )

  cat("Classifying pairs from different years...\n")

  # Compare all eligible image pairs
  for (i in seq_len(nrow(data_1))) {

    for (j in seq_len(nrow(data_2))) {

      # For LL comparisons, avoid comparing an image with itself
      # or counting the same pair twice.
      if (orientation_1 == orientation_2 && j <= i) {
        next
      }

      # Extract the dates
      date_1 <- as.character(data_1$date[i])
      date_2 <- as.character(data_2$date[j])

      # Only compare images taken in different years
      if (substr(date_1, 1, 4) != substr(date_2, 1, 4)) {

        # Extract turtle identities
        identity_1 <- as.character(data_1$identity[i])
        identity_2 <- as.character(data_2$identity[j])

        # Determine the actual label
        y_true <- as.integer(identity_1 == identity_2)

        # Extract the feature vectors (columns 5 onwards)
        features_1 <- as.numeric(
          unlist(data_1[i, 5:ncol(data_1)], use.names = FALSE)
        )

        features_2 <- as.numeric(
          unlist(data_2[j, 5:ncol(data_2)], use.names = FALSE)
        )

        # Calculate cosine similarity
        similarity <- as.numeric(
          lsa::cosine(features_1, features_2)
        )

        # Apply the threshold decision rule
        y_pred <- as.integer(similarity > threshold)

        # Add the results to the results table
        results <- rbind(results, data.frame(
          Turtle_1 = identity_1,
          Turtle_2 = identity_2,
          Date_1 = date_1,
          Date_2 = date_2,
          Orientation_1 = orientation_1,
          Orientation_2 = orientation_2,
          Similarity = similarity,
          Y_true = y_true,
          Y_pred = y_pred
        ))
      }
    }
  }

  cat("Finished classifying.\n")

  # Save the results to a CSV file
  filepath <- "./outputs/1_binary_classifier_outputs"

  filename <- paste0(
    "/", orientation_1, "_", orientation_2,
    "_thresh_", threshold, ".csv"
  )

  dir.create(filepath, showWarnings = FALSE, recursive = TRUE)

  write.csv(
    results,
    paste0(filepath, filename),
    row.names = FALSE
  )

  cat("Results saved in:", filepath, filename, "\n")

  return(results)
}


# Main code ==========================================================================

cat("\n### Started binary classifier ###\n")

# Run the Left-Left classifier
LL_results <- classify(
  features,
  "left",
  "left",
  threshold = 0.5
)

# Run the Right-Left classifier
RL_results <- classify(
  features,
  "right",
  "left",
  threshold = 0.5
)

cat("\n### Finished binary classifier ###\n")
