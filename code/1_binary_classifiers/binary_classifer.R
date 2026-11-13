# Load the required data and packages ====================================================
source("code/0_load_data.R") # Contains the datasets
library(lsa) # Contains the `cosine` function, calculates cosine similarity.

library(dplyr)
library(stringr) # string manipulation to extract the years

# Binary classifier ======================================================================
#' Implementation of the threshold dependent binary classifier. We will use the decision
#' rule of s(x, y) > T, where s(x, y) is the cosine similarity and T is our threshold.

#' Need to use the classifier against two types of pairs of images: left-left and right-
#' left, both taken in different years.

#' TODO: right-left pairs.

classify <- function(data, orientation_1, orientation_2, threshold){

  # Same side matching, so we use left-left.
  if (orientation_1 == orientation_2){
    cat("Comparing Left-Left Pairs...\n")
    data <- filter(data, orientation == 'left') 
  } else {
    cat("Comparing Right-Left Pairs...\n")
  }


  cat("Creating results table...\n")
  # create a results table
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


  cat("Classifiying pairs from different years...\n")

  for (i in 1:nrow(data)){
    for (j in i:nrow(data)){
      #cat(paste0("Iteration Tracker: i = ", i, ", j = ", j, ", nrow = ", nrow(data), "\n"))

      # We now have: data[i,] and data[j,] as two different entries.

      # Now we will check if they are of different years. If they are observations from
      # different years, then we will can classify them. Otherwise we ignore them.

      # We have two different cases here: finding left-left pairs and finding right-left
      # pairs from different years.

      if (substr(data[i,]$date, 1, 4) != substr(data[j,]$date, 1, 4)){
        # data[i,] and data[j,] are observations from different years, we will now 
        # classify them.

        # See if its the same turtle, 0 if they are different.
        y_true_val <- 0
        if (data[i,]$identity == data[j,]$identity) y_true_val <- 1

        # Classify the two observations
        features_1 <- as.numeric(data[i, -c(1:4)])
        features_2 <- as.numeric(data[j, -c(1:4)])
        similarity <- lsa::cosine(features_1, features_2)
        y_pred     <- as.integer(similarity > threshold)

        identity_1 <- data[i,1]
        identity_2 <- data[j,1]
        
        # Adds the results to the dataframe.
        results <- rbind(results, data.frame(
                                             Turtle_1 = identity_1,
                                             Turtle_2 = identity_2,
                                             Date_1 = data[i,]$date,
                                             Date_2 = data[j,]$date,
                                             Orientation_1 = data[i,]$orientation,
                                             Orientation_2 = data[j,]$orientation,
                                             Similarity = similarity,
                                             Y_true = ifelse(identity_1 == identity_2, 1, 0),
                                             Y_pred = y_pred
                                             ))
      }
    }
  }

  cat("Finished Classifiying\nWriting to CSV...\n")
  
  # From here, we have all the results. We now aim to save them.
  # Stores the findings in a csv.
  filepath <- "./outputs/1_binary_classifier_outputs"
  filename <- paste0("/",orientation_1, "_",orientation_2,"_thresh_", threshold, ".csv")

  dir.create(filepath, showWarnings = FALSE, recursive = TRUE)
  write.csv(results, paste0(filepath, filename), row.names = FALSE)

  cat(paste0("Results saved in:", filepath, filename, "\n"))


}

# Main code here
cat("\n### Started 1_binary_classifier/binary_classifer.R ###\n")

classify(features, 'left', 'left', threshold = 0.5)

cat("\n### Finished 1_binary_classifier/binary_classifer.R ###\n")




