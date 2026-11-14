#' Form a confusion matrix using the values from `code/1_binary_classifiers/1_binary_classifer'

# Imports ================================================================================
library(dplyr)

# Creates the confusion matrix ===========================================================
confusion_matrix <- function(data, filename){

  # Extract the Y true values and the Y predicted values.
  data <- data%>%
    dplyr::transmute(Y_true, Y_pred)
  
  # Creates the confusion matrix.
  cat("Creating confusion matrix...\n")
  confusion_mat <- table(
    Actual = factor(data$Y_true, levels = c(1, 0)),
    Predicted = factor(data$Y_pred, levels = c(1, 0))
  )

  cat("Saving confusion matrix...\n")
  
  # Save the results to a CSV file
  filepath <- "./outputs/1_binary_classifier_outputs"
  filename <- paste0("/", filename, ".csv")
  dir.create(filepath, showWarnings = FALSE, recursive = TRUE)
  write.csv(confusion_mat, paste0(filepath, filename), row.names = FALSE)

  cat(paste0("Results saved in: ", filepath, filename, "\n"))

}

# TODO: Find the other statistics, like rates.

# Main ==================================================================================
cat("\n### Started running code/1_binary_classifier/2_confusion_matrix.R ###\n")

left_left <- read.csv("outputs/1_binary_classifier_outputs/left_left_thresh_0.5.csv")
confusion_matrix(left_left, "left_left_thres_0.5_confusion_matrix")

right_left <- read.csv("outputs/1_binary_classifier_outputs/right_left_thresh_0.5.csv")
confusion_matrix(right_left, "right_left_thres_0.5_confusion_matrix")

cat("### Finished running .code/1_binary_classifier/2_confusion_matrix.R ###\n")

