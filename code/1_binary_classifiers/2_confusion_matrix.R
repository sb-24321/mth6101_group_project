#' Form a confusion matrix using the values from `code/1_binary_classifiers/1_binary_classifer'

cat("### Started running code/1_binary_classifier/2_confusion_matrix.R ### ")

# Get the .csv files =====================================================================

left_left <- read.csv("./outputs/1_binary_classifer_outputs/left_left_thresh_0.5.csv")

# Only interested in the last two columns, which are the true y values and the predicted 
# y values.

cat("### Finished running .code/1_binary_classifier/2_confusion_matrix.R ### ")

