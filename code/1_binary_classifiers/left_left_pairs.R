#' Compare the pairs of left-left profiles taken in different years.

# Load the required data =================================================================
source("code/0_load_data.R") # Contains the datasets
source("code/1_binary_classifiers/binary_classifier.R") # Contains `binary_classifier` 

# Get all left-sides images ==============================================================
left_features <- subset(features, features$orientation == 'left')

# TODO: get the different years and then compare them.
# NOTE: Use the function `binary_classifier(x, y, threshold)`

