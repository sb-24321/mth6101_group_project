#' Compiles and performs all the analysis created for this week.

# Get all the functions ==================================================================
source("code/0_load_data.R")
source("code/1_binary_classifiers/1_binary_classifier.R")
source("code/1_binary_classifiers/2_confusion_matrix.R")

# Create function task_i =================================================================
# This main function interfaces and  performs all the individual functions that are 
# implemented in the  directory `1_binary_classifiers`.

task_i <- function(
                   data = features, 
                   orientation_1, 
                   orientation_2, 
                   threshold, 
                   folder_name, 
                   should_classify = FALSE){

  #' data:            (df)  the dataset to use. Tasks are designed for `features`, 
  #'                        which is the default.
  #' orientation_1:   (str) directions to consider. Either 'right' or 'left'.
  #' orientation_2:   (str) directions to consider. Set to 'left' for the tasks.
  #' threshold:       (int) the threshold at which to classify at.
  #' folder_name:     (str) the name of the folder to get and save all the information.
  #' should_classify:(bool) whether to go through the dataset and find y_true and y_pred.
  #'                         default to false. (long process, should skip if already done).

  if (should_classify){
    classify(
             data = data,
             orientation_1 = orientation_1,
             orientation_2 = orientation_2,
             threshold = threshold,
             folder_name = folder_name
             )
  }

  confusion_matrix(
                   folder_name = folder_name 
  )
}

# Call here ==============================================================================
main <- function(){

  # LEAVE THE THRESHOLD FOR 0.5 AS THEY ARE.
  task_i(
         orientation_1 = 'right',
         orientation_2 = 'left',
         threshold = 0.5,
         folder_name = "right_left_threshold_0.5",
         should_classify = FALSE
  )

  task_i(
         orientation_1 = 'left',
         orientation_2 = 'left',
         threshold = 0.5,
         folder_name = "left_left_threshold_0.5",
         should_classify = FALSE
  )

  # Edit from here. Test various thresholds.
  
  ## Smallest MSE

  task_i(
         orientation_1 = 'left',
         orientation_2 = 'left',
         threshold = 0.366,
         folder_name = "left_left_threshold_0.366_smallest_mse",
         should_classify = TRUE 
  )

  task_i(
         orientation_1 = 'right',
         orientation_2 = 'left',
         threshold = 0.386,
         folder_name = "right_left_threshold_0.386_smallest_mse",
         should_classify = TRUE  
  )

  ## Best MCC and F1 score

  task_i(
         orientation_1 = 'left',
         orientation_2 = 'left',
         threshold = 0.315,
         folder_name = "left_left_threshold_0.315_largest_mcc_f1",
         should_classify = TRUE 
  )


  task_i(
         orientation_1 = 'right',
         orientation_2 = 'left',
         threshold = 0.304,
         folder_name = "right_left_threshold_0.304_largest_mcc_f1",
         should_classify = TRUE 
  )


  ## Youden's Index, (balanced accuracy is similar to this)
  task_i(
         orientation_1 = 'left',
         orientation_2 = 'left',
         threshold = 0.0837,
         folder_name = "left_left_threshold_0.0837_youden",
         should_classify = TRUE 
  )

  task_i(
         orientation_1 = 'right',
         orientation_2 = 'left',
         threshold = 0.111,
         folder_name = "right_left_threshold_0.111_youden",
         should_classify = TRUE
  )

}

# main()



