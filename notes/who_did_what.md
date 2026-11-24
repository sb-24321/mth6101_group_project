# Who did what

## code/

| File | Author| Description | Outputs |
|:--|:--|:--|:--|
| 0_load_data.R | Sami | When sourced, loads all the datasets into a file | NA |

## code/1_binary_classifiers

| File | Author| Description | Outputs |
| :--|:--|:--| :--|
| 0_main.R | Sami | Integrates all sub-tasks performed this week into a singular function. | NA |
| 1_binary_classifier.R | Malaika | Performs binary classification on the 'features' dataset based on a given threshold for the left-left and right-left pairs of images. | outputs/1_binary_classifier_outputs/*/data.csv |
| 2_confusion_matrix.R | Sami | Creates the confusion matrix and calculates various performance measures. | outputs/1_binary_classifier_outputs/*/{classifier_performance.csv, confusion_matrix.csv} |

## code/2_classifier_comparison

| File | Author(s) | Description | Outputs |
| :--|:--|:--| :--|
| 0_main.R | Ela, Sami | Integrates all main subtasks performed this week into a singular function | NA |
| 1_roc_auc.R | Ela | For the left-left and right-left datasets, create the ROC curves, calculates various statistics and creates a relevant plot. | outputs/2_classifier_comparison_outputs/*|
| 2_best_threshold.R | Sami | Estimates the threshold through different methods. | Console outputs, on what is the best threshold given a method type. |

