# Who did what
A tracker of which group member has performed each task.

## code/

| File | Author| Description | Outputs |
|:--|:--|:--|:--|
| 0_load_data.R | Sami | When sourced, loads all the datasets into a file | NA |

## code/1_binary_classifiers

| File | Author| Description | Outputs |
| :--|:--|:--| :--|
| 0_main.R | Sami | Integrates all sub-tasks performed this week into a singular function. | NA |
| 1_binary_classifier.R | Malaika | Performs binary classification on the 'features' dataset based on a given threshold for the left-left and right-left pairs of images. | outputs/1_binary_classifier_outputs/*/data.csv |
| 2_confusion_matrix.R | Sami | Find the confusion matrix and calculates various performance measures. | outputs/1_binary_classifier_outputs/*/{classifier_performance.csv, confusion_matrix.csv} |


