source('Utils.R')

### Install and load required packages ###
required_packages = c("ade4", # for converting categorical to bool attributes
                      "corrplot", # for computing and plotting correlations
                      "rpart", # for decision trees
                      "neuralnet", # for neural net
                      "e1071", # for SVM and Naive Bayes
                      "AUC", # for computing auc(roc)
                      "caret", # for createFolds
                      "class", # for knn
                      "adabag", # for bagging and adaboost
                      "randomForest") # for random forest
missing_packages =
  required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) {
  install.packages(missing_packages)
}
for (required_package in required_packages) {
  require(required_package, character.only = T)
  library(required_package, character.only = T)
}

set.seed(1)

### Read the dataset ###
dataset = read_dataset('dataset', 'train.csv', ',')
dataset = dataset[,-1]

numerical_columns = c(3,4,19,20,26,34,36,37,38,43,44,45,46,47,48,49,50,51,52,54,56,59,61,62,66,67,68,69,70,71,75,76,77,80)

cat("Total Number of Attributes (Including Output) :: ", length(colnames(dataset)), "\n")

cat("Number of Categorical Columns :: ", (length(colnames(dataset)) - length(numerical_columns)), "\n")

cat("Categorical columns are :: ", colnames(dataset[,-numerical_columns]), "\n")

cat("Number of Numerical Columns (Including Output) :: ", (length(numerical_columns)), "\n")

cat("Categorical columns are :: ", colnames(dataset[,numerical_columns]), "\n")


length_before_removing_attr_with_single_values = length(colnames(dataset))
cat("Checking and Removing Attributes with Single Values","\n")
dataset = remove_attributes_with_single_values(dataset)
if (length(colnames(dataset)) == length_before_removing_attr_with_single_values) {
  cat("No Attribute contains Single Values")
} else {
  cat("Total Number of Attributes (Including Output) After Removing Attributes with Single Values:: ",
      length(colnames(dataset)), "\n")
}

dataset = correct_numeric_attributes_with_missing_values(dataset, numerical_columns, "NA")

boolean_dataset = convert_to_boolean_attributes(dataset, c(1:34))
#for (numeric_attribute in colnames(dataset[, -categorical_columns])) {
#  plot(density(dataset[, numeric_attribute]), main = numeric_attribute)
#}



