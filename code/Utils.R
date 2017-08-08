read_dataset <- function(directory, data_file, sep) {
  cat("Reading the dataset from", file.path(directory, data_file), "\n")
  dataset = read.csv(file = file.path(directory, data_file), header = T, sep = sep)
  return(dataset)
}

remove_attributes_with_missing_values <- function(dataset,
                                                  missing_value_token) {
  attributes_with_missing_values =
    apply(dataset, 2, function(x) length(x[x == "?"]) > 0);
  cat("Removing the following attributes as they contain missing values:",
      colnames(dataset)[attributes_with_missing_values], "\n")
  dataset = dataset[, !attributes_with_missing_values]
  return(dataset)
}

correct_numeric_attributes_with_missing_values <- function(dataset, numerical_columns,
                                                  missing_value_token) {
  numeric_dataset = dataset[, numerical_columns]
  categorical_dataset = dataset[, -numerical_columns]
  
  
  numeric_attributes_with_missing_values =
    apply(numeric_dataset, 2, function(x) length(x[x == missing_value_token]) > 0);
  
  cat("Correcting the following numeric attributes as they contain missing values:",
     colnames(numeric_dataset)[numeric_attributes_with_missing_values], "\n")
  
  list1=list()
  i = 1
  for (att in numeric_attributes_with_missing_values) {
    
    if (att) {
      m = mean(numeric_dataset[,i], na.rm="True")
      list1[i] = m
    }
    else
      list1[i] = 0
    i = i + 1
  }
  numeric_dataset = as.matrix(numeric_dataset) 
  
  i = 1
  for (m in list1) {
    if ( m != 0)
    {
      if ( i != 22)
      {
      cat("Updating Mean",m," for missing values in feature ",colnames(numeric_dataset)[i], "\n")
      row = 1;
      for (vaiabler in numeric_dataset[,i]) {
        if (is.na(vaiabler)) {
        #  cat("Row ", row, "Column", i,"\n")
       #  cat("Before Updating ", numeric_dataset[row][i],"\t")
         if (i == 5) {
           value = floor(m)
         } else {
            value = m
          }
         
          numeric_dataset[row][i] = value
      #cat("After Updating ", numeric_dataset[row][i] , "mean is ", value, "\n")
        } 
        row = row + 1;
      } 
      }
    }
    i = i + 1
  }
  
  dataset = data.frame(dataset)   
  
  
  #for (var in numeric_dataset[,5]) {
  #  if (is.na(var)) {
   #   cat("Still there")
    #}
  #}
  
  
  dataset = merge(numeric_dataset,categorical_dataset, by = "row.names")
  dataset = dataset[, -1]
  
  #dataset = dataset[, !attributes_with_missing_values]
  return(dataset)
}

remove_attributes_with_single_values <- function(dataset) {
  attributes_with_single_values =
    apply(dataset, 2, function(x) length(unique(x)) == 1)
  if (all(attributes_with_single_values)) {
    cat("Removing the following attributes as they contain only one value:",
        colnames(dataset)[attributes_with_single_values], "\n")
    dataset = dataset[, !attributes_with_single_values]
  }
 
  return(dataset)
}

convert_to_boolean_attributes <- function(dataset, att_indexes) {
  cat("Converting categorical attributes to boolean attributes\n")
  cat("Number of attributes before conversion = ", ncol(dataset) - 1, "\n")
  all_numeric_dataset = dataset[, att_indexes]
  all_boolean_dataset = acm.disjonctif(dataset[, -att_indexes])
  dataset = merge(all_boolean_dataset, all_numeric_dataset, by = "row.names")
  # Convert the categorical attributes into boolean attributes and ignore the
  # first column since it only contains row names.
  dataset = dataset[, -1]
  cat("Number of attributes after conversion = ", ncol(dataset) - 1, "\n")
  return(dataset)
}

remove_attributes_with_high_correlation <- function(dataset, threshold) {
  # Get correlation between attributes except the output class.
  correlation_matrix = cor(dataset[, 2:ncol(dataset)])
  
  # Display the correlation heat map.
  corrplot(correlation_matrix, method = "circle")
  
  # Remove attributes with high correlation.
  correlation_matrix[!lower.tri(correlation_matrix)] = 0
  attributes_with_high_correlation =
    apply(correlation_matrix, 2, function(x) any(abs(x) >= threshold))
  # Insert F in the beginning since the class was removed before computing the
  # correlation matrix.
  attributes_with_high_correlation = c(F, attributes_with_high_correlation)
  cat("Removing the following attributes as they have high correlation:",
      colnames(dataset)[attributes_with_high_correlation], "\n")
  dataset = dataset[, !attributes_with_high_correlation]
  
  return(dataset)
}

remove_attributes_with_low_output_correlation <- function(dataset, threshold) {
  # Get correlation between attributes and the output class.
  correlation_matrix = cor(dataset)
  correlation_matrix = correlation_matrix[1, 2:ncol(correlation_matrix)]
  
  # Remove attributes with low output correlation.
  attributes_with_low_output_correlation =
    sapply(correlation_matrix, function(x) any(abs(x) <= threshold))
  # Insert F in the beginning since the class was removed before computing the
  # correlation matrix.
  attributes_with_low_output_correlation =
    c(F, attributes_with_low_output_correlation)
  cat("Removing the following attributes as they have low output correlation:",
      colnames(dataset)[attributes_with_low_output_correlation], "\n")
  dataset = dataset[, !attributes_with_low_output_correlation]
  
  return(dataset)
}