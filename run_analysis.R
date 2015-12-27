
# The function loads and processes either a train or a test data set
load_dataset <- function (set, features, labels) { 
  # Construct the relative paths of data files 
  prefix <- paste(set, '/', sep = '') 
  file.data <- paste(prefix, 'X_', set, '.txt', sep = '') 
  file.label <- paste(prefix, 'y_', set, '.txt', sep = '') 
  file.subject <- paste(prefix, 'subject_', set, '.txt', sep = '') 
   
  
  # Read the data as a data.table 
  data <- read.table(file.data)[, features$index] 
  names(data) <- features$name 
   
  # Apply the dataset activity labels
  DatasetLabels <- read.table(file.label)[, 1] 
  data$Activity <- factor(DatasetLabels, levels=labels$level, labels=labels$Activity) 
   
  # Apply the dataset subject labels
  DatasetSubjects <- read.table(file.subject)[, 1] 
  data$SubjectId <- factor(DatasetSubjects) 
   
  # convert to data table, and return it
  data.table(data) 
} 

# This function will perform cleansings on the column names, to make them more readable
fix_names <- function(dataset) {
  names = names(dataset)
  names = gsub("-mean", "Mean", names)
  names = gsub("-std", "Std", names)
  names = gsub("tBody", "TimeBody", names)
  names <- gsub('[()-]', '', names) # Remove the parenthesis and dashes
  names = gsub("tGravity", "TimeGravity", names)
  names = gsub("fBody", "FFTBody", names)
  setnames(dataset, names)
  dataset
}

# This function will write the dataset to the appropriate file(s)
write_tidy_dataset <- function(dataset) {
  write.table(dataset, file="tidydata.csv", row.names = FALSE)
}

# This is the main function.  Once this file is opened in R, it can be executed 
# by typing "run_analysis()".  It will load the data, perform the appropriate filtering,
# cleanse the column names, and save the tidy dataset.
run_analysis <- function() {

  # Get the features
  FeaturesAll = read.table("features.txt", col.names = c("index", "name"))
  Features = subset(FeaturesAll, grepl('-(mean|std)[(]', FeaturesAll$name))
  
  # Get the labels for the activities
  LabelsAll = read.table("activity_labels.txt", col.names = c("level", "Activity"))
  
  # Load the data sets
  TrainAll = load_dataset("train", Features, LabelsAll)
  TestAll = load_dataset("test", Features, LabelsAll)
  
  # Combine the training and test datasets into one big dirty dataset
  DirtyData = rbind(TrainAll, TestAll)
  
  # Get the average of each variable for every subject and activity
  DirtyData = DirtyData[, lapply(.SD, mean), by=list(Activity, SubjectId)]
  
  # Clean the dataset column names
  TidyData = fix_names(DirtyData)
  
  # Write the nice new shiny tidy dataset
  write_tidy_dataset(TidyData)
}
