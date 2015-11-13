
===================================================================
Getting and Cleaning Data Assignment

===================================================================

The following files are included:
- 'README.md'
- 'tidyingData.R' : Script performing the analisys and generating the tidy dataset.
- 'CodeBook.md'   : Code book describing each variable and its values in the tidy data set.
- 'TidyData.txt'  : The tidy dataset.


For the purposes of this analisys, we assume:
- The data files stored in a Zip format at url https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip, 
have been downloaded and extracted to a local directory where the script will be executed.
- We have been required to work with the mean and standard deviation for each measurement. 
Under the test and training directories, other subdirectories named "Inertial Signals" exist. 
These directories contains data that has been processed in order to obtain the vectors of features provided 
in the train and test directories and don't list any mean or standard deviation variable. 
Therefore, this work ignores the data stored in the "Inertial Signals" directories.
- Because of the previous requirement, the script provided in this work, searches the names of the 561 variables 
included in the datasets for any occurrence of the words "mean" or "std" and selects the 79 variables which names 
contains these words to include them in the tidy dataset.
- Another "idSubject" and "Activity" variables has been added in order to link each vector of features 
with the subject who performed the activity and the corresponding activity label.
- The output of the script is a file named "TidyData.txt"
