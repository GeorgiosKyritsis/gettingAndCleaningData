Script(run_analysis.R) Explanation
========================================================

Reading the Test set

```r
X_test = read.table("X_test.txt")
```


Reading the six activities of test set (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING)

```r
Y_test = read.table("y_test.txt")
```


Reading the Users of test set

```r
subject_test = read.table("subject_test.txt")
```


Reading the Train set

```r
X_train = read.table("X_train.txt")
```


Reading the six activities of test set (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING)

```r
Y_train = read.table("y_train.txt")
```


Reading the Users of test set

```r
subject_train = read.table("subject_train.txt")
```


Reading the Activity Labels

```r
activity_labels = read.table("activity_labels.txt")
```


Reading the features

```r
features = read.table("features.txt")
```


Column Combine the activity with the test set

```r
comb_activity_test = cbind(Y_test, X_test)
```


Column Combine the users with the activity and the test set

```r
comb_users_activity_test = cbind(subject_test, comb_activity_test)
```


Column Combine the activity with the train set

```r
comb_activity_train = cbind(Y_train, X_train)
```


Column Combine the users with the activity and the train set

```r
comb_users_activity_train = cbind(subject_train, comb_activity_train)
```


Changing the column names of the test data frame

```r
colnames(comb_users_activity_test) <- c("Subject", "Activity_Labels", as.vector(features[, 
    2]))
```


Changing the column names of the train data frame

```r
colnames(comb_users_activity_train) <- c("Subject", "Activity_Labels", as.vector(features[, 
    2]))
```


Assigning the Activity Labels to the test data set

```r
for (i in 1:nrow(comb_users_activity_test)) {
    
    if (comb_users_activity_test[i, 2] == 1) 
        comb_users_activity_test[i, 2] = as.vector(activity_labels[1, 2]) else if (comb_users_activity_test[i, 2] == 2) 
        comb_users_activity_test[i, 2] = as.vector(activity_labels[2, 2]) else if (comb_users_activity_test[i, 2] == 3) 
        comb_users_activity_test[i, 2] = as.vector(activity_labels[3, 2]) else if (comb_users_activity_test[i, 2] == 4) 
        comb_users_activity_test[i, 2] = as.vector(activity_labels[4, 2]) else if (comb_users_activity_test[i, 2] == 5) 
        comb_users_activity_test[i, 2] = as.vector(activity_labels[5, 2]) else comb_users_activity_test[i, 2] = as.vector(activity_labels[6, 2])
}
```


Assigning the Activity Labels to the train data set

```r
for (i in 1:nrow(comb_users_activity_train)) {
    
    if (comb_users_activity_train[i, 2] == 1) 
        comb_users_activity_train[i, 2] = as.vector(activity_labels[1, 2]) else if (comb_users_activity_train[i, 2] == 2) 
        comb_users_activity_train[i, 2] = as.vector(activity_labels[2, 2]) else if (comb_users_activity_train[i, 2] == 3) 
        comb_users_activity_train[i, 2] = as.vector(activity_labels[3, 2]) else if (comb_users_activity_train[i, 2] == 4) 
        comb_users_activity_train[i, 2] = as.vector(activity_labels[4, 2]) else if (comb_users_activity_train[i, 2] == 5) 
        comb_users_activity_train[i, 2] = as.vector(activity_labels[5, 2]) else comb_users_activity_train[i, 2] = as.vector(activity_labels[6, 2])
}
```


Merging the two data sets to create one data set.

```r
total = rbind(comb_users_activity_train, comb_users_activity_test)
```


Columns with measurements on mean

```r
mean_columns = c(1, 2, 3, 41, 42, 43, 81, 82, 83, 121, 122, 123, 161, 162, 163, 
    201, 214, 227, 240, 253, 266, 267, 268, 345, 346, 347, 424, 425, 426, 503, 
    516, 529, 542)
```


Columns with measurements on std

```r
std_columns = c(4, 5, 6, 44, 45, 46, 84, 85, 86, 124, 125, 126, 164, 165, 166, 
    202, 215, 228, 241, 254, 269, 270, 271, 348, 349, 350, 427, 428, 429, 504, 
    517, 530, 543)
```


Extracting only the measurements on the mean and standard deviation for each measurement

```r
columns_mean_std = sort(c(mean_columns, std_columns)) + 2
columns_to_get = c(1, 2, columns_mean_std)
total = total[, columns_to_get]
```


Splitting the data set based on the Subject and Activity_Labels. The variable q is a list.

```r
q = split(total, list(total$Subject, total$Activity_Labels))
```


Getting the Column names

```r
a = colnames(total)
```


Subtracting the first two Column Names (Subject, Activity_Labels)

```r
a = a[c(-1, -2)]
```


Creation of a second tidy data set with the average of each variable for each activity and each subject.

```r
w = lapply(q, function(x) colMeans(x[, c(a)]))
final = data.frame(t(as.data.frame(w)))
colnames(final)[0] = "UserAndActivity"
write.table(final, file = "finalTable.txt")
```

