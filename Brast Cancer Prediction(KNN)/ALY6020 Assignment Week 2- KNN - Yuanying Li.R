#Install and load requried packages
library(class)
library(gmodels)


#Load the "wbcd" data and assign it to variable house
wbcd <- read.csv("bresastcancer_data.csv")
View(wbcd)

#Check Null values Number
sum(is.na(wbcd)) 

#Prodide information about the structure of housing dataset
#This data set contains 569 observation and 32 variables
str(wbcd)

#Drop some features that is not related to model
Drop <- names(wbcd) %in% c("id")
wbcd <- wbcd[!Drop]


#Rename the diagnosis's values
wbcd$diagnosis<- factor(wbcd$diagnosis, levels = c("B", "M"),labels = c("Benign", "Malignant"))
View(wbcd)

#Compute the proportion of Benign and Maliganant in diagnosis
table(wbcd$diagnosis)
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

#Data summary
summary(wbcd)



#Apply normalization to rescale the features to standard range of values.
#Create a normaliza() function in R
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Test the function
normalize(c(10, 20, 30, 40, 50))

#Apply the normalize() function to the numeric features in our data frame
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n)


#TRAIN, TEST & SPLIT
#Data splicing basically involves splitting the data set into training and testing data set
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]


#After deriving the training and testing data set, 
#the below code snippet is going to create a separate data frame 
#for the 'diagnosis' variable so that our 
#final outcome can be compared with the actual value.
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]



#Apply KNN classfication
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k = 21)

#Compare the true values with prediciton value using crosstable function
validation_table <- CrossTable(x = wbcd_test_labels, 
                               y = wbcd_test_pred, prop.chisq=FALSE)
validation_table

#Evaluate the classfication
accuracy=mean(wbcd_test_pred==wbcd_test_labels)
print("Accuracy:")
print(accuracy)

