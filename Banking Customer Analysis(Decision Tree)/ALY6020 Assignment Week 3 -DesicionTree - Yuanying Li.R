#Install and load requried packages
library(tidyverse)  # For data cleaning, sorting, and visualization
library(DataExplorer) # For Exploratory Data Analysis
library(gridExtra) # To plot several plots in one figure
library(ggpubr) # To prepare publication-ready plots
library(GGally) 
library(rpart) 
library(caret)
library(dplyr)
library(caTools)
library(plotly)
library(rattle)
library(caret)
library(randomForest)
library(xlsx)
library(varSelRF)
library(pROC)
library(ggplot2)
library(grid)

#Load the "CC GENERAL_updated_final_version" data and assign it to cc_df
cc_df <- read.csv("CC GENERAL_updated_final_version.csv",header = TRUE)
View(cc_df)


#Prodide information about the structure of credit card data set
#This data set contains 8920 observation and 20 variables
str(cc_df)

#Check Null values Number
sum(is.na(cc_df)) 

#Remove NA
cc_df <- na.omit(cc_df) 
sum(is.na(cc_df))

#Drop the unnecessary columns of the dataframe
cc_df<-select(cc_df, -c(CUST_ID, USED_FREQUENCY, TENURE))

#Data summary
summary(cc_df)

#Active Rate
table(cc_df$RESULTS)
# Active rates in proportions
prop.table(table(cc_df$RESULTS))

#Plot a histogram 
hist(cc_df$USED_FREQUENCY , main = "used_frequency", xlab = "used_frequency", col = "red")


#TRAIN, TEST & SPLIT
#Data splicing basically involves splitting the data set into training and testing data set
n <- nrow(cc_df)
n_train <- round(0.8*n)
n_train

#set seed 
set.seed(2020) 
train_indices <- sample(1:n, n_train)
credit_train <- cc_df[train_indices, ]
View(credit_train)
credit_test <- cc_df[-train_indices, ]


#Using Decision tree Algorithm.
require(rpart)
#Building decision tree
my_tree_two <- rpart(formula = RESULTS ~.,data = credit_train, method = "class")

#graphical interface for data mining
fancyRpartPlot(my_tree_two)

#Making Predictions with decision trees
#Make predictions on the test set
my_prediction <- predict(my_tree_two, credit_test, type = "class")

#Get the accurancy of Desicion Tree
confusionMatrix(data = my_prediction, reference = credit_test$RESULTS)

#Decision Tree
#Get the best parameter for random forest
err<-as.numeric()
for(i in 1:(length(names(credit_train)))-1){
  mtry_test <- randomForest(RESULTS~., data=credit_train, mtry=i)
  err<- append( err, mean( mtry_test$err.rate ) )
}
print(err)
mtry<-which.min(err)
ntree_fit<-randomForest(RESULTS~., data=credit_train, mtry=mtry, ntree=1000)
plot(ntree_fit)

#Final Random Forest model
rf<-randomForest(RESULTS~., data=credit_train, mtry=mtry, ntree=300, importance=T )
rf

# Create an object for importance of variables
importance <- importance(rf)
# Create data frame using importance. 
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[,'IncNodePurity'], 0))
varImpPlot(rf)
# Create interactive plot.  
ggplotly(ggplot(varImportance, aes(x = reorder(Variables, Importance), 
                                   y = Importance, fill = Importance)) +
           geom_bar(stat='identity') + 
           labs(title = 'Importance of predictors', x = 'Predictors', y = 'rmsle') +
           coord_flip() + 
           theme_light())

#use p-value to evaluate model
pred1<-predict(rf,newdata=credit_test)
Freq1<-table(pred1,credit_test$RESULTS)
tp<-as.data.frame(Freq1)[4,3]
tn<-as.data.frame(Freq1)[1,3]
fn<-as.data.frame(Freq1)[2,3]
fp<-as.data.frame(Freq1)[3,3]
p<-tp/(tp+fp)
r<-tp/(tp+fn)
f<-2/(1/p+1/r)
f

