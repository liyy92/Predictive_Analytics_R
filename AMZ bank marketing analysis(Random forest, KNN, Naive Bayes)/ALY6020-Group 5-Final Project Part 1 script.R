# Author  : Yuanying Li, Ketaki Joshi, Na Qian, Chen Liang
# Purpose : Final Group project
# Course  : ALY 6020

#Goal-Part 1: To identify which customers will accept the offer and activate a credit card and who will decline the offer. 

library(caret) # models
library(corrplot) # correlation plots
library(DALEX) # explain models
library(DescTools) # plots
library(doParallel) # parallel processing
library(dplyr) # syntax 
library(ggplot2) # plots
library(inspectdf) # data overview
library(readr) # quick load
library(sjPlot) # contingency tables
library(tidyverse) 
library(funModeling)
library(cowplot)
library(rpart)
library(ROCR)
library(rpart.plot)
library(dplyr)
library(e1071)
library(tidyr)
library(class)
library(gmodels)
library(randomForest)
library(naivebayes)
library(rattle)
library(ROSE)

#Load the data and view it
df <- read_csv("D:/NEU/Quarter 3/Predictive Analytics/Week 5/Final/creditcardmarketing-bbm_final.csv")

#View of the data set 
head(df)

#Setting ggplot theme for visually appealing graphs 
theme <- theme(
  axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  legend.position="none" 
)

#Checking missing values
sum(is.na(df))

#Getting a brief about missing values 
colSums(is.na(df))

df %>%
  select(
  CustomerNumber, AverageBalance, Q1Balance,Q2Balance,Q3Balance,Q4Balance ) %>% 
filter(is.na(AverageBalance)
 )

#Replace null values in titalcharges into mean of averagebalance
df[is.na(df)] <- 940.52  

colnames(df)

#We have average balance and all four quarter balance(Q1, Q2, Q3, Q4) as variables. 
#So, we dropped these four variables and used only Average Balance variable. 

# Dropping unwanted columns
Drop <- names(df) %in% c("CustomerNumber", "Q1Balance","Q2Balance","Q3Balance","Q4Balance")
df <- df[!Drop]
View(df)

#Data summary
summary(df)

#Checking the structure of the dataset 
str(df)

#Checking number of customers who accepted the offer to activate credit card and who did not
prop.table(table(df$ActiveCreditCard))
table(df$ActiveCreditCard)

#We can see this data set contains only 5.68%(rounded) positive cases. 
#This means only 5.68% accepted the offer and activated the credit card. Rest 94.31% of cases are negative.
#This is a severely imbalanced data set and will affect the model's prediction accuracy. 
#Therefore, it is necessary to balanced data before applying a machine learning algorithm.

#We can use sampling method-undersampling and oversampling
#If we use only undersampling method, we can lost significant information from the sample. 
#Hence, we used 'both' methods to balance the data set. In this case, the minority class is oversampled with replacement and majority class is undersampled without replacement.

#Balancing the classes 
df_new<- ovun.sample(ActiveCreditCard ~ ., data = df, method = "both", p=0.5, seed = 2020)

#Merging into original data set 
df <- df_new$data

#Checking the values again 
table(df$ActiveCreditCard)
#Now we can see that the data set is balanced. 

#Explore categorical variables
x <- inspect_cat(df)
show_plot(x)

#Get more detail for frequency/count of each level of each categorical variables
freq(df)

#Descriptive analysis of numeric variables
profiling_num(df)

#Bivariate Analysis 
#Cross-plot shows relationship between each variable and response variable
cross_plot(data = df, target = 'ActiveCreditCard')

#histogram of numeric variable
plot_num(df)

#Converting response variable data type into factor
df$ActiveCreditCard <- as.factor(df$ActiveCreditCard)

#Checking relationship between job, age, and ActiveCreditCard
p <- ggplot(df, aes(x=Job, y=Age, fill=ActiveCreditCard), title = "Job and Age relationship ")  
p+geom_boxplot()

#TRAIN, TEST & SPLIT
#Data splicing basically involves splitting the data set into training and testing data set
#set seed
set.seed(2020)
train_indices <- sample(1:nrow(df), 0.75*nrow(df))
df_train <- df[train_indices, ]
df_test <- df[-train_indices, ]

#Checking the number of observations in train and test sets 
NROW(df_train)
NROW(df_test)

#Decision Tree
model_tree <- rpart(ActiveCreditCard ~., data = df_train, method = 'class')
summary(model_tree)
fancyRpartPlot(model_tree,main = 'Decision Tree')

#Examine the complexity plot
printcp(model_tree)
plotcp(model_tree)

#Confirming optimal CP value
cp= model_tree$cptable[which.min(model_tree$cptable[,"xerror"]),"CP"]
cp

#Naive Bayes
model_naive <- naive_bayes(ActiveCreditCard ~., data = df_train, usekernel = T)
model_naive

df_train %>%
  filter(ActiveCreditCard == 'Yes')%>%
  summarise(mean(Tenure),sd(Tenure))

#Random Forest
model_rf1 <- randomForest(ActiveCreditCard~., data = df_train, importance = TRUE)
model_rf1
plot(model_rf1, main = 'Model with default parameters')

#Using loop to identify the optimal mtry value for model 
set.seed(123)
acc_check = c()
i= 5
for (i in 3:8) {
  ran_model<- randomForest(ActiveCreditCard ~ ., data = df_train, ntree = 500, mtry = i, importance = TRUE)
  ran_pred <- predict(ran_model, df_test, type = "class")
  acc_check[i-2] <- mean(ran_pred == df_test$ActiveCreditCard)
} 

#Checking accuracy
100* acc_check

#Plotting mtry values
plot(3:8,acc_check,xlab = 'mtry values',ylab = 'accuracy', main = 'mtry value plot')
#At mtry = 4, the model achieved the highest accuracy of 98.88% (rounded).  

#Tuning model with parameters
model_rf2 <- randomForest(ActiveCreditCard~., data = df_train, ntree = 500, mtry = 4,importance = TRUE)
model_rf2
plot(model_rf2, main = 'Model with tuned parameters')

#creating a data frame for putting every model's accuracy together 
model<-c()
accuracy<-c()

#Model evaluation 

#Decision Tree
pred_tree <- predict(model_tree, df_test, type = 'class')
conf_tree <- confusionMatrix(as.factor(pred_tree),as.factor(df_test$ActiveCreditCard))
conf_tree
accuracy<-append(accuracy,conf_tree$overall[1])
model<-append(model,'DecisionTree')

#Naive Bayes
pred_naive <- predict(model_naive, df_test)
conf_naive <- confusionMatrix(as.factor(pred_naive),as.factor(df_test$ActiveCreditCard))
conf_naive
accuracy<-append(accuracy,conf_naive$overall[1])
model<-append(model,'NaiveBayes')

#Random Forest
pred_rf <- predict(model_rf2, df_test, type = 'class')
conf_rf <- confusionMatrix(as.factor(pred_rf),as.factor(df_test$ActiveCreditCard))
conf_rf
accuracy<-append(accuracy,conf_rf$overall[1])
model<-append(model,'RandomForest')

#Accuracy comparison of all models

#Choosing  the best model
Accuracy_table <- data.frame(model,accuracy)
Accuracy_table

#Model comparison 
p <-ggplot(Accuracy_table, aes(model,accuracy,fill=model))
p +geom_bar(stat="identity",width = 0.2)+coord_flip()

#Predict who will or will not accept the offer 
head(cbind(pred_rf,df_test)) 
summary(pred_rf)
