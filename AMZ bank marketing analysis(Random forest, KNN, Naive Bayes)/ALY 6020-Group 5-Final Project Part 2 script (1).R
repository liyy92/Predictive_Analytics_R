# Author  : Yuanying Li, Ketaki Joshi, Na Qian, Chen Liang
# Purpose : Final Group project
# Course  : ALY 6020

#Goal-Part2: Classify the customers who will and will not Churn

getwd()
#read the data set into RStudio
df<-read.csv('/Users/daxiong/Desktop/aly6020 predictive analytics/week5/churn.csv')
#check the head of the data
head(df)

# check the detail of the dataset
summary(df)
str(df) #currently the data type of churn,usefrequency,HasCrCard are int, they will be chaged
        #to factor when building the model
# check null value
any(is.na(df)) # there is no null in this dataset
# drop the inrelevant column RowNumber and Surname
df<-df[-c(1,2)]
head(df)

# 1. heatmap to analyze the relationships between numeric variables
library(corrplot)
corr.data<-cor(df[,-2])
corrplot(corr.data, method='color')
# churn is closely related with Age, usefrequency

# Convert churn,HasCrCard,UseFrequency to factor and  0,1 to No,Yes
df[df$Churn==1,]$Churn<-'Yes'
df[df$Churn==0,]$Churn<-'No'
df$Churn<-as.factor(df$Churn)

df[df$HasCrCard==1,]$HasCrCard<-'Yes'
df[df$HasCrCard==0,]$HasCrCard<-'No'
df$HasCrCard<-as.factor(df$HasCrCard)

df[df$UseFrequency==1,]$UseFrequency<-'High'
df[df$UseFrequency==0,]$UseFrequency<-'Low'
df$UseFrequency<-as.factor(df$UseFrequency)

# 2. Barplot to show the ratio of Churn Customers
library(ggplot2)
pl<- ggplot(df,aes(x=Churn)) + geom_bar(aes(fill=Churn)) + ylab("CustomerCount")
 
print(pl+ggtitle('Count of Churn Customers and Not Churn Customers'))

# 3. explore the relationship between age and churn
#histogram
pl<- ggplot(df,aes(x=Age,fill=Churn)) + geom_histogram(bins=20)
pl+ggtitle('The Distribution of Customers Age')
# The age is right_skewed
#boxplot
pl<-ggplot(df,aes(x=Churn,y=Age))+geom_boxplot(aes(fill=Churn))
pl+ ggtitle('The Distribution of Customers Age')

# 4. explore the relationship between balance and churn
pl <- ggplot(df, aes(x = Balance, fill = Churn)) +
  geom_histogram(bins=30) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0,255000,by=30000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pl+ggtitle('The Distribution of Bank Balance')

# 5 explore the relationship between UseFrequency and Churn
pl<- ggplot(df,aes(x=UseFrequency))+geom_bar(aes(fill=Churn))
pl+ggtitle('The Relationship between Churn and Use Frequency')
#customers who use the cards less often are more likely subject to churn

#6 explore the relationship between NumOfProducts and Chrun
pl<- ggplot(df,aes(x=NumOfProducts))+geom_bar(aes(fill=Churn),position = 'dodge')
pl+ggtitle('The Relationship between Churn and Number of Products')+theme(plot.title = element_text(size = 12))

# 7. Explore the relationship between Gender and Churn
pl<- ggplot(df,aes(x=Gender))+geom_bar(aes(fill=Churn))
pl+ggtitle('The Relationship between Churn and Gender')
# Feamle are more likely subject to churn

# 8 Explore the relationship between Tenure and Churn
library(dplyr)
df %>%
group_by(Tenure, Churn) %>% 
  summarise(Number = n()) %>% 
  ggplot(aes(Tenure, Number)) +
  geom_line(aes(col = Churn)) +
  labs(x = "Tenure(years)",
       y = "Number of Customers",
       title = "Churn Based on Tenure") +
  scale_x_continuous(breaks = seq(0, 10)) +
  theme_minimal()
# Customers in different tenure groups don’t have an apparent tendency to churn or stay

# 9. Explore the relationship between CreditScore and Churn
credit_hist <- ggplot(df, aes(x = CreditScore, fill = Churn)) +
  geom_histogram(bins=30) +theme_minimal() 
credit_hist+ggtitle('The Relationship between Churn and CreditScore')
# There is a similar distribution of credit score for customers no matter they are churn or stay.

# 10. Explore the relationship between EstimatedSalary and Churn
credit_hist <- ggplot(df, aes(x = EstimatedSalary, fill = Churn)) +
  geom_histogram(bins=30) + scale_fill_manual(values=c("#999999", "#E69F00"))
credit_hist+ggtitle('The Relationship between Churn and EstimatedSalary')+theme(plot.title = element_text(size = 12))

# There is a similar distribution of estimated salary for customers no matter they are churn or stay.

# 11. Explore the relationship between HasCrCard and Churn
HasCrCard_bar<- ggplot(df,aes(x=HasCrCard,fill=Churn))+geom_bar(position = 'fill')+labs(y = "Percentage")
HasCrCard_bar+ggtitle('The Relationship between Churn and HasCrCard')
# Has a credit card or not seems to have the same proportion to churn.
##########################################################
#bulid classification Model
###########################################################
head(df)
# Delete HasCrCard column
df_new<-df[-c(7)]
str(df_new)

################################## Logistic Regression Model
#Split dataset into train and test data
library(caTools)
set.seed(123) 
sample = sample.split(df_new,SplitRatio = 0.80) 
train =subset(df_new,sample ==TRUE) 
test=subset(df_new, sample==FALSE)
dim(train)
dim(test)

# Build logistic regression model
logistic_all<- glm(Churn ~., family = binomial(link = 'logit'), data = train)

# Check the result
summary(logistic_all)

# Make prediction
predict_logistic <- predict(logistic_all, test, type = "response")
pred <- as.factor(ifelse(predict_logistic > 0.5, 1, 0))
pred

# Evaluate the model
#confusionMatrix(pred, test$Churn)
confus.matrix <- table(real=test$Churn, predict=pred)
sum(diag(confus.matrix))/sum(confus.matrix)
# The accuracy is 0.8065.

############################# Logistic all variable
library(caTools)
set.seed(123) 
sample_allfeatures= sample.split(df,SplitRatio = 0.80) 
train_allfeatures =subset(df,sample ==TRUE) 
test_allfeatures=subset(df, sample==FALSE)
dim(train_allfeatures)
dim(test_allfeatures)
str(train_allfeatures)

# Build logistic regression model
logistic_allfeatures<- glm(Churn ~., family = binomial(link = 'logit'), data = train_allfeatures)

# Check the result
summary(logistic_allfeatures)

# Make prediction
predict_logistic_allfeatures <- predict(logistic_allfeatures, test_allfeatures, type = "response")
pred_allfeatures <- as.factor(ifelse(predict_logistic_allfeatures > 0.5, 1, 0))
pred_allfeatures

# Evaluate the model
#confusionMatrix(pred, test$Churn)
confus.matrixpred_allfeatures <- table(real=test_allfeatures$Churn, predict=pred_allfeatures)
sum(diag(confus.matrixpred_allfeatures))/sum(confus.matrixpred_allfeatures)
# The accuracy is 0.803. The HasCrCard column does not effect the accuracy.

######################################## Decision Tree Model
str(df_new)

# Build model
library(rpart)
library(rattle)
churn_tree <- rpart(Churn ~., data = train, method = 'class')
fancyRpartPlot(churn_tree)

summary(churn_tree)

# Make prediction
pred <- predict(churn_tree, newdata=test, type="class")

# Evaluate the model
confus.matrix <- table(real=test$Churn, predict=pred)
sum(diag(confus.matrix))/sum(confus.matrix)
# The accuracy is 0.8483

# Prune Tree
plotcp(churn_tree)
prunetree_cart.model <- prune(churn_tree, cp = churn_tree$cptable[which.min(churn_tree$cptable[,"xerror"]),"CP"]) 

# Rebuild the model
prunetree_pred <- predict(prunetree_cart.model, newdata=test, type="class")

# Evaluate the model
confus.matrix.prune <- table(real=test$Churn, predict=prunetree_pred)
sum(diag(confus.matrix.prune))/sum(confus.matrix.prune)
# The accuracy is still 0.8455. It shows that there is no need to prune tree.

# Use K-fold Cross-Validation to avoid overfitting
library(lattice)
library(caret)
library(e1071)
train_control <- trainControl(method="cv", number=10)
train_control.model <- train(Churn~., data=train, method="rpart", trControl=train_control)
train_control.model
# The accuracy is 0.8285.

###########################random forest 
str(df)
library(randomForest)
model <- randomForest(Churn ~ ., data=df, proximity=TRUE)
model
#check if 500 trees are enough
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Yes", "No"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"Yes"], 
          model$err.rate[,"No"]))
ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

# Rebuild the model with ntree=100
model_100tree <- randomForest(Churn ~ ., data=df, proximity=TRUE,ntree=100)
model_100tree


# Compare this random forest for different values for mtry.

oob.values <- vector(length=9)
for(i in 1:9) {
  temp.model <- randomForest(Churn ~ ., data=df, mtry=i, ntree=100)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values

#find the minimum error
min(oob.values)

#find the optimal value for mtry
which(oob.values == min(oob.values))

# optimal random forest model 
model <- randomForest(Churn ~ ., data=df, proximity=TRUE,ntree=100,mtry=2)
model


########################################## knn
df_knn<-read.csv('churn.csv')
any(is.na(df_knn)) 
str(df_knn)
# drop inrelevant and categorial varialbes
df_knn<-df_knn[-c(1,2,9)]
#Convert Gender to int
df_knn$Gender<- ifelse(df_knn$Gender=="Female", 1, 0)
# the column going to be predict
Churn<-df_knn[,9]
Churn
str(df_knn)
#Normalization the data
standardized.df_knn<-scale(df_knn[,-9])
head(standardized.df_knn)
# train test split the dataset
test.index<-1:2000
test.data<-standardized.df_knn[test.index,]
test.Churn<-Churn[test.index]
train.data<-standardized.df_knn[-test.index,]
train.Churn<-Churn[-test.index]

# choose an optimal K Value(the elbow method)
library(class)
predicted.Churn<-NULL
error_rate<-NULL
for (i in 1:100) {
  set.seed(101)
  predicted.Churn <- knn(train.data,test.data,train.Churn,k=i)
  error_rate[i]<-mean(test.Churn != predicted.Churn)
}
print(error_rate)

# plot the error_rate and use the elbow method to find the best K 
k.values<-1:100
error_df<-data.frame(error_rate,k.values)
pl<-ggplot(error_df,aes(k.values,error_rate))+geom_point()+geom_line(color='blue')
pl

# build the knn model
set.seed(101)
predicted.Churn.12<-knn(train.data,test.data,train.Churn,k=12)
predicted.Churn.13<-knn(train.data,test.data,train.Churn,k=13)
#Model evaluation
error_rate.12<-mean(test.Churn != predicted.Churn.12)
print(error_rate.12)

error_rate.13<-mean(test.Churn != predicted.Churn.13)
print(error_rate.13)

#k=13 gives the lowest error rate 0.1615

#########################################Naive Bay

#Build Naive Bayes Model
# library(e1071)
model.Naive<-naiveBayes(Churn~., data=train)
model.Naive

# Predict Test Data
pre_Churn<-predict(model.Naive,test,type = 'class')
pre_result<-cbind(pre_Churn,test)
head(pre_result)

#confusion Matrix
con_mat<-table(pre_Churn,test$Churn)
con_mat
# misclassification rate
1-sum(diag(con_mat))/sum(con_mat)

# error rate 0.1715
###################################################
head(df)
##########################use the random forest to predict customer churn
CreditScore<-c(650,720,400,500,680)
Gender<-c('Female','Female','Female','Male','Male')
Age<-c(40,45,28,65,50)
Tenure<-c(3,8,5,4,2)
Balance<-c(4000.84,7934.00,0.00,2343.98,9000.87)
NumOfProducts<-c(1,1,3,2,3)
HasCrCard<-c('Yes','Yes','Yes','No','No')
UseFrequency<-c('Low','Low','High','High','High')
EstimatedSalary<-c(110000.00,120000.00,90000.00,95000.00,80000.00)
df_NewSample<-data.frame(CreditScore,Gender,Age,Tenure,Balance,NumOfProducts,HasCrCard,
                         UseFrequency,EstimatedSalary  )
df_NewSample
model <- randomForest(Churn ~ ., data=df, proximity=TRUE,ntree=100,mtry=2)
predict.CustomerChurn<-predict(model,df_NewSample)
predict.CustomerChurn

df_report<-cbind(df_NewSample,predict.CustomerChurn)
df_report


####################################################
####################################################
#Reduce the churn for customers'age between 40 and 55
df2<-read.csv('churn.csv')
library(dplyr)
df_FilterAge<-filter(df2,Age>=40 & Age<=55)
head(df_FilterAge)
summary(df_FilterAge)
df_FilterAge[df_FilterAge$Churn==1,]$Churn<-'Yes'
df_FilterAge[df_FilterAge$Churn==0,]$Churn<-'No'
df_FilterAge$Churn<-as.factor(df_FilterAge$Churn)
# bar plot shows the churn rate of customers age between 40 and 55
library(ggplot2)
pl<- ggplot(df_FilterAge,aes(x=Churn)) + geom_bar(aes(fill=Churn)) + ylab("CustomerCount")
print(pl+ggtitle('Count of Churn Customers and Not Churn 
    Customers for Age between 40 and 50'))
###########################
###########################check the effectivness of our strategy
df_str<-read.csv('churn_age.csv')
df_str_age_40_55<-filter(df_str,Age>=40 & Age<=55)
df_str_age_40_55
summary(df_str_age_40_55)
df_str_age_40_55[df_str_age_40_55$Churn==1,]$Churn<-'Yes'
df_str_age_40_55[df_str_age_40_55$Churn==0,]$Churn<-'No'
df_str_age_40_55$Churn<-as.factor(df_str_age_40_55$Churn)

pl_str<- ggplot(df_str_age_40_55,aes(x=Churn)) + geom_bar(aes(fill=Churn)) + ylab("CustomerCount")
print(pl_str+ggtitle('Count of Churn Customers and Not Churn 
   Customers for Age between 40 and 50 
                  After New Strategy'))
