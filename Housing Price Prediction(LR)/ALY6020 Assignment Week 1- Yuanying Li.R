#Install and load requried packages
library(ggplot2)
library(readr)
library(corrplot)
library(car)
library(ggthemes)
library(easyGgplot2)
library(VIM)
library(e1071)
library(caret)
library(dplyr)
library(statsr)
library(BAS)
library(corrplot)
library(GGally)
library(ggplot2)
library(MASS)
library(caTools)
library(plotly)


#Load the "housing" data and assign it to variable house
house <- read.csv("housing.csv", header = TRUE)
View(house)

#Check Null values Number
sum(is.na(house)) 

#Prodide information about the structure of housing dataset
str(house)#This data set contains 546 observation and 13 variables

#Get to know how many features are factors in this dataset
res <- sapply(house, class)
table(res)

#Convert features which are belongs to factor into a numberic value column.
a <- sub("no","0", house$driveway)
b <- sub("yes","1",a)
house$driveway <- b
house$driveway <- as.numeric(house$driveway)

c <- sub("no","0", house$recroom)
d <- sub("yes","1",c)
house$recroom <- d
house$recroom <- as.numeric(house$recroom)

e <- sub("no","0", house$fullbase)
f <- sub("yes","1",e)
house$fullbase <- f
house$fullbase <- as.numeric(house$fullbase)

g <- sub("no","0", house$gashw)
h <- sub("yes","1",g)
house$gashw <- h
house$gashw <- as.numeric(house$gashw)

i <- sub("no","0", house$airco)
j <- sub("yes","1",i)
house$airco <- j
house$airco <- as.numeric(house$airco)
str(house)

#Drop some features that is not related to model
Drop <- names(house) %in% c("prefarea")
house <- house[!Drop]
View(house)

#we would like to know something about price range and how much is the average price for a house in Ames, Iowa
#Plot histogram to see the distribution of each of each feature
hist(house$price, main = " House price", xlab = "price", ylab = "amount", col = "green")

hist(house$lotsize, main = "lotsize", xlab = "lotsize", col = "red")

hist(house$bathrms, main = "bathrms", col = "blue")

hist(house$bedrooms, main = "bedrooms", col = "yellow")

hist(house$stories, main = "stories", col = "yellow")


#Plot different Boxplot of each feature with price
par(mfrow=c(2,3))
boxplot(house$price~house$driveway,xlab="driveway (1=yes,-1=no)",ylab="housing price")
boxplot(house$price~house$recroom,xlab="recroom (1=yes,-1=no)",ylab="housing price")
boxplot(house$price~house$fullbase,xlab="fullbase (1=yes,-1=no)",ylab="housing price")
boxplot(house$price~house$gashw,xlab="gashw (1=yes,-1=no)",ylab="housing price")
boxplot(house$price~house$airco,xlab="airco (1=yes,-1=no)",ylab="housing price")


#Data summary
summary(house)


#checking the relationships of the price and the variables
plot(house$price~house$lotsize, main = "Lotsize vs Price", xlab = "Lotsize", col = "red")
plot(house$price~house$bathrms, main = "Bathrms vs Price", xlab = "Bathrms", col = "red")
plot(house$price~house$bedrooms, main = "bedrooms vs Price", xlab = "Bathrms", col = "red")

#Find correlation amongst all the features and price
corrhouse <- cor(house)
corrplot(corrhouse,type="full", method = "circle", main="Correlation")

#Find correlation coefficient which are over 0.3
corr<-cor(house)
name <- names(which(sort(abs(corr[, "price"]), decreasing = T) > 0.3))
corrplot(cor(house[,name]),title = "Correlation Plot",method="square",type="lower",addCoef.col="white",number.digits=2,tl.cex=0.9,tl.col="black",tl.srt=45,cl.lim=c(0,1))

#Choose the features of correlation coefficient what are over 0.3 to be the independent variable of the model
fit <- price ~ lotsize + bathrms + airco + stories + garagepl + bedrooms 


#TRAIN, TEST & SPLIT
split <- sample.split(house$price, SplitRatio = 0.75)
train <- subset(house, split == TRUE)
test <- subset(house, split == FALSE)
model <- lm(fit, data = train)
summary(model)


#Predict the price from test model and check the accuracy comparing with the real price. 
test$predicted.price<- predict(model,test)

pl1 <-test %>% 
  ggplot(aes(price,predicted.price)) +
  geom_point(alpha=0.5) + 
  stat_smooth(aes(colour='black')) +
  xlab('Actual value of price') +
  ylab('Predicted value of price')+
  theme_bw()
ggplotly(pl1)

error <- test$price - test$predicted.price
rmse <- sqrt(mean(error)^2)
rmse


#Q-Q norm plot, Plot of fitted and residuals 
layout(matrix(1:4,2,2))
plot(lm.base)

#Q-Q norm shows a linear pattern. However points are not well distributed on each side of zero in residuals-fitted plot, indicating the complete design model used previously is not well fitted, further effort should be made.


cooksd <- cooks.distance(lm.base)


influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
train <- train[ -influential, ]


layout(matrix(1:2,1,2))
hist(train$price)
hist(log(train$price))


fm.base <- log(price) ~ log(lotsize) + bathrms + airco + stories + garagepl + bedrooms 
lm.base.new <- lm(fm.base, train)
summary(lm.base.new)


library(randomForest)
library(caret)

#Random Forest
set.seed(223)
ctrl <- trainControl(method = "cv", number = 10, repeats = 20, verboseIter = TRUE)

lm.rf <- train(fm.base , data = train,  method = "rf",  trControl = ctrl,  tuneLength = 3)
lm.rf
  















