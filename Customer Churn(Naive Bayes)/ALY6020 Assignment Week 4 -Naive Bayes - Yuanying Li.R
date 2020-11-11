## Packages
library(tidyverse)
library(cowplot)
library(caret)
library(rpart)
library(ROCR)
library(rpart.plot)
library(dplyr)
library(magrittr)
library("e1071")
library(tidyr)


#Load the data and view it
data <- read.csv("telecom_churn.csv")
View(data)

#ggplot theme
theme <- theme(
  axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  legend.position="none" 
)

#Check Null values Number
sum(is.na(data)) 

#Prodide information about the structure of data
str(data)

#Data summary
summary(data)



#Check null data in which column
data %>%
  summarise_all(
    funs(sum(is.na(.)))) %>%
  
gather(ColumnTitle, NAs, customerID:Churn)

data %>%
  select(
    customerID, TotalCharges, TotalCharges
  ) %>%
  filter(
    is.na(TotalCharges)
  )

#Replace null value in titalcharges into 0
data[is.na(data)] <- 0

# Remove columns we didn't see correlation from above
data <- data %>%
  select(
    -customerID, -gender,-PhoneService, -MultipleLines, -MonthlyCharges, -TotalCharges 
  )

#Convert features which are belongs to int value into factor value column.
a <- sub("0","0", data$SeniorCitizen)
b <- sub("1","1",a)
data$SeniorCitizen <- b
data$SeniorCitizen <- as.factor(data$SeniorCitizen)


#Check proportion of churn
options(repr.plot.width = 4, repr.plot.height = 3)

data %>%
  group_by(Churn) %>%
  summarize(
    n = n()
  ) %>%
  mutate(
    percentage = round(n / sum(n), 3),
    n = NULL
  ) %>%
  ggplot(aes(x = Churn, y = percentage)) + geom_col(aes(fill = Churn)) +
  theme +
  geom_text(
    aes(x = Churn, y = percentage, label = paste(percentage*100, "%", sep = ""))
  )



# Decrease graph size from standard
options(repr.plot.width = 4, repr.plot.height = 4)

# Function to generate graphs for factor variables and churn

## Extract columns to be analyzed
function_columns <- data %>%
  select(
    "gender", "SeniorCitizen", "Partner", "Dependents", "PhoneService", "MultipleLines", 
    "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport",
    "StreamingTV", "StreamingMovies", "Contract", "PaperlessBilling", "PaymentMethod", "Churn"
  )

## Function, goes through each column selected
for (i in 1:ncol(function_columns))
{
  # Get column names so dplyr group by works
  cname <- colnames(function_columns[c(i,17)])
  # Subset data frame by variable name selected
  a <- subset(
    function_columns, !is.na(function_columns[,i]) & function_columns[,i] != "",
    select = cname
  ) %>%
    # Create percentage statistics per variable
    group_by_at(vars(cname)) %>%
    summarize(
      n = n()
    ) %>%
    mutate(
      Percentage = round(n / sum(n), 2)
    )
  
  # Save plot in a variable so plots can be displayed sequentialy
  p <- ggplot(
    data = a, aes_string(
      x = colnames(a[1]), y = colnames(a[4]), fill = colnames(a[1])
    )
  ) +
    # Split each graph per Churn to see influence of variable
    facet_wrap("Churn") + 
    geom_bar(stat = "identity") +
    # Make graph a bit cleaner
    theme(
      axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      axis.text.x = element_text(angle = 70, hjust = 1),
      legend.position="none"
    ) +
    geom_text(
      aes(y = Percentage, label = paste0(Percentage * 100,"%"))
    ) +
    labs(
      x = colnames(a[1]), y = "Churn", title = paste("Churn and", colnames(a[1]))
    )
  
  # Display graphs
  print(p)
  # Cleanup
  rm(cname, a, p)
}

# Decrease graph size from standard
options(repr.plot.width = 7, repr.plot.height = 3)

#Plot relationship between churn and tenure
plot_grid(
  data %>%
    filter(Churn == "Yes") %>%
    group_by(tenure) %>%
    summarize(
      n = n()
    ) %>%
    mutate(
      Percentage = round(n / sum(n), 3)
    ) %>%
    # Create plot
    ggplot(
      aes(x = tenure, y = Percentage, color = tenure)
    ) +
    stat_smooth(method = "lm", col = "red") +
    geom_point(alpha = 2/3) +
    # Clean graph visual a bit
    theme +
    labs(
      x = "Tenure", y = "Churn (%)"
    ),
  
  ggplot(
    data = data,
    aes(y = tenure, x = Churn, color = Churn)
  ) +
    theme +
    geom_boxplot()
  , align = "h")




#TRAIN, TEST & SPLIT
#Data splicing basically involves splitting the data set into training and testing data set
#set seed
n <- nrow(data)
n_train <- round(0.8*n)
n_train
set.seed(2020) 
train_indices <- sample(1:n, n_train)
data_train <- data[train_indices, ]
data_test <- data[-train_indices, ]

#Apply model
bayes <- naiveBayes(Churn~., data = data_train, laplace = 1)

#Evalusate model(confusionMatrix)
pred <- predict(bayes, data_test)
confusionMatrix(pred, data_test$Churn)

#Evalusate model
rawpred <- predict(bayes, data_test, type = "raw")
ptest <- prediction(rawpred[,2], data_test$Churn)
perf <- performance(ptest, "tpr", "fpr")
plot(perf, colorize = T)

performance(ptest, "auc")@y.values

