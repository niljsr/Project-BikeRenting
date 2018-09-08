# project one

# clean the directory
rm(list=ls())

# check working directory
getwd()

# set working directory
setwd("G:/edwisor")

# importing the project dataset
data = read.csv("day.csv")

# removing the variable instant and dteday

#data = subset(data, select = -c(instant,dteday))

# checking for missing values

data_missing = sapply(data,is.na)
data_missing
# Outlier Analysis
install.packages("ggplot2")
library(ggplot2)

numeric_index = sapply(data, is.numeric)

numeric_index

data_numeric = data[,numeric_index]
cnames = colnames(data_numeric)

# to detect outliers in the data
for(i in cnames)
{
  print(i)
  val = data[,i][data[,i] %in% boxplot.stats(data[,i],coef=1.5)$out]
  #boxplot(data[season], main=i, boxwex=0.1)
}

print(val)

boxplot(data['yr'], main='year', boxwex=0.1)
boxplot(data['mnth'], main='month', boxwex=0.1)
boxplot(data['Holiday'], main='Holiday', boxwex=0.1)
boxplot(data['season'], main='Season', boxwex=0.1)
boxplot(data['weekday'], main='Weekday', boxwex=0.1)
boxplot(data['workingday'], main='Workingday', boxwex=0.1)
boxplot(data['weathersit'], main='Weathersit', boxwex=0.1)
boxplot(data['temp'], main='Temperature', boxwex=0.1)
boxplot(data['atemp'], main='Feeling Temp', boxwex=0.1)
boxplot(data['hum'], main='Humidity', boxwex=0.1)
boxplot(data['windspeed'], main='WindSpeed', boxwex=0.1)
boxplot(data['registered'], main='Registered', boxwex=0.1)
boxplot(data['cnt'], main='Count', boxwex=0.1)

#Feature Selection using corrgram

install.packages("corrgram")
library(corrgram)

corrgram(data[,numeric_index], order=F ,upper.panel = panel.pie, text.panel = panel.txt, main='correlation plot')

# attributes to be removed 
#temp,instant, season,humidity, registered,casual (4 removed out of 15 variables)

# Method 2 using caret r package to calculate correlation values

install.packages("caret")
library(caret)

# calculate the correlation matrix
corelationmatrix <- cor(data_numeric[,1:15])

# summarize the correlation matrix
print(corelationmatrix)

# find attributes taht are highly correlated (ideally >0.75)

highlycorrelated <- findCorrelation(corelationmatrix, cutoff = 0.5)

print(highlycorrelated)

# attributes to be removed 
#atemp,instant, season,humidity,registered,casual (6 removed out of 15 variables)

# Dimension Reduction

data_new = subset(data_numeric, select = -c(atemp,instant,season,hum,registered,casual))
data_new

# Normalization

# check normality
hist(data_new$temp)
hist(data_new$windspeed)
hist(data_new$holiday)
hist(data_new$cnt)
hist(data_new$weathersit)


# splitting the data into training and test data and applying regression model
# checking for multicollinearity between independent variables before splitting the data.
install.packages("usdm")
library(usdm)
vifcor(data_new[,-9], th=0.6)

# splitting the data into training and test data

smp_size <- floor(0.75 * nrow(data_new))

# set the seed so that the partition is reproducible
set.seed(123)
train_index = sample(1:nrow(data_new), size = smp_size)

train_data = data_new[train_index,]
test_data = data_new[-train_index,]

# fitting the train data into regression model
lm_model = lm(cnt ~., data = train_data)

# summary of the model

summary(lm_model)
# Adjusted r square for this is 77.25

# predicting the test data

predict_data = predict(lm_model,test_data[,1:8])


# Error metrics
# Calculate MAPE
mape <- function(y , yhat) {
   mean(abs((y- yhat)/y))*100
}
mape(test_data[,9], predict_data)

# mape is 0.1769
# Accuracy is 82.31
# KNN model for regression

# to apply KNN model for regression we will first normalise the data

cnames = c('yr','mnth','holiday','weekday','workingday','weathersit','temp','windspeed')
for(i in cnames) {
  print(i)
  data_new[,i] = (data_new[,i] - min(data_new[,i]))/(max(data_new[,i] - min(data_new[,i])))
}


# KNN model for regression
install.packages("class")
library(class)

# splitting the data into training and test data for knn

smp_size <- floor(0.8 * nrow(data_new))

# set the seed so that the partition is reproducible
set.seed(123)
train_index = sample(1:nrow(data_new), size = smp_size)

train_data = data_new[train_index,]
test_data = data_new[-train_index,]


KNN_predictions = knn(train_data[,1:9], test_data[,1:9], train_data$cnt, k=3)

KNN_predictions


# Accuracy

original_data <- test_data[,9]
KNN_predictions = as.numeric(as.character(KNN_predictions))
class(KNN_predictions)

accuracy  <- mape(original_data, KNN_predictions)
accuracy

# accuracy for KNN regressor is 94.14% with k=3
# we will freeze with k =3 and chooose KNN regressor model as the appropriate model

# sample output predicted data using R KNN model
test_data['KNN_prediction']  <- KNN_predictions

View(test_data)

write.csv(test_data, "sample_input_output.csv", row.names = F)


