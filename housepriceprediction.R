housing.df <- read.csv("kc_house_data.csv")
View(housing.df)

nomsg<-suppressMessages
nomsg(library(tidyverse))
nomsg(library(caret))
nomsg(library(Metrics))

prices <- read.csv("kc_house_data.csv")

#Reshaping data
nomsg(require(reshape2))
prices<-prices %>% 
  mutate(id=as.factor(id)) %>% 
  mutate(Date=str_replace_all(prices$date,"T0{1,}","")) %>% 
  select(Date,everything(),-date,-id)
nomsg(require(lubridate))
prices<-prices %>% 
  mutate(Date=ymd(Date)) %>% 
  separate(Date,c("Year","Month","Day"))
head(prices)

#checking for missing data
anyNA(prices)

#count vs year  
options(warn=-1)
nomsg(require(ggthemes))
prices %>% 
  ggplot(aes(Year,fill=Year))+geom_histogram(stat="count")+
  ggtitle("Count vs Year")


#price vs year
options(warn=-1)
options(scipen=999)
prices %>% 
  ggplot(aes(Year,price,fill=Year))+geom_boxplot()+
  scale_fill_manual(values=c("dodgerblue","snow"))+
  ggtitle("Price vs Year")


#sale trends in 2015
nomsg(require(ggthemes))
prices %>% 
  filter(Year==2015) %>% 
  ggplot(aes(Month,price,fill=Month))+geom_histogram(stat="identity")+
  ggtitle("Sales Trend in the Year 2015")

#sale trend in year 2014
nomsg(require(ggthemes))
prices %>% 
  filter(Year==2014) %>% 
  ggplot(aes(Month,price,fill=Month))+geom_histogram(stat="identity")+
  ggtitle("Sales Trend in the Year 2014")

#day sales in april 2015
prices %>% 
  filter(Year==2015,Month=="04") %>% 
  ggplot(aes(Day,price,fill=Day))+geom_histogram(stat="identity")+
  theme_economist()+
  ggtitle("Daily Sales in April 2015")





#Model Performance
house.df <- read.csv("kc_house_data.csv")
View(house.df)

# select variables for regression
selected.var <- c(3, 4, 5, 6, 7, 9, 12, 14, 15, 17)

# partition the data 
set.seed(12)  # set seed for reproducing the partition
numberOfRows <- nrow(house.df)
train.index <- sample(numberOfRows, numberOfRows*0.6)  
train.df <- house.df[train.index, selected.var]
summary(train.df)
summary(house.df)
valid.df <- house.df[-train.index, selected.var]
View(valid.df)
# use lm() to run a linear regression of Price on all 11 predictors in the
# use . after ~ to include all the remaining columns in train.df as predictors.
house.lm <- lm(price~ ., data = train.df)

#  use options(scipen = TRUE) to display in scientific notation
# use options(scipen = 999) to not use scientific notation.
options(scipen = 999)
summary(house.lm)

# use predict() to make predictions on a new set. 
house.lm.pred <- predict(house.lm, valid.df)
options(scipen=999, digits = 0)
some.residuals <- valid.df$price[1:20] - house.lm.pred[1:20]
data.frame("Predicted" = house.lm.pred[1:20], "Actual" = valid.df$price[1:20],
           "Residual" = some.residuals)
summary(house.lm.pred)




residuals <- valid.df$price - house.lm.pred
squaredResiduals <- residuals*residuals
df <- data.frame("Actual" = valid.df$price, "Predicted" = house.lm.pred, 
                 "Residual" = residuals, "Squared Residuals" = residuals*residuals)
rmse <- sqrt(mean(df$Squared.Residuals))
print(rmse)

library(forecast)
library(leaps)

# use accuracy() from forecast package to compute common accuracy measures.
# From help file (??accuracy) the measures calculated are:
#  ME: Mean Error
#  RMSE: Root Mean Squared Error
#  MAE: Mean Absolute Error
#  MPE: Mean Percentage Error
#  MAPE: Mean Absolute Percentage Error
#  MASE: Mean Absolute Scaled Error

options(scipen=999, digits = 3)
accuracy(house.lm.pred, valid.df$price)


house.lm.pred <- predict(house.lm, valid.df)
all.residuals <- valid.df$price - house.lm.pred
hist(all.residuals, breaks = 45, xlab = "Residuals", main = "Residual Plot")
# use regsubsets() in package leaps to run an exhaustive search. 
# unlike with lm, categorical predictors must be turned into dummies manually.
search <- regsubsets(price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")
sum <- summary(search)

# show models
sum$which

# show metrics
sum$rsq
sum$adjr2

par(mfrow=c(1,1))
plot(search, scale="r2", main = "Adjusted R Square")

house.best.lm <- lm(price ~ bedrooms + bathrooms + grade + yr_built + zipcode, data = train.df)
summary(house.best.lm)
house.best.lm.pred <- predict(house.best.lm, valid.df)
accuracy(house.best.lm.pred, valid.df$price)

#library(gains)
library(dplyr)
library(Hmisc)
library(ggplot2)

###plot predicted price vs target price for range of prices
df <- df[order(-df$Actual),] 
df$bin = as.numeric(cut2(df$Actual, g = 21))
table(df$bin)

bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted), min_Actual = min(Actual), min_Predicted = min(Predicted), max_Actual = max(Actual), max_Predicted = max(Predicted) )

##Plotting actual vs predicted values for Training and Validation data
p1<- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted Price")) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Price")) 

p1

write.csv(df, file = "df.csv")

# use step() to run backward regression.
house.lm.step <- step(house.lm, direction = "backward")
summary(house.lm.step)
house.lm.step.pred <- predict(house.lm.step, valid.df)
accuracy(house.lm.step.pred, valid.df$price)

# create model with no predictors
house.lm.null <- lm(price~1, data = train.df)

# use step() to run forward regression.
house.lm.step <- step(house.lm.null, scope=list(lower=house.lm.null, upper=house.lm), direction = "forward")
summary(house.lm.step)  
house.lm.step.pred <- predict(house.lm.step, valid.df)
accuracy(house.lm.step.pred, valid.df$price)

# use step() to run stepwise regression.
house.lm.step <- step(house.lm, direction = "both")
summary(house.lm.step)
house.lm.step.pred <- predict(house.lm.step, valid.df)
accuracy(house.lm.step.pred, valid.df$price)