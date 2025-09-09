library(tidyverse)
#library(dplyr)
#library(readr)
#library(ggplot2) 

# to see what's the directory and all the files within
getwd()
list.files()
data <- read_csv("Lanzhot_oak.CSV")

#---------------------------------------------
#             I. prepare the data
#---------------------------------------------

# data overview
summary(data)
class(data)
head(data)
str(data)

# delete all the rows with NA (luckily no NA in time)
data <- na.omit(data)

# change column names, doesn't matter what were the original names
colnames(data) <- c("Time", "oak1", "oak2", "oak3", "oak4", "oak5")

# calculate stem radius fluctuation for each tree, 
# then merge into data_all
tree_cols <- c("oak1", "oak2", "oak3", "oak4", "oak5")
data_all <- data.frame(Time = data$Time)
for (col in tree_cols){
  values <- data[[col]]
  fluct <- (values - values[1])/(2*pi)*1000
  data_all[[col]] <- fluct
}

# check if the first value for each tree is zero!
head(data_all)
summary(data_all)

#Convert “Time" into POSIXct format
data_all$Time <- as.POSIXct(data_all$Time, format = "%m/%d/%Y %H:%M")

# add date column
data_all <- data_all %>% 
  mutate(date = as.Date(data_all$Time))

# make one dataframe for each tree, 
# containing date and daily average of stem radius fluctuation 
day_oak1 <- setNames(
  aggregate(data_all$oak1,
            by=list(data_all$date),
            FUN=mean),
  c("Date", "Stem_Radius_Fluctuation")
)

head(day_oak1)

# daily mean for oak 2
day_oak2 <- setNames(
  aggregate(data_all$oak2,
            by=list(data_all$date),
            FUN=mean),
  c("Date", "Stem_Radius_Fluctuation")
)

head(day_oak2)

# daily mean for oak 3
day_oak3 <- setNames(
  aggregate(data_all$oak3,
            by=list(data_all$date),
            FUN=mean),
  c("Date", "Stem_Radius_Fluctuation")
)

head(day_oak3)


# daily mean for oak 4
day_oak4 <- setNames(
  aggregate(data_all$oak4,
            by=list(data_all$date),
            FUN=mean),
  c("Date", "Stem_Radius_Fluctuation")
)

head(day_oak4)


# daily mean for oak 5
day_oak5 <- setNames(
  aggregate(data_all$oak5,
            by=list(data_all$date),
            FUN=mean),
  c("Date", "Stem_Radius_Fluctuation")
)

head(day_oak5)

# make new dataframe with month and monthly mean
monthly_mean <- aggregate(x=data$Stem_Radius_Fluctuation,
                          by=list(month=format(data$date, "%Y-%m")),
                          FUN = mean)

colnames(monthly_mean) <- c("month", "grow")

monthly_mean <- drop_na(monthly_mean)

summary(monthly_mean)
head(monthly_mean)
str(monthly_mean)


#-----------------------------------------------
#           II. visualize the time series
#-----------------------------------------------
#make time series based on daily_mean
#?ts
#day.ts <- ts(daily_mean$Stem_Radius_Fluctuation,
  #                start=c(2020,1),
   #               end = c(2024,365),
    #              frequency=365)
#plot(day.ts)

#make time series based on monthly_mean
mon.ts <- ts(monthly_mean$grow, 
                 start = c(2019,12), 
                 frequency = 12)

# top level metrics of a ts
str(mon.ts)
class(mon.ts)
start(mon.ts)
end(mon.ts)
frequency(mon.ts)
summary(mon.ts)

# detailed metrics
plot(mon.ts)
abline(lm(mon.ts~time(mon.ts)), col="red")
## lm(y~x, data=my_data) linear reg, x explaining y, 
## lm(data ~ time(data)),col="red"), data is ts, 
## y=data, x is extracted with FUN time().

cycle(mon.ts)
# This will print the cycle across years.

plot(aggregate(mon.ts, FUN=mean)) 
# This will agrregate the cycles and display a year-on-year trend
## The year-on-year trend shows that the growth has been increasing

boxplot(mon.ts~cycle(mon.ts))
# boxplot across months will give us a sense on seasonal effect
## the plot shows that from December to May the variance is higher
### than the months from Jun to November 
#### May seems to have the highest value, but not much higher 
##### than the rest months.
 
#-----------------------------------------------------
#            III. auto arima & forecast
#-----------------------------------------------------

# Load library
# install.packages("forecast")
library(forecast)

# Example: assume x is your time series object (ts)
x <- mon.ts
# x <- day.ts

summary(x)

# Split into training and test
n <- length(x) # n = 66
n
train_size <- floor(0.8 * n) # train_size = 52
train_size

## train <- window(x, end = c(0, train_size))   # adjust indexing if needed
## test  <- window(x, start = c(0, train_size + 1))

# time(x)[30] == x[30] # FALSE!!

train <- window(x, end = time(x)[train_size])   
test  <- window(x, start = time(x)[train_size + 1])

# Fit best ARIMA/SARIMA model
fit <- auto.arima(train, seasonal = TRUE, 
                  stepwise = FALSE, approximation = FALSE)

print(fit)
summary(fit)
# Forecast horizon = length of test set
h <- length(test)
fc <- forecast(fit, h = h)

# Prediction vs actual
pred <- fc$mean

# Compute errors
errors <- data.frame(
  MAE  = mean(abs(pred - test)),
  RMSE = sqrt(mean((pred - test)^2)),
  MAPE = mean(abs((pred - test) / test)) * 100
)

print(fit)
print(errors)

# Optional: plot forecast vs actual
autoplot(fc) + autolayer(test, series = "Test")

