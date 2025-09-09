library(tidyverse)
#library(dplyr)
#library(readr)
#library(ggplot2) 

# to see what's the directory and all the files within
# getwd()
# list.files()
data <- read_csv("Lanzhot_oak1_v1.csv")

#---------------------------------------------
#             I. prepare the data
#---------------------------------------------

# data overview
summary(data)
class(data)
head(data)
str(data)

# change column names, doesn't matter what were the original names
colnames(data) <- c("Time", "dendrometer")

data <- data %>%
  mutate(Stem_Radius_Fluctuation = (data$dendrometer-25.86429596)/2/pi*1000)

#Convert “Time" into POSIXct format
data$Time <- as.POSIXct(data$Time, format = "%m/%d/%Y %H:%M")

# add date column
data <- data %>% 
  mutate(date = as.Date(data$Time))

# make new dataframe with date and daily mean
daily_mean <- aggregate(x=data$Stem_Radius_Fluctuation,
          by=list(data$date),
          FUN=mean)

# just to check
summary(daily_mean)
head(daily_mean)

# there is one NA at the end of the time series (Jun 2025)
daily_mean <- drop_na(daily_mean)

# change column names
colnames(daily_mean) <- c("Date", "Stem_Radius_Fluctuation")


# make new dataframe with month and monthly mean
## to avoid the possible problem around the leap year
monthly_mean <- aggregate(x=data$Stem_Radius_Fluctuation,
                          by=list(month=format(data$date, "%Y-%m")),
                          FUN = mean)

colnames(monthly_mean) <- c("month", "grow_mid_point")

monthly_mean <- drop_na(monthly_mean)

summary(monthly_mean)
head(monthly_mean)
str(monthly_mean)


#-----------------------------------------------
#           II. visualize the time series
#-----------------------------------------------
#make time series based on daily_mean
?ts
day.ts <- ts(daily_mean$Stem_Radius_Fluctuation,
                  start=c(2020,1),
                  end = c(2024,365),
                  frequency=365)
plot(day.ts)

#make time series based on monthly_mean
mon.ts <- ts(monthly_mean$grow_mid_point, 
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
install.packages("forecast")
library(forecast)

# Example: assume x is your time series object (ts)
x <- mon.ts
x <- day.ts

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

