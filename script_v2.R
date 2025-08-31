library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2) 

# to see what's the directory and all the files within
getwd()
list.files()
data <- read_csv("Lanzhot_oak1_v1.csv")

#---------------------------------------------
#             I. preparing data
#---------------------------------------------

# data overview
summary(data)
class(data)
head(data)

# change column names, doesn't matter what were the original names
colnames(data) <- c("Time", "Stem_Radius_Fluctuation")

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

#---------------------------------------------
#           II. time series data
#---------------------------------------------
#make time series based on daily_mean
?ts
#day.ts <- ts(daily_mean$Stem_Radius_Fluctuation,
  #                start=c(2020,1),
    #              end = c(2024,365),
     #             frequency=365)
#plot(day.ts)

#make time series based on monthly_mean
mon.ts <- ts(monthly_mean$grow_mid_point, 
                 start = c(2019,12), 
                 frequency = 12)

# top level metrics of a ts
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
#      III. AR(1) / linear model of ts and tslag1
#-----------------------------------------------------

# https://online.stat.psu.edu/stat510/Lesson01#exm-timeseriesplot
# Lesson 1.3

library(astsa)
#detrend
detrended_mon.ts <- detrend(mon.ts, 1)
plot(detrended_mon.ts)

ts <- detrended_mon.ts
lag1.plot(ts,1) #the graph shows that this linear reg. is good

acf(ts,xlim=c(1,2)) # Plots the ACF of ts for lags 1 to 2
# instead of 2, numbers including 19,10,5,3 have been tried
# 2 looks the best for the graph

tslag1=stats::lag(ts,-1) # Creates a lag 1 of x variable.
y=cbind(ts,tslag1)
ar1fit=lm(y[,1]~y[,2]) # 用y的第1列作为因变量，y的第2列作为自变量，拟线性回归
summary(ar1fit)
plot(ar1fit) # generates 4 graphs!
plot(ar1fit$fit,ar1fit$residuals)  # plot of residuals versus fits
acf(ar1fit$residuals, xlim=c(1,18)) # ACF of the residuals for lags 1 to 18
