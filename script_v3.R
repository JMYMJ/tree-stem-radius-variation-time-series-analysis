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

#-----------------------------------------------
#           II. visualize the time series
#-----------------------------------------------
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
#            III. non-seasonal models
#-----------------------------------------------------
library(astsa)
#############################################

# make 1st difference to detrend the upward tendency
diff_1 <- diff(mon.ts,1)
plot(diff_1)
acf2(diff_1)
# not good

#############################################
# remove the seasonality
diff_12 <- diff(mon.ts,12)
plot(diff_12)
acf2(diff_12,24)
## PACF shuts off after lag1, ACF tapers to zero
## In Lesson4.1/Step2, it says first check for seasonality,
### if the graphs seems okay, there might be no need for 1st difference 
#### despite the obvious upward tendency in the origianl series.
## possible AR(1) model with s=12?

sarima(diff_12,1,0,0)
# notice: the xmean is the estimated mean of the series based on this model,
## not the intercept!
### the estimated model is
#### (x sub t - 14.2777) = 0.9262 (x sub t-1 - 14.2777) + w sub t
# Lesson 3.1 

sarima(diff_12,1,0,1) #not bad, acceptable


#############################################
# remove both upward trend and seasonality
diff_1_12 <- diff(diff_1,12)
plot(diff_1_12)
acf2(diff_1_12)

sarima (diff_1_12,1,0,1)
sarima (diff_1_12,1,0,0)
#even worse

#-----------------------------------------------------
#              IV. seasonal models
#-----------------------------------------------------

sarima (mon.ts, 1,0,0,1,1,0,12)


# to show more lags on x axis.
acf2(diff_12, max.lag = 48)
# maybe AR(2) s=12?!

acf2(diff_1_12, max.lag = 48) # good_plot
# not bad, resembles the colorado river example,
# then try the same model used in that example
sarima(mon.ts,1,0,0,0,1,1,12)
# not good

# twitching...
sarima(mon.ts,0,1,1,1,1,0,12) #best so far
sarima(mon.ts,0,1,1,2,1,0,12)
sarima(mon.ts,0,1,0,1,1,1,12) #makes most sense to me according to #good_plot

