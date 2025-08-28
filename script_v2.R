install.packages("tidyverse")
library(tidyverse)
library(dplyr)library(readr)  #读取csv
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
head(data$Time_full,30)


# The midnight hours don't have hour and minute in the original data.
# to add 00:00 to the end of time data at midnight
data <- data %>% #pipe operator
  mutate(Time_full = if_else(nchar(data$Time) <11,
                              paste0(data$Time, " 0:00"),
                              data$Time)
  )

#Convert “Time_full" into POSIXct format
#Even after mutate "Time" to "Time_full", the midnight hours are still NA.
data$Time_full <- as.POSIXct(data$Time, format = "%m/%d/%Y %H:%M")

# add date column
data <- data %>% 
  mutate(date = as.Date(data$Time_full))

# make new dataframe with date and daily mean
daily_mean <- aggregate(x=data$Stem_Radius_Fluctuation,
          by=list(data$date),
          FUN=mean)

# there is one NA at the end of the time series (Jun 2025)
daily_mean <- drop_na(daily_mean)

# change column names, doesn't matter what were the original names
colnames(daily_mean) <- c("Date", "Stem_Radius_Fluctuation")

# just to check
summary(daily_mean)
head(daily_mean)

# make new dataframe with month and monthly mean
## to avoid the issue about the leap year
monthly_mean <- aggregate(x=data$Stem_Radius_Fluctuation,
                          by=list(month=format(data$date, "%Y-%m")),
                          FUN = mean)

colnames(monthly_mean) <- c("month", "grow_mid_point")

summary(monthly_mean)
head(monthly_mean)
#---------------------------------------------
#           II. time series analysis
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
### x and y are from my_data
## lm(data ~ time(data)),col="red"), data is ts, 
### y=data, x is extracted with FUN time().

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
 
