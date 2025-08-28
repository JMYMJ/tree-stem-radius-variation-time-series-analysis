#载入数据处理包
library(readr)  #读取csv
library(ggplot2) #绘图

#读取csv文件
data <- read_csv("data oak1 cleaned.csv")

#optional 查看directory里面的所有文件，以防找不到data
getwd()
list.files()

#查看数据
head(data)

#将“Time”转换为 POSIXct 格式 #但是转完了每天00:00的数据就变成N/A了
data$Time <- as.POSIXct(data$Time, format = "%m/%d/%Y %H:%M")

#绘制折线图
ggplot(data, aes(x=Time, y=Tree_Stem_Variation))+
  geom_line(color="forestgreen", size = 1)+
  labs(
    title = "Tree Stem Variation Over Time",
    x="Time",
    y="Tree Stem Variation"
  )+
  theme_minimal()

#解决每天00:00的时间数据没有小时和分钟信息的问题
library(dplyr)

#检查情况
summary(data$Time)
head(data$Time,30)


data <- mutate(data,
  Time = ifelse(
  nchar(Time) == 10,
  paste0(Time, "00:00"),
  Time
  )
)
