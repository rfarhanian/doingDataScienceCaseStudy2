cat("Changing working directory.\nCurrent working directory: ", getwd(), "\n")
setwd("/Users/raminfarhanian/projects/R/caseStudy2/fourthQuestion")
cat("working directory is changed to: ", getwd(), "\n")

installLibrariesOnDemand <- function (packages)
{
  cat("Installing required libraries on demand:", packages , "\n")
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  cat("Missing libraries installation is complete.", "\n")
}
installLibrariesOnDemand(c("lubridate", "dplyr", "ggplot2"))
library(dplyr)
library(lubridate)
library(ggplot2)

## Reading Temp file
readData <- function(fileName) {
  cat("Reading ", fileName, "\n")
  originalData <- read.csv(file =  fileName , header=TRUE, sep=",",fill = TRUE, quote = "\"", skipNul=TRUE, encoding = "UTF-8")
  originalData <- subset(originalData,  Monthly.AverageTemp!="")
  originalData$Date <- parse_date_time(originalData$Date, c("%Y-%m-%d","%m/%d/%Y", "%m/%d/%Y"), exact = TRUE)
  return (originalData) 
}

TempData <- readData("./TEMP.csv")
cat("Temp file is successfully read", "\n")


monthlyAverageTemperatureDiff <- function(data, byColumn) {
  res <- aggregate(data$Monthly.AverageTemp, by=data[byColumn], FUN=function(x) {return(diff(range(x)))})
  colnames(res)[2] <- "delta"
  return (res)
}
#Find the difference between the maximum and the minimum monthly average temperatures for each country
result <- monthlyAverageTemperatureDiff(TempData, "Country")


reportDataByColumn <- function(data, byColumn) {
  result<-data[ which(data$Date >= as.Date("1900-01-01", "%Y-%m-%d")), ]
  finalResult <- aggregate(result$Monthly.AverageTemp, by=result[byColumn], FUN=function(x) {return(diff(range(x)))})
  colnames(finalResult)[2] <- "delta"
  return (finalResult)
}

#Report/visualize top 20 countries with the maximum differences for the period since 1900.
reportResult <- reportDataByColumn(TempData, "Country")

visualResult <- slice(reportResult[order(-reportResult$delta), ], 1:20)
barPlot <- ggplot(visualResult, aes(x = reorder(Country, -delta), y = delta)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="Country", y="Monthly Temperature Difference")  
print(barPlot)


#Select a subset of data called “UStemp” where US land temperatures from 01/01/1990 in Temp data.
UStemp <-TempData[ which((TempData$Date >= as.Date("1900-01-01", "%Y-%m-%d") & TempData$Country == "United States")), ]

#Create a new column to display the monthly average land temperatures in Fahrenheit (°F). T(°F) = T(°C) × 1.8 + 32
UStemp$Monthly.AverageTemp.F <- tapply(UStemp$Monthly.AverageTemp, INDEX = seq_along(UStemp$Monthly.AverageTemp), FUN = function(c) { return (c*1.8+ 32)})
print(head(UStemp))

#Calculate average land temperature by year and plot it. The original file has the average land temperature by month. 
UStemp$Monthly.AverageTemp.Uncertainty <- NULL
UStemp$Monthly.AverageTemp <- NULL
byYear <- ts(UStemp, start=c(1969,1), end = c(2013, 1), frequency=12)
plot(byYear)

#Calculate the one year difference of average land temperature by year and provide the maximum difference (value) with corresponding years.
#(for example, year 2000: add all 12 monthly averages and divide by 12 to get average temperature in 2000. You can do the same thing for all
# the available years. Then you can calculate the one year difference as 1991-1990, 1992-1991, etc) 
UStemp$Year <- format(UStemp$Date,format="%Y")
X <- aggregate(UStemp$Monthly.AverageTemp, by=UStemp["Year"], FUN=mean)
colnames(X)[2] <- "Yearly.AverageTemp"
#FINISH THIS WORK!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#Download “CityTemp” data set (check your SMU email).
cityTemp <- readData("./CityTemp.csv")
cat("CityTemp file is successfully read", "\n")

#Find the difference between the maximum and the minimum temperatures for each major city 
cityResult <- monthlyAverageTemperatureDiff(cityTemp, "City")

#report/visualize top 20 cities with maximum differences for the period since 1900
cityReportResult <- reportDataByColumn(cityTemp, "City")

cityVisualResult <- slice(cityReportResult[order(-cityReportResult$delta), ], 1:20)
cityBarPlot <- ggplot(cityVisualResult, aes(x = reorder(City, -delta), y = delta)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="City", y="Monthly Temperature Difference")  
print(cityBarPlot)


#Compare the two graphs in (i) and (iii)  and comment it.
#FINISH THIS WORK!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


