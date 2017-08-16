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
installLibrariesOnDemand(c("lubridate", "dplyr", "ggplot2", "doBy"))
library(dplyr)
library(lubridate)
library(ggplot2)
library(doBy)
library(data.table)


readData <- function(fileName) {
  cat("Reading ", fileName, "\n")
  originalData <- read.csv(file =  fileName , header=TRUE, sep=",",fill = TRUE, quote = "\"", skipNul=TRUE, encoding = "UTF-8")
  originalData <- subset(originalData,  Monthly.AverageTemp!="")
  originalData$Date <- parse_date_time(originalData$Date, c("%Y-%m-%d","%m/%d/%Y", "%m/%d/%Y"), exact = TRUE)
  return (originalData) 
}

## Reading Temp file
TempData <- readData("./TEMP.csv")
cat("Temp file is successfully read", "\n")


monthlyAverageTemperatureDiff <- function(data, byColumn) {
  res <- summaryBy(Monthly.AverageTemp ~ City , data=data, FUN= function(x){ return(diff(range(x))) })
  colnames(res)[1] <- "delta"
  return (res)
}
#Find the difference between the maximum and the minimum monthly average temperatures for each country
result <- monthlyAverageTemperatureDiff(TempData, "Country")


reportDataByColumn <- function(data, byColumn) {
  result<-subset(data,data$Date >= as.Date("1900-01-01", "%Y-%m-%d"))
  finalResult <- aggregate(result$Monthly.AverageTemp, by=result[byColumn], FUN=function(x) {return(diff(range(x)))})
  colnames(finalResult)[2] <- "delta"
  return (finalResult)
}

firstTwentyOrderedByDelta <- function(data) { 
  return (slice(data[order(-data$delta), ], 1:20))
}

#Report/visualize top 20 countries with the maximum differences for the period since 1900.
reportResult <- reportDataByColumn(TempData, "Country")

visualResult <- firstTwentyOrderedByDelta(data = reportResult)
barPlot <- ggplot(visualResult, aes(x = reorder(Country, -delta), y = delta)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="Country", y="Monthly Temperature Difference")  
print(barPlot)


#ii) Select a subset of data called “UStemp” where US land temperatures from 01/01/1990 in Temp data.
UStemp <-TempData[ which((TempData$Date >= as.Date("1990-01-01", "%Y-%m-%d") & TempData$Country == "United States")), ]

#a) Create a new column to display the monthly average land temperatures in Fahrenheit (°F). T(°F) = T(°C) × 1.8 + 32
UStemp$Monthly.AverageTemp.F <- UStemp$Monthly.AverageTemp*1.8 + 32
print(head(UStemp))

#Calculate average land temperature by year and plot it. The original file has the average land temperature by month. 
UStemp$Yeardate<-year(UStemp$Date)
Avgperyear<-summaryBy(Monthly.AverageTemp ~ Yeardate , data=UStemp, FUN=mean)
print(head(Avgperyear))

#create line plot
#lines(Avgperyear$Yeardate,Avgperyear$Monthly.AverageTemp.mean)
#We need to make the above line work!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#Calculate the one year difference of average land temperature by year and provide the maximum difference (value) with corresponding years.
#(for example, year 2000: add all 12 monthly averages and divide by 12 to get average temperature in 2000. You can do the same thing for all
# the available years. Then you can calculate the one year difference as 1991-1990, 1992-1991, etc) 
DT <- data.table(Avgperyear)
DT
DT[ , list(Yeardate, Monthly.AverageTemp.mean, diffyear=diff(Monthly.AverageTemp.mean))  ]


#iv)Download “CityTemp” data set (check your SMU email).
cityTemp <- readData("./CityTemp.csv")
cat("CityTemp file is successfully read", "\n")

#Find the difference between the maximum and the minimum temperatures for each major city 
cityResult <- monthlyAverageTemperatureDiff(cityTemp, "City")

#report/visualize top 20 cities with maximum differences for the period since 1900
cityReportResult <- reportDataByColumn(cityTemp, "City")

cityVisualResult <- firstTwentyOrderedByDelta(data = cityReportResult)
cityBarPlot <- ggplot(cityVisualResult, aes(x = reorder(City, -delta), y = delta)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="City", y="Monthly Temperature Difference")  
print(cityBarPlot)


#Compare the two graphs in (i) and (iii)  and comment it.
These two graphs illustrate that the countries who had cities with most temperature changes were not in the top 20 list. It indicates
that the drastic temperature change in one city was not followed by other cities of the same country since 1900 to 2013. However, this
trend can be a subset of longer trend. That is why we cannot extend our observation to other time intervals.



