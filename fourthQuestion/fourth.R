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
readData <- function() {
  cat("Reading Temp file:", "\n")
  originalData= read.csv(file =  "./TEMP.csv" , header=TRUE, sep=",",fill = TRUE, quote = "\"", skipNul=TRUE, encoding = "UTF-8")
  originalData <- subset(originalData,  Monthly.AverageTemp!="")
  originalData$Date <- parse_date_time(originalData$Date, c("%Y-%m-%d","%m/%d/%Y", "%m/%d/%Y"), exact = TRUE)
  return (originalData) 
}

TempData<-readData()
cat("Temp file is successfully read", "\n")

#Find the difference between the maximum and the minimum monthly average temperatures for each country
result <- aggregate(TempData$Monthly.AverageTemp, by=TempData["Country"], FUN=function(x) {return(diff(range(x)))})
colnames(result)[2] <- "delta"


#Report/visualize top 20 countries with the maximum differences for the period since 1900.
reportData<-TempData[ which(TempData$Date >= as.Date("1900-01-01", "%Y-%m-%d")), ]
reportResult <- aggregate(reportData$Monthly.AverageTemp, by=reportData["Country"], FUN=function(x) {return(diff(range(x)))})

colnames(reportResult)[2] <- "delta"
visualResult<-slice(reportResult[order(-reportResult$delta), ], 1:20)
barPlot <- ggplot(visualResult, aes(x = reorder(Country, -delta), y = delta)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="Country", y="Monthly Temperature Difference")  
print(barPlot)


#Select a subset of data called “UStemp” where US land temperatures from 01/01/1990 in Temp data.
UStemp <-TempData[ which((TempData$Date >= as.Date("1900-01-01", "%Y-%m-%d") & TempData$Country == "United States")), ]

#Create a new column to display the monthly average land temperatures in Fahrenheit (°F). T(°F) = T(°C) × 1.8 + 32
UStemp$Monthly.AverageTemp.F <- tapply(UStemp$Monthly.AverageTemp, INDEX = seq_along(UStemp$Monthly.AverageTemp), FUN = function(c) { return (c*1.8+ 32)})
print(head(UStemp))


