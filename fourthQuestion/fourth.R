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
installLibrariesOnDemand(c("fpp"))
library(fpp)

## Reading Temp file
cat("Reading Temp file:", "\n")
readData <- function() {
  originalData= read.csv(file =  "./TEMP.csv" , header=TRUE, sep=",",fill = TRUE, quote = "\"", skipNul=TRUE, encoding = "UTF-8")
  originalData <- subset(originalData,  Monthly.AverageTemp!="")
  return (originalData) 
}

TempData<-readData()
cat("Temp file is successfully read", "\n")

#Find the difference between the maximum and the minimum monthly average temperatures for each country and 
#report/visualize top 20 countries with the maximum differences for the period since 1900.
result <- aggregate(data=TempData,TempData$Monthly.AverageTemp, by=TempData["Country"], FUN=function(x) {return(diff(range(x)))})
colnames(result)[2] <- "delta"
