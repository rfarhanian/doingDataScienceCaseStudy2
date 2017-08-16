#+eval=FALSE

##Question 01 (10 points)
##Create the X matrix and print it from SAS, R, and Python.

X=matrix(c(4,1,2,5,1,1,1,4,0,2,5,2) , nrow=3, ncol=4)
X

##qUESTION2

install.packages("tseries")
library(tseries)

##Download AGIo Data:
agio<-get.hist.quote('agio',quote="Close")

#Below creates the log return of Agio data set:
agioreturn <- log(lag(agio))-log(agio)

# it calculate the entire volatility for the SNP500 data:
agiovolatility <- sd(agioreturn)*sqrt(250)*100

# it shows that the volatility of agio is 76%
agiovolatility

# below is a function to calculate volatility in a continous lookback window.
# it is a function of d and log returns.
# different numbers represent the volatility at particular period of time:
Volatility <- function(d, logrets) {
  var=0
  lam=0
  varlist <- c()
  for (r in logrets) {
    lam=lam*(1-1/d) + 1
    var=(1-1/lam)*var + (1/lam)*r^2
    varlist <- c(varlist, var)
  }
  sqrt(varlist)}

# below is the estimate of volatilities correspond to 3 different d:
volest <- Volatility (10,agioreturn)
volest2 <- Volatility (40,agioreturn)
volest3 <- Volatility (90,agioreturn)

# graphs shows the the estimate of agio volatility over the entire lookback window. 
# curves of results overlying eachother to show the volatility data with various d. 
plot(volest, type = "l")
lines(volest2, type="l" , col="red")
lines(volest3, type="l" , col="blue")

## question 3- orange

#a)	Calculate the mean and the median of the trunk circumferences
#for different size of the trees. (Tree)

library(doBy)
summaryBy(circumference~Tree , data=Orange, FUN=list(mean, median))

#b)	Make a scatter plot of the trunk circumferences against the age of the tree.
#Use different plotting symbols for different size of trees.

library(ggplot2)
ggplot(Orange, aes(x=age, y=circumference, colour=Tree, shape=Tree))+geom_point(size=5)

#c)	Display the trunk circumferences on a comparative boxplot against tree. 
#Be sure you order the boxplots in the increasing order of maximum diameter.

Orange$Tree <- factor(Orange$Tree, levels = as.integer(Orange$Tree[order(Orange$Tree)]))
ggplot(Orange, aes( Orange$Tree, Orange$circumference)) + geom_boxplot() + labs(y = "Circumference") + labs(x = "Tree Size")
Orange

## Question 4:

# Read Data
Temp<-read.csv("C:/Users/R900255/Desktop/Uni/Doing DS/Case Study 2/Temp.csv")

# Clean Data Table
install.packages("lubridate")
library(lubridate)
cleantable<-na.omit(Temp)
cleantable$Date <- parse_date_time(cleantable$Date, c("%Y-%m-%d","%m/%d/%Y", "%m/%d/%Y"), exact = TRUE)
#i
diffresult<-summaryBy(Monthly.AverageTemp ~ Country , data=cleantable, FUN= function(x){return(diff(range(x)))})
diffresult

Temp1900<-subset(cleantable,cleantable$Date >= as.Date("1900-01-01", "%Y-%m-%d"))
diff1900<-summaryBy(Monthly.AverageTemp ~ Country , data=Temp1900, FUN=function(x){return(diff(range(x)))})
colnames(diff1900)[2]<-"delta"
diff1900
# It reports the top 20 countries with highest Max-Min after year 1900:
diffsort1900<-diff1900[order(diff1900$delta),]
Top20<-tail(diffsort1900,20)
Top20
ggplot(Top20, aes(x = reorder(Country, -delta), y = delta)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="Country", y="Monthly Temperature Difference") 

#ii
#Subset table for year 1990 and after in United State:
UStemp<-subset(cleantable,cleantable$Date >= as.Date("1990-01-01", "%Y-%m-%d") & cleantable$Country == "United States")

#a
UStemp$F<- (UStemp$Monthly.AverageTemp*9)/5 + 32

#b
UStemp$Yeardate<-year(UStemp$Date)
Avgperyear<-summaryBy(Monthly.AverageTemp ~ Yeardate , data=UStemp, FUN=mean)
Avgperyear

#cteate line plot
lines(Avgperyear$Yeardate,Avgperyear$Monthly.AverageTemp.mean)

#c "need to add maximum row"
install.packages("data.table")
library(data.table)
DT <- data.table(Avgperyear)
DT
DT[ , list(Yeardate,Monthly.AverageTemp.mean,diffyear=diff(Monthly.AverageTemp.mean))  ]



#iv
CityTemp<-read.csv("C:/Users/R900255/Desktop/Uni/Doing DS/Case Study 2/CityTemp.csv")
cleancity<-na.omit(CityTemp)
cleancity$Date <- parse_date_time(cleancity$Date, c("%Y-%m-%d","%m/%d/%Y", "%m/%d/%Y"), exact = TRUE)

diffresultcity<-summaryBy(Monthly.AverageTemp ~ City , data=cleancity, FUN= function(x){return(diff(range(x)))})
diffresultcity

city1900<-subset(cleancity,cleancity$Date >= as.Date("1900-01-01", "%Y-%m-%d"))
diffcity1900<-summaryBy(Monthly.AverageTemp ~ City , data=city1900, FUN=function(x){return(diff(range(x)))})
colnames(diffcity1900)[2]<-"delta"
diffcity1900

diffsortcity<-diffcity1900[order(diffcity1900$delta),]
topcity20<-tail(diffsortcity,20)
topcity20
ggplot(topcity20, aes(x = reorder(City, -delta), y = delta)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="City", y="Monthly Temperature Difference")

#Question 5
convert <- function (Temp_val, Convert_to) {
  if (Convert_to != 'F' & Convert_to != 'C') {
    stop("Convert_to must be either 'F' or 'C'")
  }
  if ( Convert_to== 'F') {
    return ((Temp_val * 1.8) + 32)
  } else if (Convert_to == 'C') {
    return ((Temp_val - 32) / 1.8)
  }
}

tryCatch(expr = convert(Temp_val = 22, Convert_to = 'D') ,error= function(e) {print(paste("Expected Error: ", e))})

cat("convert(68, 'C'): ", convert(Temp_val = 68, Convert_to = 'C'), "\n")

cat("convert(20, 'F'): ", convert(Temp_val = 20, Convert_to = 'F'), "\n")

cat("convert(20, 22, 24, 26, 'F'): ", convert(Temp_val = c(20, 22, 24, 26), Convert_to = 'F'), "\n")

cat("convert(32, 68, 72, 100, 'C'): ", convert(Temp_val = c(32, 68, 72, 100), Convert_to = 'C'), "\n")




