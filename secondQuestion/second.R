installLibrariesOnDemand <- function (packages)
{
  cat("Installing required libraries on demand:", packages , "\n")
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  cat("Missing libraries installation is complete.", "\n")
}
installLibrariesOnDemand(c("tseries"))
library(tseries)

cat("Downloading Agios Pharmaceuticals stock data from 2013-07-24 to 2017-08-04", "\n")
AGIOdata <- get.hist.quote('agio',quote="Close")
cat("Agios data has ", length(AGIOdata), " rows. It corresponds to the value of the stocks from 2013-07-24 to 2017-08-04", "\n")
cat("The stock value range is from ", min(AGIOdata), " to " , max(AGIOdata), "\n")

cat("Calculating the Agios log return", "\n")
AGIOreturn <- log(lag(AGIOdata)) - log(AGIOdata)

cat("Calculating the Agios volatility", "\n")
AGIOvol <- sd(AGIOreturn) * sqrt(250) * 100
cat("Agios volatility:", AGIOvol, "\n")

Vol <- function(d, logrets)
{
  var = 0
  lam = 0
  varlist <- c()
  for (r in logrets) {
    lam = lam*(1 - 1/d) + 1
    var = (1 - 1/lam)*var + (1/lam)*r^2
    varlist <- c(varlist, var)
  }
  
  sqrt(varlist)
}

volest <- Vol(10, AGIOreturn)

volest2 <- Vol(30, AGIOreturn)

volest3 <- Vol(100, AGIOreturn)

plot(volest, type="l")

lines(volest2, type="l",col="red")

lines(volest3, type = "l", col="blue")

