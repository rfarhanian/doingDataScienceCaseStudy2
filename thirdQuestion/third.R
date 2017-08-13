installLibrariesOnDemand <- function (packages)
{
  cat("Installing required libraries on demand:", packages , "\n")
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  cat("Missing libraries installation is complete.", "\n")
}
installLibrariesOnDemand(c("ggplot2"))
library(ggplot2)

data("Orange")

#Calculate the mean and the median of the trunk circumferences for different size of the trees. (Tree)
cat("mean:", "\n")
means <-tapply(Orange$circumference, Orange$Tree, FUN=mean)
print(means)

cat("median:", "\n")
medians<- tapply(Orange$circumference, Orange$Tree, FUN=median)
print(medians)

#	Make a scatter plot of the trunk circumferences against the age of the tree. Use different plotting symbols for different size of trees. (Tree)

scatterPlot <- ggplot(Orange,aes(Orange$circumference, Orange$age, color= Orange$Tree, shape= Orange$Tree)) + 
  geom_point() + labs(x = "Circumference") + labs(y = "Age") + labs(shape = "Tree Size")  + labs(color = "Tree Size") +
 labs(title = "Trunk circumferences vs. age of the tree")
print(scatterPlot)

#Display the trunk circumferences on a comparative boxplot against tree. Be sure you order the boxplots in the increasing order of maximum diameter.
Orange$Tree <- factor(Orange$Tree, levels = as.integer(Orange$Tree[order(Orange$Tree)]))
box <- ggplot(Orange, aes(x = Orange$Tree, y = Orange$circumference)) + geom_boxplot() + labs(y = "Circumference") + labs(x = "Tree Size")
print(box)

