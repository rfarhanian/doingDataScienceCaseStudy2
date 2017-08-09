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

cat("groups:",levels(Orange$Tree), "\n")
means <-tapply(Orange$circumference, Orange$Tree, FUN=mean)
cat("means:", means, "\n")

medians <-tapply(Orange$circumference, Orange$Tree, FUN=median)
cat("medians:", medians, "\n")


#	Make a scatter plot of the trunk circumferences against the age of the tree. Use different plotting symbols for different size of trees. (Tree)

scatterPlot <- ggplot(Orange,aes(Orange$circumference, Orange$age, color= Orange$Tree, shape= Orange$Tree)) + 
  geom_point() + labs(x = "Circumference") + labs(y = "Age") + labs(shape = "Tree Group")  + labs(color = "Tree Group") +
 labs(title = "Trunk circumferences vs. age of the tree")

print(scatterPlot)

#Display the trunk circumferences on a comparative boxplot against tree. Be sure you order the boxplots in the increasing order of maximum diameter.
box <- ggplot(Orange[Orange$circumference,], aes( Orange$Tree, Orange$circumference)) + geom_boxplot() + labs(y = "Circumference") + labs(x = "Tree Group")

print(box)

