# Samantha L. Misurda
# smisurda@andrew.cmu.edu
# HW 3

library(zoo)
library(plyr)
library(MASS)
library(leaps)


##############################
#prepare data for PCA analysis
##############################
mydata <- read.csv('SP500_close_price.csv')
date <- as.Date(as.character(mydata[, 1]), format="%m/%d/%Y")
myzoo <- zoo(mydata[,-1], date )
myzoo <- na.locf(myzoo) #impute missing values
prices2returns <- function(x) 100*diff(log(x)) #function to covert from price to return
log.return.zoo <- prices2returns(myzoo)
log.return.data <- coredata(log.return.zoo) #data
log.return.date <- time(log.return.zoo) #date

#####
# Problem 1.1
#####

# Perform PCA
returns.pca <- prcomp(log.return.data, retx = TRUE, center = TRUE, scale = FALSE)
#returns.pca <- princomp(log.return.data, center = TRUE, scale = FALSE)
plot(returns.pca, type = "lines", main = "Screeplot for PCA Model of Stock Returns")


#####
# Problem 1.2
#####

# Calculate the cumulative sum of variances
cumulative.sum <- cumsum(returns.pca$sdev^2/sum(returns.pca$sdev^2))

plot(cumulative.sum, xlab = "Number of Components Kept", ylab = "Percentage of Variances", main = "Cumulative Sum of Variances")


######
# Problem 1.3
#####

# Iterate over components and stop when variance is 80%
for(i in 1:length(returns.pca$sdev)) {
  total.variance <- sum(returns.pca$sdev[1:i]^2/sum(returns.pca$sdev^2))
  if(total.variance > .80) {
    cat(sprintf("A total variance of %f > .80 uses %d components", total.variance, i))
    break
  }
}

######
# Problem 1.4
#####


1 - cumulative.sum[2]


#########
#
# Problem 2
#
########
# Computes the absolute values of linear correlation coefficients between each input and the output
# Outputs sorted data frame with names of features and the corresponding coefficients
filter.features.by.cor <- function(df){
  result <- data.frame(Feature = c(), Correlation = c())
  # Loop over features, excluding output
  for(i in 1:ncol(df)-1){
    feature.name <- colnames(df)[i]
    correlation <- abs(cor(df[i], df[ncol(df)]))
    result <- rbind(result, data.frame(Feature = feature.name, Correlation = correlation))
    
  }
  # Remove row names 
  row.names(result) <- NULL
  
  # Rename columns
  colnames(result) <- c("Feature", "Correlation")
  
  # Sort rows by correlation
  result <- result[order(-result$Correlation),]
  
  return(result)
}

#######
# Problem 2, Part A
#######

bmi <- read.csv("BMI.csv")
feature.correlations <- filter.features.by.cor(bmi)

# Full correlation data
feature.correlations

# Print out top 3
feature.correlations[1:3,]

######
# Problem 2, Part B
######

feature.subsets <- regsubsets(fatpctg~., data = bmi, nbest = 1, nvmax = 3, method = "exhaustive")
summary(feature.subsets)
plot(feature.subsets, scale = "adjr2", main = "Adjusted R^2")

######
# Problem 2, Part C
######

# Build the model on all variables
model <- lm(fatpctg ~ . , data = bmi)

# Run stepwise regression
step <- stepAIC(model, direction = "backward")

# Print results
step$anova 

