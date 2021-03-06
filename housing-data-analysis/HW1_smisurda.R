# Samantha Misurda
# March 21, 2017"
# HW1


# Load libraries for usage
library(ggplot2)
library(FNN)
library(plyr)
library(gplots)

########
# Problem 1A 
########


#high level briefing function that takes a data frame as an argument, and produces summary output
brief <- function(df){
  
  num.columns <- ncol(df)
  # Print the number of rows and columns
  cat(sprintf("This dataset has %s Rows and %s Attributes\n\n", nrow(df), num.columns))
  
  # Capture real values and add them to a data frame
  df.to.return <- data.frame(Attribute_ID = numeric(),
                             Attribute_Name = character(),
                             Missing = numeric(),
                             Mean = numeric(),
                             Median = numeric(), 
                             SDev = numeric(), 
                             Min = numeric(),
                             Max = numeric())
  
  cat("real valued attributes\n")
  cat("----------------------\n")
  for(col in 1:num.columns){
    if(is.numeric(df[[col]])){
      temp.missing <- sum(is.na(df[[col]]))
      temp.mean <- mean(df[[col]], na.rm=TRUE)
      temp.median <- median(df[[col]], na.rm=TRUE)
      temp.sdev <- sd(df[[col]], na.rm=TRUE)
      temp.min <- min(df[[col]], na.rm=TRUE)
      temp.max <- max(df[[col]], na.rm=TRUE)
      
      df.to.return <- rbind(df.to.return, data.frame(Attribute_ID = col,
                                                     Attribute_Name = colnames(df)[col],
                                                     Missing = temp.missing,
                                                     Mean = temp.mean,
                                                     Median = temp.median, 
                                                     SDev = temp.sdev, 
                                                     Min = temp.min,
                                                     Max = temp.max))
    }
  }
  
  print(format(df.to.return, scientific=999,digits=2))
  
  # Capture symbolic attributes and add to data frame
  df.to.return <- data.frame(Attribute_ID = numeric(),
                             Attribute_Name = character(),
                             Missing = numeric(),
                             Arity = numeric(),
                             MCVs_counts = character())
  
  
  cat("symbolic attributes\n")
  cat("----------------------\n")
  for(col in 1:num.columns){
    if(!is.numeric(df[[col]])){
      temp.missing <- sum(df[[col]]=="" | is.na(df[[col]]))
      temp.arity <- length(levels(df[[col]]))
      
      temp.mcvs <- sort(table(df[[col]]),decreasing=TRUE)[1:3]
      temp.mcvs_str <- ""
      
      for(i in 1:3){
        if(!is.na(names(temp.mcvs)[i]) & names(temp.mcvs)[i]!= "") {
          temp.str <- sprintf("%s(%d)", names(temp.mcvs)[i], temp.mcvs[i])
          temp.mcvs_str <- paste(temp.mcvs_str, temp.str)
        }
      }
      
      df.to.return <- rbind(df.to.return, data.frame(Attribute_ID = col,
                                                     Attribute_Name = colnames(df)[col],
                                                     Missing = temp.missing,
                                                     Arity = temp.arity,
                                                     MCVs_counts=temp.mcvs_str))
    }
  }
  print(format(df.to.return, scientific=999,digits=2))
}

########
# Problem 1B - Test brief function
########
housing.no.missing <- read.csv("house_no_missing.csv")
housing.missing <- read.csv("house_with_missing.csv")

print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("Brief function output for house_no_missing.csv")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
brief(housing.no.missing)

print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("Brief function output for house_with_missing.csv")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
brief(housing.missing)


##############
# Visualizations for Problem 1B 
##############
options(scipen = 999)

# Home Price (histogram)
hist(housing.no.missing$house_value, xlab = "Home Price", main = (paste("Histogram of Home Prices")))

# Charles and home prices (Contingency Table)
# Group into bins
housing.grouped <- housing.no.missing
housing.grouped$house_value <- cut(housing.no.missing$house_value, breaks = c(0, 100000, 200000, 300000, 400000, 9999999999), labels= c("Below 100,000", "100,000 -200,000", "200,001-300,000", "300,001-400,000", "400,000 and Greater"))
with(housing.grouped, table(house_value, Charles_river_bound))

# Student/Teacher Ratio and taxes (Scatterplot)
ggplot(housing.no.missing, aes(x=student_teacher_ratio, y=property_tax_rate, color=Charles_river_bound)) + geom_point() +
  ggtitle("Student/Teacher Ratio Vs. Tax Rate") +
  labs(x="Student/Teacher Ratio",y="Tax Rate") 


#####
# Problem 2A
#####

# Connect-the-dots model that learns from train set and is being tested using test set
# Assumes the last column of data is the output dimension
get_pred_dots <- function(train, test){
  nf <- ncol(train)
  input <- train[,-nf]
  query <- test[,-nf]
  my.knn <- get.knnx(input,query,k=2) # Get two nearest neighbors
  nn.index <- my.knn$nn.index
  pred <- rep(NA,nrow(test))
  for (ii in 1:nrow(test)){
    y1 <- train[nn.index[ii,1],nf]
    y2 <- train[nn.index[ii,2],nf]
    pred[ii] = (y1+y2)/2
  }
  return(pred)  
}


# Linear model
# Assumes the last column of data is the output dimension
get_pred_lr <- function(train, test){
  colnames(train)[ncol(train)] <- "output"
  linear.model <- lm(output ~ ., data=train)
  return(predict(linear.model, test))
}


# Default predictor model
# Assumes the last column of data is the output dimension
get_pred_default <- function(train, test){
  nf <- ncol(train)
  pred <- mean(train[,nf])
  #return that prediction for each input in the test set
  return(rep(pred, nrow(test)))
}

#nn: number of data points, k: number of folds
get_folds <- function(nn, k) {
  index <- seq(1, nn)
  rand.index <- sample(index, nn)
  group <- seq_along(rand.index)%%k
  chunk <- split(rand.index, group)
  return(chunk)
}

# Perform k-fold cross-validation 
do_cv <- function(df, output, k, model){
  #get the number of rows
  size <- nrow(df[output])
  #generate k folds
  folds <- get_folds(size, k)
  
  mse <- rep(NA,k)
  ii <- 1
  for(fold in folds){
    test.set <- subset(df, as.numeric(rownames(df)) %in% fold)
    train.set <- subset(df, !as.numeric(rownames(df)) %in% fold)
    
    result <- model(train.set, test.set)
    mse[ii] <- mean((test.set[output] - result)^2) 
    ii <- ii + 1
  }
  
  return(mse)
}

#test case
df <- data.frame(x=c(1,2,3,4,5,6,7,8,9,10), y=c(1.2,2.3,3.4,4.5,5.6,6.7,7.8,8.9,9.1,10.0))

mean(do_cv(df, "y", 10, get_pred_default))
mean(do_cv(df, "y", 10, get_pred_lr))
mean(do_cv(df, "y", 10, get_pred_dots))


#####
# Problem 2B
#####

# Build data frame using house_value = log(Crime Rate)
df <- housing.no.missing[,c("Crime_Rate", "house_value")]
df$Crime_Rate <- log(df$Crime_Rate)

# Run Cross-Validation using all three models, using the "mean preprocessing step"
default.housing.model <- do_cv(df, "house_value", nrow(df), get_pred_default)
linear.housing.model <- do_cv(df,"house_value", nrow(df), get_pred_lr)
dot.housing.model <- do_cv(df, "house_value", nrow(df), get_pred_dots)

models.means <- c(mean(default.housing.model), mean(linear.housing.model), mean(dot.housing.model))

models.sd <- c(sd(default.housing.model), sd(linear.housing.model), sd(dot.housing.model))

# Compute 95% CI for each score
ci.l <- models.means - (1.96 * models.sd)/sqrt(nrow(df))
ci.u <- models.means + (1.96 * models.sd)/sqrt(nrow(df))

# Make a bar chart

barplot2(models.means, plot.ci = TRUE, ci.l = ci.l, ci.u = ci.u, main = "House Values Predicted from log(Crime Rate)", ylab="MSE of Model Trained on LOO-CV", names.arg = c("Default Model", "Linear Model", "Connect the Dots Model"))




