

#SET AND GET WORKING DIRECTORY==================================================
# CHANGE TO THE WORKING DIRECTORY THAT YOU WANT TO
setwd("C:/Users/User/Documents/BigDataProject1/Area Level Grocery Purchases/")
getwd() # GET YOUR CURRENT WORKING DIRECTORY
#SET AND GET WORKING DIRECTORY==================================================



# LOAD LIBRARIES BEFORE STARTING================================================
library(data.table)
library(tidyverse)
library(microbenchmark)
library(ggplot2)
# LOAD LIBRARIES BEFORE STARTING================================================



# FUNCTION TO READ THE CSV FILES================================================
df <- function(i){
  library(data.table)
  library(tidyverse)
  library(microbenchmark)
  # CHANGE PATH IF FILES ARE IN DIFFERENT LOCATION
  list.files(path = "C:/Users/User/Documents/BigDataProject1/Area Level Grocery Purchases/", pattern = "*.csv") %>%
    map_df(~fread(.))
}
# FUNCTION TO READ THE CSV FILES================================================



# SEQUENTIAL PROCESSING AND MICROBENCHMARKING THE TIME TAKEN====================
mbm4 <- microbenchmark("Sequential Processing" = {lapply(1:100, df)})
mbm4

#Plot mbm4
library(ggplot2)
autoplot(mbm4)
# SEQUENTIAL PROCESSING AND MICROBENCHMARKING THE TIME TAKEN====================



# PARALLEL PROCESSING FUNCTION==================================================
parallelProcessing <- function(i){
  library(data.table)
  library(tidyverse)
  library(microbenchmark)
  library(parallel)
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, library(lme4))
  parLapply(cl, 1:100, df)
  stopCluster(cl)
}
# PARALLEL PROCESSING FUNCTION==================================================



# PARALLEL PROCESSING AND MICROBENCHMARKING THE TIME TAKEN======================
mbm5 <- microbenchmark("Parallel Processing" = parallelProcessing)
mbm5

#Plot mbm5
library(ggplot2)
autoplot(mbm5)
# PARALLEL PROCESSING AND MICROBENCHMARKING THE TIME TAKEN======================



# HYPOTHESIS TESTING CODE=======================================================
# CHANGE PATH IF FILES ARE IN DIFFERENT LOCATION
hypo_df <- read.csv("C:/Users/User/Documents/BigDataProject1/Dataset/ObjectiveDataset.csv")
View(hypo_df)

model1 <- lm(f_obese ~ energy_fat, data = hypo_df)
print(summary(model1))

model2 <- lm(f_obese ~ energy_sugar, data = hypo_df)
print(summary(model2))
# HYPOTHESIS TESTING CODE=======================================================



# Dataset Structure and Summary=================================================
str(hypo_df)
summary(hypo_df)
# Dataset Structure and Summary=================================================



# CORRELATION BETWEEN THE NUMERICAL DATAS=======================================
library(dplyr)
library(ggplot2)
library(ggpubr)
mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  cormatrix <- cor(mydataframe)
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  fm <- as.data.frame(as.table(cormatrix))
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}
model2 <- select(hypo_df, 'f_obese', 'energy_fat', 'energy_sugar')
mosthighlycorrelated(model2, 10)
# CORRELATION BETWEEN THE NUMERICAL DATAS=======================================



# CORRELATION VISUALISATION=====================================================
#Correlation between energy_fat and f_obese
library("ggpubr")
ggscatter(hypo_df, x = "energy_fat", y = "f_obese", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Energy Fat", ylab = "Prevalence of Obesity")


#Correlation between energy_sugar and f_obese
library("ggpubr")
ggscatter(hypo_df, x = "energy_sugar", y = "f_obese", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Energy Sugar", ylab = "Prevalence of Obesity")
# CORRELATION VISUALISATION=====================================================



# REGRESSION CODE===============================================================
#Regression between energy_sugar and f_obese
x <- c(hypo_df$energy_sugar)
y <- c(hypo_df$f_obese)
relation <- lm(y~x)

plot(y,x,col = "blue",main = "Prevalence of Obesity VS Energy Sugar Regression",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "energy_sugar",ylab = "f_obese")


#Regression between energy_fat and f_obese
x2 <- c(hypo_df$energy_fat)
y2 <- c(hypo_df$f_obese)
relation <- lm(y~x)

plot(y2,x2,col = "blue",main = "Prevalence of Obesity VS Energy Fat Regression",
     abline(lm(x2~y2)),cex = 1.3,pch = 16,xlab = "energy_fat",ylab = "f_obese")
# REGRESSION CODE===============================================================



# DESCRIPTIVE ANALYSIS==========================================================
# COMPUTE THE MEAN VALUE
mean1 <- mean(hypo_df$energy_fat)
print(mean1)

mean2 <- mean(hypo_df$energy_sugar)
print(mean2)

mean3 <- mean(hypo_df$f_obese)
print(mean3)


# VISUALIZATION FOR MEAN AND MEDIAN
hist(hypo_df$energy_fat)

hist(hypo_df$energy_sugar)

hist(hypo_df$f_obese)


# CALCULATING STANDARD DEVIATION
std1 <- sd(hypo_df$energy_fat)
print(std1)

std2 <- sd(hypo_df$energy_sugar)
print(std2)

std3 <- sd(hypo_df$f_obese)
print(std3)


# VISUALIZATION FOR MEAN AND STANDARD DEVIATION
standard_df <- data.frame(meanValue = c(mean(hypo_df$energy_fat), 
                        mean(hypo_df$energy_sugar), 
                        mean(hypo_df$f_obese)),
               
               stdValue = c(sd(hypo_df$energy_fat),
                            sd(hypo_df$energy_sugar),
                            sd(hypo_df$f_obese)),
               
               Category=c("energy_fat","energy_sugar","f_obese"))
               
library(ggplot2)

ggplot(standard_df, aes(x=Category, y=meanValue)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour='black') +
  geom_errorbar(aes(ymin=meanValue-stdValue, ymax=meanValue+stdValue), width=.2)


# COMPUTE THE MEDIAN VALUE
median1 <- median(hypo_df$energy_fat)
print(median1)

median2 <- median(hypo_df$energy_sugar)
print(median2)

median3 <- median(hypo_df$f_obese)
print(median3)
# DESCRIPTIVE ANALYSIS==========================================================

