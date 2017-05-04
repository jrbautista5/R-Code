#Install packages and attach appropriate libraries:
#install.packages('gam')
#install.packages('randomForest')
#install.packages('gbm')
#install.packages('mgcv')
#install.packages('ggplot2')
#install.packages('caret', dependencies = TRUE)#Needed for confusion matrix

library(gam)
library(randomForest)
library(gbm)
library(caret)
library(randomForest)
library(mgcv)
library(ggplot2)
library(e1071)

#set.seed(666)


#==============================================================
#========Cleaning Data Etc========================================
#==============================================================

# Import the credit dataframe:
credit <- read.csv('/Users/Ihsorak/OneDrive/RCode/crx.data.txt')
