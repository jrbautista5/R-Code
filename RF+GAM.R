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

# Rename the columns of the 'credit' dataframe:
colnames(credit) <- c('Male', 'Age', 'Debt', 'Married', 'BankCustomer',
                      'EducationLevel', 'Ethnicity', 'YearsEmployed',
                      'PriorDefault', 'Employed', 'CreditScore',
                      'DriversLicense', 'Citizen', 'ZipCode',
                      'Income', 'Approve')
#---------------------------------------------------------------------------

# Clean data and get rid of '?' and replace with NA values:
levels(credit$Approve) <- c(0, 1)
levels(credit$Male) <- c(NA, 0, 1)
credit$Age <- as.numeric(as.character(credit$Age))
credit$Married <- replace(credit$Married, credit$Married == '?', NA)
credit$BankCustomer <- replace(credit$BankCustomer, credit$BankCustomer == '?', NA)
credit$EducationLevel <- replace(credit$EducationLevel, credit$EducationLevel == '?', NA)
credit$Ethnicity <- replace(credit$Ethnicity, credit$Ethnicity == '?', NA)
credit$ZipCode <- replace(credit$ZipCode, credit$ZipCode == '?', NA)
#---------------------------------------------------------------------------

#Create a multiple linear regression model to predict missing Age values.
age.predictor <- lm(Age ~ Debt + YearsEmployed + Married, data = credit)

predicted_age <- predict(age.predictor, data = credit)

credit$Age[is.na(credit$Age)] = predicted_age[is.na(credit$Age)]

#-----------------------------------------------------------------------
#Store credit in credit.PDC
credit.PDC <- credit


#Fill missing categorical variables with mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
credit.PDC$Male[is.na(credit.PDC$Male)] = Mode(credit.PDC$Male)
credit.PDC$EducationLevel[is.na(credit.PDC$EducationLevel)] = Mode(credit.PDC$EducationLevel)
credit.PDC$BankCustomer[is.na(credit.PDC$BankCustomer)] = Mode(credit.PDC$BankCustomer)
credit.PDC$Employed[is.na(credit.PDC$Employed)] = Mode(credit.PDC$Employed)
credit.PDC$DriversLicense[is.na(credit.PDC$DriversLicense)] = Mode(credit.PDC$DriversLicense)
credit.PDC$Married[is.na(credit.PDC$Married)] = Mode(credit.PDC$Married)
credit.PDC$Ethnicity[is.na(credit.PDC$Ethnicity)] = Mode(credit.PDC$Ethnicity)
credit.PDC$Citizen[is.na(credit.PDC$Citizen)] = Mode(credit.PDC$Citizen)
credit.PDC$ZipCode[is.na(credit.PDC$ZipCode)] = Mode(credit.PDC$ZipCode)

#====================================================================================

#Change number factors to actual numerics
credit.PDC$Approve <- as.numeric(as.character(credit.PDC$Approve))
credit.PDC$CreditScore <- as.numeric(credit.PDC$CreditScore)
credit.PDC$Income <- as.numeric(credit.PDC$Income)
credit.PDC$Debt <- as.numeric(credit.PDC$Debt)

#Change categorical factors to numerics
levels(credit.PDC$PriorDefault) <- c(0,1)
credit.PDC$PriorDefault <- as.numeric(as.character(credit.PDC$PriorDefault))

levels(credit.PDC$Male) <- c(0,1)
credit.PDC$Male <- as.numeric(as.character(credit.PDC$Male))

levels(credit.PDC$EducationLevel) <- c(seq(0, nlevels(credit.PDC$EducationLevel), 1))
credit.PDC$EducationLevel <- as.numeric(as.character(credit.PDC$EducationLevel))

levels(credit.PDC$Married) <- c(seq(0, nlevels(credit.PDC$Married), 1))
credit.PDC$Married <- as.numeric(as.character(credit.PDC$Married))

levels(credit.PDC$BankCustomer) <- c(seq(0, nlevels(credit.PDC$BankCustomer), 1))
credit.PDC$BankCustomer <- as.numeric(as.character(credit.PDC$BankCustomer))

levels(credit.PDC$Ethnicity) <- c(seq(0, nlevels(credit.PDC$Ethnicity), 1))
credit.PDC$Ethnicity <- as.numeric(as.character(credit.PDC$Ethnicity))

levels(credit.PDC$Employed) <- c(seq(0, nlevels(credit.PDC$Employed), 1))
credit.PDC$Employed <- as.numeric(as.character(credit.PDC$Employed))

levels(credit.PDC$DriversLicense) <- c(seq(0, nlevels(credit.PDC$DriversLicense), 1))
credit.PDC$DriversLicense <- as.numeric(as.character(credit.PDC$DriversLicense))

levels(credit.PDC$Citizen) <- c(seq(0, nlevels(credit.PDC$Citizen), 1))
credit.PDC$Citizen <- as.numeric(as.character(credit.PDC$Citizen))

levels(credit.PDC$ZipCode) <- c(seq(0, nlevels(credit.PDC$ZipCode), 1))
credit.PDC$ZipCode <- as.numeric(as.character(credit.PDC$ZipCode))


#====================================================================================

#Check correlation
correlations <- cor(credit.PDC[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)])
#View(correlations)

#====================================================================================

#Build sample set
smp_size = floor(0.5 * nrow(credit.PDC))
train_ind = sample(seq_len(nrow(credit.PDC)), size = smp_size)
#Specify testing and training portions of code
train = credit.PDC[train_ind, ]
test = credit.PDC[-train_ind, ]

#====================================================================================



#==============================================================
#======RandomForest========================================
#==============================================================

credit.PDC$ZipCode = NULL # Removing zipcode to avoid length(levels) problem

# Split data into training/testing samples
smp_size = floor(0.5 * nrow(credit.PDC))
train_ind = sample(seq_len(nrow(credit.PDC)), size = smp_size)
train = credit.PDC[train_ind, ]
test = credit.PDC[-train_ind, ]

# no eth, no male: 87.3%
#fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + BankCustomer + EducationLevel + YearsEmployed + Employed + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
#fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + BankCustomer + EducationLevel + YearsEmployed + PriorDefault + Employed + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
#fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + BankCustomer + EducationLevel + YearsEmployed + PriorDefault + Employed + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
#fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + BankCustomer + EducationLevel + YearsEmployed + PriorDefault + Employed + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)

# No ethnicity: 88.4%
fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + BankCustomer + EducationLevel + YearsEmployed + PriorDefault + Employed + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)


predictionRF = predict(fit, test)
predictionRF[is.na(predictionRF)] = 0
submitRF = data.frame(Real = test$Approve, Predicted = predictionRF, check.names = FALSE, row.names = NULL)
submitRF$Real = as.numeric(submitRF$Real)
submitRF$Predicted = as.numeric(submitRF$Predicted)
#Changing values so they are the same as GAM's
submitRF$Predicted[submitRF$Predicted<1.5] <- 0
submitRF$Predicted[submitRF$Predicted>1.5] <- 1
#accuracy = 1 - sum(abs(submitRF$Real - submitRF$Predicted))/nrow(submitRF)
#accuracy

dataFramePrediction <- data.frame((predictionRF))
ourCMRF <- confusionMatrix(data = submitRF$Predicted, reference = submitRF$Real)
ourCMRF




#==============================================================
#========GAM with MGCV========================================
#==============================================================

#lambdaArray <- c(1:20)
#for (lam in lambdaArray){ 
#Smoothing parameter lambda
lam=1
lamNotPD=0.0001

#b1 <- mgcv::gam(Approve ~ s(Income, bs='ps', sp=lamNotPD)  +s(Debt, bs='ps', sp=lamNotPD)
#                +s(CreditScore, bs='ps', sp=lamNotPD), data = train, method="REML")
b1 <- mgcv::gam(Approve ~ s(PriorDefault, bs='ps', sp=lam), data = train)


summary(b1)
plot(b1)
predictionGAM <- predict(b1, newdata=test) #prediction is a list of values
submitGAM = data.frame(Real = test$Approve, Predicted = predictionGAM, check.names = FALSE, row.names = NULL)
submitGAM$Real = as.numeric(submitGAM$Real)
submitGAM$Predicted = as.numeric(submitGAM$Predicted)
submitGAM$PredictedBoolean <- ifelse(submitGAM$Predicted < 0.5, submitGAM$PredictedBoolean <- 0, submitGAM$PredictedBoolean <- 1)
#==================================================================================
#Build Confusion Matrix
dataFramePrediction <- data.frame((predictionGAM))
ourCM <- confusionMatrix(data = submitGAM$PredictedBoolean, reference = submitGAM$Real)
ourCM


#==============================================================
#========Combining Results========================================
#==============================================================


#prediction[is.na(prediction)] = 0
total <- submitRF
#Temporarily make same as RF data
total$Real = as.numeric(total$Real)
total$PredictedBoolean = as.numeric(total$Predicted)
total$RFPredicted <- submitRF$Predicted
total$GAMPredicted <- submitGAM$PredictedBoolean
#Now if submitGAM has predicted values of 0, copy it over
total$PredictedBoolean <- ifelse(submitGAM$PredictedBoolean==0, total$PredictedBoolean <- 0, ifelse(submitRF$Predicted==1, total$PredictedBoolean <-1, total$PredictedBoolean <-0))


#total$PredictedBoolean <- ifelse(submitGAM$PredictedBoolean==0, total$PredictedBoolean <- 0)
#total$PredictedBoolean <- ifelse(submitRF$Predicted==1, total$PredictedBoolean <- 1)


ourCMTotal <- confusionMatrix(data = total$PredictedBoolean, reference = total$Real)
ourCMTotal

#View(as.matrix(ourCM))
#}
