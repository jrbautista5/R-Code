#Install packages and attach appropriate libraries:
#install.packages('gam')
#install.packages('randomForest')
#install.packages('gbm')
#install.packages('mgcv')
#install.packages('ggplot2')
#install.packages('caret', dependencies = TRUE)#Needed for confusion matrix
"===Code Starts Here==="
"===Code Starts Here==="
"===Code Starts Here==="





#library(gam)
library(caret)
library(randomForest)
library(gbm)
library(mgcv)
library(ggplot2)
library(e1071)
### Based on GAM example using mgcv
##By: http://multithreaded.stitchfix.com/blog/2015/07/30/gam/

#credit$PriorDefault <- replace(credit$PriorDefault, credit$PriorDefault == 't', '1')


#====================================================================================
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
prediction <- predict(b1, newdata=test) #prediction is a list of values


# Out of Sample Error
#prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
submit$PredictedBoolean <- ifelse(submit$Predicted < 0.5, submit$PredictedBoolean <- 0, submit$PredictedBoolean <- 1)
#accuracy = 1 - sum(abs(submit$Real - submit$PredictedBoolean))/nrow(submit)

dataFramePrediciton <- data.frame((prediction))
ourCM <- confusionMatrix(data = submit$PredictedBoolean, reference = submit$Real)
ourCM
#View(as.matrix(ourCM))
#}

