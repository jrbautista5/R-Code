# Install packages and attach appropriate libraries:
install.packages('gam')
install.packages('randomForest')
install.packages('gbm')

library(gam)
library(randomForest)
library(gbm)

set.seed(666)

crx.data = read.csv('/home/jvita/Documents/school/math-485/crx.data')

# Import the credit dataframe:
credit <- crx.data

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
#---------------------------------------------------------------------------
#Fill in missing categorical values with the mode for each.

credit$BankCustomer[is.na(credit$BankCustomer)] = 'g'
#---------------------------------------------------------------------------

credit$ZipCode = NULL # Removing zipcode to avoid length(levels) problem

# Split data into training/testing samples
smp_size = floor(0.75 * nrow(credit))
train_ind = sample(seq_len(nrow(credit)), size = smp_size)
train = credit[train_ind, ]
test = credit[-train_ind, ]

# no eth, no male: 87.3%
fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + BankCustomer + EducationLevel + YearsEmployed + Employed + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + BankCustomer + EducationLevel + YearsEmployed + PriorDefault + Employed + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + BankCustomer + EducationLevel + YearsEmployed + PriorDefault + Employed + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + BankCustomer + EducationLevel + YearsEmployed + PriorDefault + Employed + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)

prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy

# All vars: 86.1%
fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + BankCustomer + EducationLevel + Ethnicity + YearsEmployed + PriorDefault + Employed + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy

# No income: 87.3%
fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + BankCustomer + EducationLevel + Ethnicity + YearsEmployed + PriorDefault + Employed + CreditScore + DriversLicense + Citizen, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy

# No citizen: 87.3%
fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + BankCustomer + EducationLevel + Ethnicity + YearsEmployed + PriorDefault + Employed + CreditScore + DriversLicense + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy

# No driversLicense: 87.3%
fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + BankCustomer + EducationLevel + Ethnicity + YearsEmployed + PriorDefault + Employed + CreditScore + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy

# No creditScore: 86.1%
fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + BankCustomer + EducationLevel + Ethnicity + YearsEmployed + PriorDefault + Employed + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy

# no employed: 86.7%
fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + BankCustomer + EducationLevel + Ethnicity + YearsEmployed + PriorDefault + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy

# no priorDefault: 79.7%
fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + BankCustomer + EducationLevel + Ethnicity + YearsEmployed + Employed + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy

# no yearsEmp: 87.3%
fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + BankCustomer + EducationLevel + Ethnicity + PriorDefault + Employed + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy

# No ethnicity: 88.4%
fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + BankCustomer + EducationLevel + YearsEmployed + PriorDefault + Employed + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy

# No educationLevel: 87.3%
fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + BankCustomer + Ethnicity + YearsEmployed + PriorDefault + Employed + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy

# No bankCustomer: %
fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + Married + EducationLevel + Ethnicity + YearsEmployed + PriorDefault + Employed + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy

# No married: %
fit <- randomForest(as.factor(Approve) ~ Male + Age + Debt + BankCustomer + EducationLevel + Ethnicity + YearsEmployed + PriorDefault + Employed + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy

# No male: 86.7%
#fit <- randomForest(as.factor(Approve) ~ Age + Debt + Married + EducationLevel + Ethnicity + YearsEmployed + PriorDefault + Employed + CreditScore + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
fit <- randomForest(as.factor(Approve) ~ Age + Debt + Married + BankCustomer + EducationLevel + Ethnicity + YearsEmployed + PriorDefault + Employed + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy

# No debt: 86.7%
fit <- randomForest(as.factor(Approve) ~ Male + Age + Married + BankCustomer + EducationLevel + Ethnicity + YearsEmployed + PriorDefault + Employed + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy

# No Age: 87.3%
fit <- randomForest(as.factor(Approve) ~ Male + Debt + Married + BankCustomer + EducationLevel + Ethnicity + YearsEmployed + PriorDefault + Employed + CreditScore + DriversLicense + Citizen + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy

# no PriorDefault: 87.9%
fit <- randomForest(as.factor(Approve) ~ Age + Debt + Married + EducationLevel + YearsEmployed + PriorDefault + Employed + CreditScore + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy 

# only financials: 87.3%
fit <- randomForest(as.factor(Approve) ~ Debt + PriorDefault + CreditScore + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy

# only human traits: 73.4%
fit <- randomForest(as.factor(Approve) ~ Age + Married + EducationLevel + YearsEmployed + Employed, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy

# Kuhn variables: 79.2%
fit <- randomForest(as.factor(Approve) ~ YearsEmployed + CreditScore + Income, data=train, importance=TRUE, ntree=2000, na.action=na.exclude)
prediction = predict(fit, test)
prediction[is.na(prediction)] = 0
submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
submit$Real = as.numeric(submit$Real)
submit$Predicted = as.numeric(submit$Predicted)
accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)
accuracy