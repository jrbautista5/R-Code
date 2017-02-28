# Install packages and attach appropriate libraries:
install.packages('gam')
install.packages('randomForest')
install.packages('gbm')

library(gam)
library(randomForest)
library(gbm)

# Import the credit dataframe:
credit <- read.csv('/Users/ruebenbautista/Desktop/crx.data.txt')

# Rename the columns of the 'credit' dataframe:
colnames(credit) <- c('Male', 'Age', 'Debt', 'Married', 'BankCustomer',
                   'EducationLevel', 'Ethnicity', 'YearsEmployed',
                   'PriorDefault', 'Employed', 'CreditScore',
                   'DriversLicense', 'Citizen', 'ZipCode',
                   'Income', 'Approve')
#---------------

# Clean data and get rid of '?' and replace with NA values:
levels(credit$Approve) <- c(0, 1)
levels(credit$Male) <- c(NA, 0, 1)
credit$Age <- as.numeric(as.character(credit$Age))
credit$Married <- replace(credit$Married, credit$Married == '?', NA)
credit$BankCustomer <- replace(credit$BankCustomer, credit$BankCustomer == '?', NA)
credit$EducationLevel <- replace(credit$EducationLevel, credit$EducationLevel == '?', NA)
credit$Ethnicity <- replace(credit$Ethnicity, credit$Ethnicity == '?', NA)
credit$ZipCode <- replace(credit$ZipCode, credit$ZipCode == '?', NA)
#---------------

#Create a multiple linear regression model to predict missing Age values.
age.predictor <- lm(Age ~ Debt + YearsEmployed + Married, data = credit)

predicted_age <- predict(age.predictor, data = credit)

credit$Age[is.na(credit$Age)] = predicted_age[is.na(credit$Age)]

#Fill in missing categorical values with the mode for each.

credit$BankCustomer[is.na(credit$BankCustomer)] = 'g'

# Log Transforms of Variables
credit$IncomeLog <- log10(credit$Income + 0.1)
boxplot(credit$IncomeLog ~ credit$Approve, horizontal = TRUE)

redit$CreditLog <- log10(credit$CreditScore + 0.1)
boxplot(credit$CreditLog ~ credit$Approve, horizontal = TRUE)

credit$CreditLogE <- log(credit$CreditScore + (exp(-1)))
boxplot(credit$CreditLogE ~ credit$Approve, horizontal = TRUE)


