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

#Adding more code here to try and get us on track for recreating the Kuhn results!

mean_Age <- mean(credit$Age)
sd_Age <- sd(credit$Age)
credit$AgeNorm <- (credit$Age - mean_Age) / sd_Age
hist(credit$AgeNorm)

credit$YearsEmployedLog <- log(credit$YearsEmployed + (1/exp(1)))
boxplot(credit$YearsEmployedLog, horizontal = TRUE)

credit$DebtLog <- log10(credit$Debt + 0.1)
boxplot(credit$DebtLog, horizontal = TRUE)
#Begin the logistic regression copy from the Kuhn paper:

model1 <- glm(formula = Approve ~ AgeNorm + DebtLog + YearsEmployedLog + 
      CreditScoreLog + IncomeLog, family = binomial, data = credit)
summary(model1)

#Calculate approved proportion from the fitted values:
model1_approved <- length(subset(model1$fitted.values, model1$fitted.values > 0.5)) / length(credit$Approve[credit$Approve == 1])
#We correctly predicted approximately 0.90 of the Approved applications in the 'credit' data.

#Calculate rejected proportion from the model1 fitted values:
model1_denied <- (nrow(credit) - model1_approved) / length(as.numeric(credit$Approve[credit$Approve == 0]))
# We over predicted by approximately `(1.078329 - 1)*nrow(credit)` or 54 applications of those that we know to be denied.

#Gather the indices of the fitted values < 0.5 and compare them to the known declined applications in the 'credit' dataset.
predicted.declined.index <- as.numeric(as.character(names(model1$fitted.values[model1$fitted.values < 0.5])))
BooleanApprove <- credit$Approve
levels(BooleanApprove) <- c(FALSE, TRUE)
actual.declined <- which(BooleanApprove == FALSE)

new = c()
for (i in 1:length(predicted.declined.index))
{
  if (actual.declined[i] %in% predicted.declined.index == TRUE){
    new[i] <- TRUE
  }
}
new <- new[!is.na(new)]
correct.predicted <- length(new) / length(credit$Approve[credit$Approve == 0])
#correct.predicted == 0.8276762

#Create a numeric credit dataframe by selecting the columns with numeric entires. If the columns do not have the 'numeric' class, change them into numeric. Additionally, add a column to this numeric dataframe of the fitted values from model1. We want to run a correlation and see how closely the predictor variables are to 
credit.numeric <- credit[c(2, 3, 8, 11, 15, 16, 17, 18, 19, 20, 21, 22)]
credit.numeric$Model1FittedValues <- model1$fitted.values
credit.numeric$Approve <- as.numeric(as.character(credit.numeric$Approve))
credit.numeric$CreditScore <- as.numeric(credit.numeric$CreditScore)
credit.numeric$Income <- as.numeric(credit.numeric$Income)

View(cor(credit.numeric))



