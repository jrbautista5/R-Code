# Install packages and attach appropriate libraries:
install.packages('gam')
install.packages('randomForest')
install.packages('gbm')

library(gam)
library(randomForest)
library(gbm)

# Import the credit dataframe: (This is the location for my Desktop, this line will need to be changed in order to run it on your own computer.
credit <- read.csv('/Users/ruebenbautista/Desktop/crx.data.txt')

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

#---------------------------------------------------------------------------
#Begin the logistic regression copy from the Kuhn paper:

model1 <- glm(formula = Approve ~ AgeNorm + DebtLog + YearsEmployedLog + 
      CreditScoreLog + IncomeLog, family = binomial, data = credit)
summary(model1)
#---------------------------------------------------------------------------

# Our next goal is to determine how well our model predicted whether a certain person would be approved or denied.

# Let's gather the indices of the fitted values < 0.5 and compare them to the known declined applications in the 'credit' dataset. For the purpose of our analysis, if a predicted value falls below 0.5 we will consider it as a denial.
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
#---------------------------------------------------------------------------

# The following is a way to see (all at once) how our predictor variables correlate with one another. `lm` gives us significance of variables, however it may be more useful to see the direct correlations. Additionally, we can see the impact our 
# Create a numeric credit dataframe by selecting the columns with numeric entires. If the columns do not have the 'numeric' class, change them into numeric. Additionally, we will add a column to this numeric dataframe of the fitted values from model1 in order to see how the fitted values correlate.

credit.numeric <- credit[c(2, 3, 8, 11, 15, 16, 17, 18, 19, 20, 21, 22)]
credit.numeric$Model1FittedValues <- model1$fitted.values
credit.numeric$Approve <- as.numeric(as.character(credit.numeric$Approve))
credit.numeric$CreditScore <- as.numeric(credit.numeric$CreditScore)
credit.numeric$Income <- as.numeric(credit.numeric$Income)

View(cor(credit.numeric))

# We find that the Approve variable has a correlation coefficient of ~0.57, which makes sense because Approve only has 0/1 values and the fitted values will be continuous on the interval [0,1]. So, a correlation coefficient around 0.5 makes decent sense.

#Gather more predictive stats about predictors and model1:
summary(credit)

#This line just gives a basic idea about the distributions of all of the variables in the 'credit' dataset.


#Consider an Age threshold and break the data to see the differences in correlations between the two subsets. Found threshold by sorting (sorted.age <- sort(credit$Age)) and then dividing the length by 2 and rounding to the nearest whole number, 28.
younger_than_28 <- subset(credit.numeric, Age < 28)
View(cor(younger_than_28))

older_than_28 <- subset(credit.numeric, Age >= 28)
View(cor(older_than_28))

#Models on Age subsets:
model_young <- glm(formula = Approve ~ DebtLog + YearsEmployedLog + CreditScoreLog + IncomeLog, family = binomial, data = younger_than_28)
summary(model_young)
#Significant predictors: YearsEmployedLog and CreditScoreLog
younger_than_28$Approve2 <- younger_than_28$Approve
levels(younger_than_28$Approve2) <- c(-1, 1)
predYoung <- sign(predict(model_young))
mean(younger_than_28$Approve2 == predYoung)


model_old <- glm(formula = Approve ~ DebtLog + YearsEmployedLog + CreditScoreLog + IncomeLog, family = binomial, data = older_than_28)
summary(model_old)
#Significant predictors: YearsEmployedLog, CreditScoreLog, and IncomeLog

