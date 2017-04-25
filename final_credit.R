install.packages("gbm")
install.packages('dplyr')
library(gbm)
library(dplyr)
source("/Users/ruebenbautista/Desktop/Spring\ 2017/MATH\ 485/credit_gbm.R")


row.sample <- sample(1:nrow(credit), size = 516, replace = FALSE)

train <- credit[row.sample,]
test <- credit[-row.sample,]

final <- gbm(Approve ~ ., data = train, distribution = "bernoulli",
             n.trees = 10000, shrinkage = 0.01, cv.folds = 3)

pred0 <- predict(object = final, newdata = train, n.trees = 5000, type = 'response')
correct0 <- ifelse(train$Approve == 0 & pred0 < 0.5 | train$Approve == 1 & pred0 >= 0.5, correct <- TRUE, correct <- FALSE)
table(correct0)


predicted_approval <- predict(object = final, newdata = test, n.trees = 9000, type = 'response')
correct <- ifelse(test$Approve == 0 & predicted_approval < 0.5 | test$Approve == 1 & predicted_approval >= 0.5, correct <- TRUE, correct <- FALSE)
table(correct)


temp <- credit[,-14]
temp.train <- temp[row.sample,]
temp.test <- temp[-row.sample,]

g.temp <- gbm(Approve ~ ., data = temp.train, distribution = 'bernoulli',
              n.trees = 10000, shrinkage = 0.001, cv.folds = 5)
temp.pred <- predict(object = g.temp, newdata = temp.train, n.trees = 5000)

temp.correct0 <- ifelse(temp.train$Approve == 0  & temp.pred < 0.5 | temp.train$Approve == 1 & temp.pred >= 0.5, temp.correct <- TRUE, temp.correct <- FALSE)
table(temp.correct0)
temp.correct0 <- ifelse(temp.correct0 == FALSE, temp.correct0 <- 0, temp.correct0 <- 1)

temp.response <- ifelse(temp.pred < 0.5, temp.response <- 0, temp.response <- 1)
confusionMatrix(data = temp.response, reference = temp.train$Approve)


temp.pred1 <- predict(object = g.temp, newdata = temp.test, n.trees = 8500)
temp.correct1 <- ifelse(temp.test$Approve == 0 & temp.pred1 < 0.5 | temp.test$Approve == 1 & temp.pred1 >= 0.5, temp.correct1 <- TRUE, temp.correct1 <- FALSE)
table(temp.correct1)
temp.response1 <- ifelse(temp.pred1 < 0.5, temp.response1 <- 0, temp.response1 <- 1)
confusionMatrix(data = temp.response1, reference = temp.test$Approve)


#--------------------------------------------------------------------------------------------

#Create a new data frame that has a sequence of Credit Scores from the minimum to the maximum from the credit data set. Our goal is to see how the probability of approval changes based on the change of a single factor.

min_credit <- min(credit$CreditScore)
max_credit <- max(credit$CreditScore)

cred.score.seq <- seq(min_credit, max_credit, by = 0.01)
new.credit <- credit
new.credit$CreditScore <- cred.score.seq

new.final <- gbm(Approve ~., data = new.credit, distribution = 'bernoulli', n.trees = 7500,
                 cv.folds = 3, shrinkage = 0.01)
pred.new.credit <- predict(object = new.final, newdata = new.credit, n.trees = 5000, type = 'response')

correct2 <- ifelse(new.credit$Approve == 0 & pred.new.credit < 0.5 | new.credit$Approve == 1 & pred.new.credit >= 0.5, correct2 <- TRUE, correct2 <- FALSE)
table(correct2)

new.default.yes <- data.frame("Male" = 1,
                              "Age" = 31.56,
                              "Debt" = 4.77,
                              "Married" = 'u',
                              "BankCustomer" = 'g',
                              "EducationLevel" = 'c',
                              "Ethnicity" = 'v',
                              "YearsEmployed" = 2.225,
                              "PriorDefault" = 't',
                              "Employed" = 'f',
                              "CreditScore" = cred.score.seq,
                              "DriversLicense" = 'f',
                              "Citizen" = 'g',
                              "ZipCode" = 00000,
                              "Income" = 1019,
                              "IncomeLog" = 0.8476,
                              "CreditScoreLog" = log10(cred.score.seq),
                              "CreditScoreLogE" = log(cred.score.seq),
                              "AgeNorm" = 0,
                              "YearsEmployedLog" = 0.3653,
                              "DebtLog" = 0.4146)

new.default.no <- data.frame("Male" = 1,
                             "Age" = 31.56,
                             "Debt" = 4.77,
                             "Married" = 'u',
                             "BankCustomer" = 'g',
                             "EducationLevel" = 'c',
                             "Ethnicity" = 'v',
                             "YearsEmployed" = 2.225,
                             "PriorDefault" = 'f',
                             "Employed" = 'f',
                             "CreditScore" = cred.score.seq,
                             "DriversLicense" = 'f',
                             "Citizen" = 'g',
                             "ZipCode" = 00000,
                             "Income" = 1019,
                             "IncomeLog" = 0.8476,
                             "CreditScoreLog" = log10(cred.score.seq),
                             "CreditScoreLogE" = log(cred.score.seq),
                             "AgeNorm" = 0,
                             "YearsEmployedLog" = 0.3653,
                             "DebtLog" = 0.4146)


pred.default.yes <- predict(object = final, newdata = new.default.yes, n.trees = 1000, type = 'response')
pred.default.no <- predict(object = final, newdata = new.default.no, n.trees = 1000, type = 'response')

plot(cred.score.seq, pred.default.no)
lines(x = cred.score.seq, y = pred.default.yes)

#Combininig the models:

#gbm*weight + quad*weight + gam*weight + rf*weight

new.default.no <- data.frame("Male" = 1,
                             "Age" = 31.56,
                             "Debt" = 4.77,
                             "Married" = 'u',
                             "BankCustomer" = 'g',
                             "EducationLevel" = 'c',
                             "Ethnicity" = 'v',
                             "YearsEmployed" = 2.225,
                             "PriorDefault" = 'f',
                             "Employed" = 'f',
                             "CreditScore" = 2.40,
                             "DriversLicense" = 'f',
                             "Citizen" = 'g',
                             "ZipCode" = 00000,
                             "Income" = 1019,
                             "IncomeLog" = 0.8476,
                             "CreditScoreLog" = 0.38,
                             "CreditScoreLogE" = 0.88,
                             "AgeNorm" = 0,
                             "YearsEmployedLog" = 0.3653,
                             "DebtLog" = 0.4146)

new.default.yes <- data.frame("Male" = 1,
                             "Age" = 31.56,
                             "Debt" = 4.77,
                             "Married" = 'u',
                             "BankCustomer" = 'g',
                             "EducationLevel" = 'c',
                             "Ethnicity" = 'v',
                             "YearsEmployed" = 2.225,
                             "PriorDefault" = 't',
                             "Employed" = 'f',
                             "CreditScore" = 2.40,
                             "DriversLicense" = 'f',
                             "Citizen" = 'g',
                             "ZipCode" = 00000,
                             "Income" = 1019,
                             "IncomeLog" = 0.8476,
                             "CreditScoreLog" = 0.38,
                             "CreditScoreLogE" = 0.88,
                             "AgeNorm" = 0,
                             "YearsEmployedLog" = 0.3653,
                             "DebtLog" = 0.4146)


pred.entire <- predict(new.final, newdata = credit, type = 'response', n.trees = 7500)
temp.correct.final <- ifelse(pred.entire < 0.5, temp.correct.final <- 0, temp.correct.final <- 1)

confusionMatrix(data = temp.correct.final, reference = credit$Approve)
