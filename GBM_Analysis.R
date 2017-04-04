install.packages("gbm")
library(gbm)
source("/Users/ruebenbautista/Desktop/Spring\ 2017/MATH\ 485/mimic.R")

na_index <- which(is.na(credit))
credit$Male[na_index] <- 1
levels(credit$Approve) <- c(-1, 1) #May need to take this out and change levels of approve later on, because gbm uses bernoulli distribution and uses 0, 1 values only.
credit$Approve <- as.numeric(as.character(credit$Approve))

row_sample <- sample(1:nrow(credit), size = 500, replace = FALSE)

Train <- credit[row_sample,]

Test <- credit[-row_sample,]

g1 <- gbm(Approve ~ CreditScoreLogE + IncomeLog + YearsEmployedLog + 
            AgeNorm + DebtLog, data = Train, 
          distribution = "bernoulli", n.trees = 400, shrinkage = 0.01)

g1.pred1 <- predict(object = g1, newdata = Train, n.trees = 100)
#From the Train dataset, gather the known and predicted values and test whether the prediction was correct.
df <- data.frame("Known" = Train$Approve, "Predicted" = g1.pred1)
df$Sign <- sign(g1.pred1)
a <- ifelse(df$Known == df$Sign, df$Correct <- TRUE, df$Correct <- FALSE)
df$Correct <- a
table(df$Correct)["TRUE"]/500

#Now test on the Test set.
g1.pred2 <- predict(object = g1, newdata = Test, n.trees = 100)
df2 <- data.frame("Known" = Test$Approve, "Predicted" = g1.pred2, "Sign" = sign(g1.pred2))
b <- ifelse(df2$Known == df2$Sign, df2$Correct <- TRUE, df2$Correct <- FALSE)
df2$Correct <- b
table(df2$Correct)["TRUE"] / nrow(df2)

#Use g1 to predict, but with more trees:
g1.pred3 <- predict(object = g1, newdata = Train, n.trees = 1000)
df3 <- data.frame("Known" = Train$Approve, "Predicted" = g1.pred3, "Sign" = sign(g1.pred3))
c <- ifelse(df3$Known == df3$Sign, df3$Correct <- TRUE, df3$Correct <- FALSE)
df3$Correct <- c
table(df3$Correct)["TRUE"]/500
#The model gains approximately 8% predictive power. Likely due to the increase in the number of trees in g1.pred3.

g1.pred4 <- predict(object = g1, newdata = Test, n.trees = 1000)
df4 <- data.frame("Known" = Test$Approve, "Predicted" = g1.pred4, "Sign" = sign(g1.pred4))
d <- ifelse(df4$Known == df4$Sign, df4$Correct <- TRUE, df4$Correct <- FALSE)
df4$Correct <- d
table(df4$Correct)["TRUE"] / 189 

#------------------------------------------------------------------------------------
#Make a new gbm model with different (or more) predictor variables:

g_credit <- credit[c("Male", "AgeNorm", "Debt", "Married", "BankCustomer", "EducationLevel", "YearsEmployed", "PriorDefault", "Employed", "CreditScoreLogE", "IncomeLog", "Approve")]

g_credit$Approve <- as.numeric(as.character(g_credit$Approve))
g2 <- gbm(Approve ~ ., data = g_credit, distribution = "bernoulli",
          n.trees = 1500, shrinkage = 0.01, cv.folds = 3)

g2.pred1 <- predict(object = g2, newdata = g_credit, n.trees = 5000, type = "response")
df.g2.1 <- data.frame("Known" = g_credit$Approve, "Predicted" = g2.pred1)
e <- ifelse((df.g2.1$Predicted < 0.5 & df.g2.1$Known == 0) | (df.g2.1$Predicted >= 0.5 & df.g2.1$Known == 1),
            df$Correct <- TRUE, df$Correct <- FALSE)
df.g2.1$Correct <- e
table(df.g2.1$Correct)["TRUE"] / nrow(g_credit)

#We may be overfitting, so we are going to take a sample of our data and call it 'train' and have the remaining observations will be our 'test' data.

row.sample <- sample(1:nrow(g_credit), size = 400, replace = FALSE)
train <- g_credit[row.sample,]
test <- g_credit[-row.sample,]

g3 <- gbm(Approve ~ . , data = train, distribution = "bernoulli",
          n.trees = 1500, shrinkage = 0.01, cv.folds = 5)

g3.pred <- predict(g3, train, n.trees = 1000, type = "response")

df.g3 <- data.frame("Known" = train$Approve, "Predicted" = g3.pred)

f <- ifelse((df.g3$Predicted < 0.5 & df.g3$Known == 0) | (df.g3$Predicted >= 0.5 & df.g3$Known == 1),
            x <- TRUE, x <- FALSE)

df.g3$Correct <- f

table(df.g3$Correct)["TRUE"] / nrow(train)

#Now use the g3 model on the test data set and see how we do.
g3.pred2 <- predict(g3, test, n.trees = 500, type = "response")

df.g3.2 <-data.frame("Known" = test$Approve, "Predicted" = g3.pred2)

g <- ifelse((df.g3.2$Predicted < 0.5 & df.g3.2$Known == 0) | (df.g3.2$Predicted >= 0.5 & df.g3.2$Known == 1),
            x <- TRUE, x <- FALSE)

df.g3.2$Correct <- g

table(df.g3.2$Correct)["TRUE"] / nrow(test)

#This gbm model produces a correctness that seems to level out around 90% on both the training and test data sets.

#Let's now look at how much each variable influences the overall outcome.
summary(g1, las = 2)
summary(g2, las = 2)
summary(g3, las = 2)


#It is interesting to not that each model has different levels of relative influence, which makes sense because it does utilize a random process. However, it is a goal to try and maintain consistency of which predictor variables are most influential in determining approval.

row.sample2 <- sample(1:nrow(g_credit), size = 600, replace = FALSE)
train.final <- g_credit[row.sample2,]
test.final <- g_credit[-row.sample2,]

g.final <- gbm(Approve ~ . , data = train.final, distribution = "bernoulli",
               n.trees = 10000, shrinkage = 0.01, cv.folds = 3)

pred.final <- predict(object = g.final, newdata = train.final, n.trees = 5000)
final <- data.frame("Known" = train.final$Approve, "Predicted" = pred.final)

h <- ifelse((final$Predicted < 0.5 & final$Known == 0) | (final$Predicted >= 0.5 & final$Known == 1),
            x <- TRUE, x <- FALSE)
final$Correct <- h

table(final$Correct)["TRUE"] / nrow(train.final)

