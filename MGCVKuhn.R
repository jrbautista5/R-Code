#Install packages and attach appropriate libraries:
#install.packages('gam')
#install.packages('randomForest')
#install.packages('gbm')
#install.packages('mgcv')
#install.packages('ggplot2')

"===Code Starts Here==="
"===Code Starts Here==="
"===Code Starts Here==="

#library(gam)
library(randomForest)
library(gbm)
library(mgcv)
library(ggplot2)
### Based on GAM example using mgcv
##By: http://multithreaded.stitchfix.com/blog/2015/07/30/gam/
dat <- credit.numeric

smp_size = floor(0.75 * nrow(credit.numeric))
train_ind = sample(seq_len(nrow(credit.numeric)), size = smp_size)
#Specify testing and training portions of code
train = credit.numeric[train_ind, ]
test = credit.numeric[-train_ind, ]



#lambdaArray <- c(1:20)
#for (lam in lambdaArray){ 
  #Smoothing parameter lambda
  lam=10
  
  # P-spline smoothers (with lambda=0.6) used for x1 and x2; x3 is parametric.
  b1 <- mgcv::gam(Approve ~ s(Age, bs="ps", sp=lam) + s(Debt, bs='ps', sp=lam) + s(YearsEmployed, bs='ps', sp=lam) + 
                    s(CreditScore, bs='ps', sp=lam) + s(Income, bs='ps', sp=lam), data = train)
  summary(b1)
  plot(b1)
  
  
  # # # plot the smooth predictor function for x1 with ggplot to get a nicer looking graph
  #  p <- predict(b1, type="lpmatrix")
  #  beta <- coef(b1)[grepl("Age", names(coef(b1)))]
  #  s <- p[,grepl("Age", colnames(p))] %*% beta
  #  ggplot(data=cbind.data.frame(s, dat$Age), aes(x=dat$Age, y=s)) + geom_line()
  #  
  #  beta <- coef(b1)[grepl("Debt", names(coef(b1)))]
  #  s <- p[,grepl("Debt", colnames(p))] %*% beta
  #  ggplot(data=cbind.data.frame(s, dat$Debt), aes(x=dat$Debt, y=s)) + geom_line()
  
  # predict
  prediction <- predict(b1, newdata=test)
  prediction[is.na(prediction)] = 0
  submit = data.frame(Real = test$Approve, Predicted = prediction, check.names = FALSE, row.names = NULL)
  submit$Real = as.numeric(submit$Real)
  submit$Predicted = as.numeric(submit$Predicted)
  accuracy = 1 - sum(abs(submit$Real - submit$Predicted))/nrow(submit)

  " "
  print(lam)
  print(accuracy)
  
  #Additional options we could use to replace b1:
  # 
  # # select smoothing parameters with REML, using P-splines
  # b2 <- mgcv::gam(y ~ s(x1, bs='ps') + s(x2, bs='ps') + x3, data = dat, method="REML")
  # 
  # # select variables and smoothing parameters
  # b3 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3) , data = dat, method="REML", select=TRUE)
  #
  # # loess smoothers with the gam package (restart R before loading gam)
  # library(gam)
  # b4 <- gam::gam(y ~ lo(x1, span=0.6) + lo(x2, span=0.6) + x3, data = dat)
  # summary(b4)
#}
