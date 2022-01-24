# Read in data
data <- read.csv("train-xy.csv")
test_set <- read.csv("test-x.csv")

# Data preprocessing
data[is.na(data)] <- 0
test_set[is.na(test_set)] <- 0
set.seed(1)
sample <- sample(1:nrow(data), nrow(data)/2)
train <- data[sample, ]
test <- data[-sample, ]

# Forward/Backward stepwise
library(leaps)
fwd <- regsubsets(Y~.,data = train, nvmax = 100, method="forward")
summary <- summary(fwd)
par(mfrow=c(2,2))
plot(summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
plot(summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
plot(summary$bic,xlab="Number of Variables",ylab="BIC",type='l')

test.mat <- model.matrix(Y~.,data = test)
val.errors <- rep(NA,100)
for(i in 1:100){
  coefi <- coef(fwd, id=i)
  pred <- test.mat[,names(coefi)] %*% coefi
  val.errors[i] <- mean((test$Y-pred)^2)
}

# validation error and model that has min error
val.errors
which.min(val.errors)

fwd.yhat <- test.mat[,names(coef(fwd, id = 10))] %*% coef(fwd, id = 10)
mean((test$Y - fwd.yhat)^2)

bwd <- regsubsets(Y~.,data = train, nvmax = 100, method="backward")
summarybwd <- summary(bwd)
par(mfrow=c(2,2))
plot(summarybwd$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(summarybwd$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
plot(summarybwd$cp,xlab="Number of Variables",ylab="Cp",type='l')
plot(summarybwd$bic,xlab="Number of Variables",ylab="BIC",type='l')

## Lasso
library(glmnet)
x <- model.matrix(Y~., data = train)[,-1]
y <- train$Y
x_test <- model.matrix(Y~., data = test)[,-1]
y_test <- test$Y

# Cross validation for lambda selection
lambda_to_try <- 10^seq(-3,5, length.out = 100)
cv_mod <- cv.glmnet(x, y, alpha = 1, lambda = lambda_to_try)
plot(cv_mod)
best_lambda <- cv_mod$lambda.min
best_lambda

lasso <- glmnet(x, y, alpha = 1, lambda = best_lambda)
lasso.yhat <- predict(lasso, x_test)
mean((lasso.yhat - y_test)^2)

#Lasso prediction
x_test_set <- as.matrix(test_set)
lasso_test <- predict(lasso, s=best_lambda, newx=x_test_set) 
colnames(lasso_test) <- "Y"
write.csv(lasso_test, file="A0204980E.csv", row.names = FALSE)

## Ridge regression
library(glmnet)
x <- model.matrix(Y~., data = train)[,-1]
y <- train$Y
x_test <- model.matrix(Y~., data = test)[,-1]
y_test <- test$Y

# Cross validation for ridge regression
lambdas <- 10^seq(-3,5, length.out = 100)
cv_ridge <- cv.glmnet(x, y, alpha = 0, lambda = lambdas)
plot(cv_ridge)
best <- cv_ridge$lambda.min
best

ridge <- glmnet(x, y, alpha = 1, lambda = best)
ridge.yhat <- predict(ridge, x_test)
mean((ridge.yhat - y_test)^2)



