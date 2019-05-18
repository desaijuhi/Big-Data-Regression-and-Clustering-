# COMP473 Assignment 2
# Question 1 - Regression

# load dataset and generate training and test set
Credit=read.csv("G:/Master/COMP473-BigData/BD-Ass2/Ass2Data/Credit.csv",header=TRUE)
train=sample(1:nrow(Credit),0.70*nrow(Credit))
test=-train
X=model.matrix(Balance~.*.,Credit)[,-1]
y=Credit$Balance

dim(X)

# ridge regression
library(glmnet)
set.seed(987654312)
grid=10^seq(3,-1,length=100)
ridge_model=cv.glmnet(X[train,],y[train],alpha=0,lambda = grid,nfolds = 10,thresh=1e-10)
ridge_model$lambda.min

# lasso regression
library(glmnet)
set.seed(987654312)
grid=10^seq(3,-1,length=100)
lasso_model=cv.glmnet(X[train,],y[train],alpha=1,lambda = grid,nfolds = 10,thresh=1e-10)
lasso_model$lambda.min

#Linear Test
linear_model=lm(y[train]~X[train,])
linear_pred=coef(linear_model)[1]+X[test,]%*%coef(linear_model)[-1]
mean((linear_pred-y[test])^2)

#Ridge Test
ridge_lambda=ridge_model$lambda.min
ridge_pred=predict(ridge_model,s=ridge_lambda,newx = X[test,])
mean((ridge_pred-y[test])^2)

#Lasso Test
lasso_lambda=lasso_model$lambda.min
lasso_pred=predict(lasso_model,s=lasso_lambda,newx = X[test,])
mean((lasso_pred-y[test])^2)

#Graph
plot(y[test],linear_pred,ylim = c(-400,1700),xlab = "y_test",ylab = "predicted")
points(y[test],ridge_pred,col="blue")
points(y[test],lasso_pred,col="red")
abline(0,1)

