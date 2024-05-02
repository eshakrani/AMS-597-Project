# Gaussian Examples

# Generate synthetic data
n <- 50
p <- 10
X <- matrix(rnorm(n * p), nrow = n)
beta <- matrix(rep(0, p))
beta[sample(1:p, 5)] <- 1  # True coefficients
y <- X %*% beta + rnorm(n)
x = data.frame(X)
y = data.frame(y = y)

# OLS Example
fit1 = modelFit(x, y, family = "gaussian", lambda = 0)
print(fit1$coefficients)
print(fit1$fitted.values[1:9])

# Linear Regression using Elastic Net with a alpha value and lambda sequence
alpha0 = 0.5
lambda0 = seq(0, 10, 0.1)
fit2 = modelFit(x, y, family = "gaussian", lambda = lambda0, alpha = alpha0)
print(fit2)

coef(fit2, s = 1) # extract coefficients at a single value of lambda
predict(fit2, newx = as.matrix(x), s = 1) # Model coefficients at single value of lambda

# Linear Regression using Elastic Net with an alpha sequence and no lambda sequence
alpha_seq = seq(0, 1, 0.1)
fit3 = modelFit(x, y, family = "gaussian", alpha = alpha_seq)
print(fit3)
predict(fit3$model, newx = as.matrix(x), s = fit3$lambda)

# Linear Regression with Bagging
fit4 = modelFit(x, y, family = "gaussian", lambda = 0, bagging = T)
print(fit4$y_pred_avg)

# Linear Regression with Bagging with Lasso constant lambda
fit5 = modelFit(x, y, family = "gaussian", alpha = 1, lambda = 0.1, bagging = T)
print(fit5$y_pred_avg)
print(fit5$naive_score)

# Linear Regression with Bagging with Lasso unknown lambda
fit6 = modelFit(x, y, family = "gaussian", alpha = 1, bagging = T)
print(fit6$y_pred_avg)
print(fit6$naive_score)

# Linear Regression with Ensemble using GLM
fit7 = modelFit(x, y, family = "gaussian", ensemble = T, alpha = 1, 
                meta_learner = "glm")
print(fit7)

# Linear Regression with Ensemble using SVM
fit8 = modelFit(x, y, family = "gaussian", ensemble = T, lambda = 0, 
                models_list = "svm", meta_learner = "svm")
print(fit8)

# Linear Regression with Ensemble using SVM
fit9 = modelFit(x, y, family = "gaussian", ensemble = T, lambda = 0, 
                meta_learner = "randomForest")
print(fit9)


# Logistic Examples
n <- 1000
x1 <- rnorm(n, 10, 2)
x2 <- rnorm(n, 5, 1)
x3 <- rnorm(n, 8, 3)

# Generate outcome variable
logistic_function <- function(x1, x2, x3) {
  p <- 1 / (1 + exp(-(-2 + 0.5 * x1 + 0.3 * x2 - 0.2 * x3)))  # Log odds
  response <- rbinom(length(x1), 1, p)  # Generate binary response
  return(response)
}

y <- factor(logistic_function(x1, x2, x3))
data <- data.frame(x1 = x1, x2 = x2, x3 = x3, y = as.character(y))

# Simple Logistic Regression
fit10 = modelFit(data[,c(1,2,3)], data.frame(y), lambda = 0, family = 'binomial')
fit10$coefficients

