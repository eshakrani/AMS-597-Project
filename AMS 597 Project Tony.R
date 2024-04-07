#AMS 597 Project Tony
simple_linear_regression <- function(X, y) {
  # Add intercept term to X
  X <- cbind(1, X)
  
  # Compute beta_hat (coefficients) using matrix operations
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  
  # Return coefficients
  return(beta_hat)
}
###########################################################
#Logistic Regression 
#Theory from: https://thelaziestprogrammer.com/sharrington/math-of-machine-learning/solving-logreg-newtons-method
logistic <- function(x) {
  return(1 / (1 + exp(-x)))
}
logistic_regression_newton <- function(X, y, max_iter = 100, tol = 1e-6) {
  X <- cbind(1, X)
  beta <- rep(0, ncol(X))
  for (i in 1:max_iter) {
    p <- logistic(X %*% beta)
    
    gradient <- colSums(X * (y - p))
    
    W <- diag(p * (1 - p))
    hessian <- t(X) %*% W %*% X
    
    beta_new <- beta + solve(hessian) %*% gradient
    if (max(abs(beta_new - beta)) < tol) {
      break
    }
    beta <- beta_new
  }
  
  return(beta)
}
#########################################################
#Ridge Regression: https://online.stat.psu.edu/stat857/node/155/
ridge_regression <- function(X, y, lambda) {
  X <- cbind(1, X)
  
  beta <- solve(t(X) %*% X + lambda * diag(ncol(X))) %*% t(X) %*% y
  
  return(beta)
}

#######################################################
#https://arxiv.org/pdf/2303.03576.pdf#:~:text=In%20this%20section%2C%20we%20presents,path%20following%20algorithm%20(PFA).
#Algorithm 3: Coordinate Gradient Descent Algorithm (CGDA) 
lasso_regression <- function(X, y, lambda, max_iter = 100, tol = 1e-6) {
  X <- cbind(1, X)
  
  beta <- rep(0, ncol(X))
  
  XtX <- t(X) %*% X
  
  for (iter in 1:max_iter) {
    beta_old <- beta
    for (j in 1:length(beta)) {
      r <- y - X %*% beta + X[, j] * beta[j]
      beta[j] <- sign(sum(X[, j] * r)) * max(0, abs(sum(X[, j] * r)) - lambda) / sum(X[, j]^2)
    }
    if (max(abs(beta - beta_old)) < tol) {
      break
    }
  }
  return(beta)
}
########################################################################
#E Net 
elastic_net_regression <- function(X, y, lambda1, lambda2, alpha, max_iter = 100, tol = 1e-6) {
  data <- checkD
  
  beta <- rep(0, ncol(X))
  
  XtX <- t(X) %*% X
  
  for (iter in 1:max_iter) {
    beta_old <- beta
    
    for (j in 1:length(beta)) {
      r <- y - X %*% beta + X[, j] * beta[j]
      if (j == 1) {
        beta[j] <- sum(X[, j] * r) / (sum(X[, j]^2) + lambda2)
      } else {
        beta[j] <- sign(sum(X[, j] * r)) * max(0, abs(sum(X[, j] * r)) - lambda1 * alpha) / (sum(X[, j]^2) + lambda2 * (1 - alpha))
      }
    }
    
    if (max(abs(beta - beta_old)) < tol) {
      break
    }
  }
  return(beta)
}

####################################
#Binary Check of Data 
is_binary <- function(vector) {
  unique_values <- unique(vector)
  if (length(unique_values) != 2) {
    return(FALSE)
  }
  if (!(is.character(unique_values))) {
    return(FALSE)
  }
  if (sum(unique_values %in% c("0", "1")) == 2) {
    return(TRUE)
  }
  return(FALSE)
}
##################################
#Using all glmnet 
linear_regression_glmnet <- function(X, y) {
  model <- glmnet(X, y, alpha = 0, lambda = 0)
  return(coef(model))
}

ridge_regression_glmnet <- function(X, y, lambda) {
  model <- glmnet(X, y, alpha = 0, lambda = lambda)
  return(coef(model))
}

lasso_regression_glmnet <- function(X, y, lambda) {
  model <- glmnet(X, y, alpha = 1, lambda = lambda)
  return(coef(model))
}

elastic_net_glmnet <- function(X, y, lambda, alpha) {
  model <- glmnet(X, y, alpha = alpha, lambda = lambda)
  return(coef(model))
}
####################################
#SVM 
#From: https://www.youtube.com/watch?v=QkAmOb1AMrY&t=20s&ab_channel=Simplilearn
#For Continuous continuous response variable: type = "eps-regression"
#For binary response variable: type = "C" or "nu"

#Q: What is the difference between nu-SVC and C-SVC? ( https://stats.stackexchange.com/questions/237382/difference-between-the-types-of-svm ) 
# A: Basically they are the same thing but with different parameters. 
# The range of C is from zero to infinity but nu is always between [0,1]. 
# A nice property of nu is that it is related to the ratio of support vectors 
# and the ratio of the training error.
svm_model <- function(X, y, type = "C", kernel = "radial", cost = 1) {
  library(e1071)
  
  # Train SVM model
  if (type == "C") {
    model <- svm(y ~ ., data = as.data.frame(cbind(y, X)), type = type, kernel = kernel, cost = cost)
  } else if (type == "nu") {
    model <- svm(y ~ ., data = as.data.frame(cbind(y, X)), type = type, kernel = kernel)
  }
  
  return(model)
}

##################################### 
