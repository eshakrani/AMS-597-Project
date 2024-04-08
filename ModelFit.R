library(glmnet)

# Not sure what measures to put, maybe leave it null.

modelFit <- function(X,y, family = c("gaussian","binomial"), measure = c("mse","auc"), 
                     lambda = c(), alpha = c(), bagging = FALSE, topP = FALSE, 
                     K = 10, ensemble = FALSE, cv = FALSE) {
  
  data <- checkData(X,y)
  X <- data[,-1]
  y <- data[,1]
  
  family = c(family)[1]
  
  lambda <- c(lambda)
  alpha <- c(alpha)
  
  if (topP) {
    # Select top K predictors to be the X matrix
    # Create lambda max * epsilon
    # fit lasso regression with bagging
    # calculate the proportion of times feature i appears
    # pick the top K
  }
  
  if (!bagging) {
    if (ensemble & family == 'gaussian') {
      
      # Going to perform Stacking
      # will take the predicted values from linear regression
      # fitted on X and having SVM fit onto [X,y^]
      # return that SVM model
      
    }
    
    else if (ensemble & family == 'binomial') {
      # Going to perform Stacking
      # will take the predicted values from logistic regression
      # fitted on X and having SVM fit onto [X,y^]
      # return that SVM model
    }
    
    
    else if (family == 'gaussian') {
      
      # If cross validation use the below Regression function
      results <- fitLinearRegressor(...)
      
      # If not cross validation uses tony's functions to return a specific
      # version of linear regression (OLS,lasso,ridge)
    }
    
    else if (family == 'binomial') {
      
      results <- fitLinearClassification(...)
      
    }
  }
  
  if (bagging) {
    
    #' The bagging procedure is simple, only implement for the regression models
    #' For the weight on the bagging procedure to get the final prediction
    #' one possibility is to a uniform weighting. Another is to have an option
    #' when the data is highly skewed/imbalanced on the classification side 
    #' to weight the models that predict the in balanced category better more heavily. 
    
  }
}


#' Fit Linear Regressor with Elastic Net Regularization
#' 
#' This function searches for the best fit linear regression model with 
#' elastic net regularization. 
#' 
#' @param X The matrix of predictor variables.
#' @param y The vector of response variable.
#' @param loss The loss function to be used. Currently not used in the function, but included for potential future extensions.
#' @param lambda The penalty parameter controlling the amount of regularization. Default is 0 (no regularization).
#' @param alphas A numeric vector specifying the mixing parameter for elastic net regularization. 
#'        Alpha = 1 corresponds to Lasso regression, Alpha = 0 corresponds to Ridge regression, 
#'        and values in between represent a combination of Lasso and Ridge. 
#' @param nfolds Number of folds for cross-validation. Default is 5.
#' 
#' @return Best fit model over a grid search of lambdas and alphas using cv.glmnet
#' 
#' @import glmnet
#' 
#' @export

fitLinearRegressor <- function(X, y, loss, lambda = 0, alphas, nfolds = 5, test_size = 0.2) {
  require(glmnet)
  
  set.seed(123)  # For reproducibility
  n <- nrow(X)
  n_train <- floor((1 - test_size) * n)
  train_indices <- sample(1:n, n_train)
  
  X_train <- X[train_indices, ]
  y_train <- y[train_indices]
  
  X_test <- X[-train_indices, ]
  y_test <- y[-train_indices]
  
  best_model <- NULL
  best_cv_error <- Inf
  
  for (alpha in alphas) {

    cvfit <- cv.glmnet(X_train, y_train, alpha = alpha, lambda = lambda, nfolds = nfolds) # Cross validation
    
    lambda_best <- cvfit$lambda.min
    
    # Fit model using best lambda
    model <- glmnet(X_train, y_train, alpha = alpha, lambda = lambda_best)
    
    # Predict on test set
    y_pred <- predict(model, newx = X_test)
    
    # Calculate mean squared error on test set
    cv_error <- mean((y_test - y_pred)^2)
    
    # Update best model if current model has lower error
    if (cv_error < best_cv_error) {
      best_model <- model
      best_cv_error <- cv_error
    }
    
    final_model <- glmnet(X, y, alpha = best_model$alpha, lambda = best_model$lambda)
    
  }
  
  return(models)
}



#' Fit Logistic Regression with Elastic Net Regularization
#' 
#' This function searches for the best fit logistic regression model with 
#' elastic net regularization and cross validation.
#' 
#' @param X The matrix of predictor variables.
#' @param y The vector of response variable.
#' @param loss The loss function to be used. Currently not used in the function, but included for potential future extensions.
#' @param lambda The penalty parameter controlling the amount of regularization. Default is 0 (no regularization).
#' @param alphas A numeric vector specifying the mixing parameter for elastic net regularization. 
#'        Alpha = 1 corresponds to Lasso regression, Alpha = 0 corresponds to Ridge regression, 
#'        and values in between represent a combination of Lasso and Ridge. 
#' @param nfolds Number of folds for cross-validation. Default is 5.
#' 
#' @return Best fit model over a grid search of lambdas and alphas using cv.glmnet
#' 
#' @import glmnet
#' 
#' @export

fitLogisticRegressor <- function(X, y, loss, lambda = 0, alphas, nfolds = 5) {
  require(glmnet)
  
  set.seed(123)  # For reproducibility
  n <- nrow(X)
  n_train <- floor((1 - test_size) * n)
  train_indices <- sample(1:n, n_train)
  
  X_train <- X[train_indices, ]
  y_train <- y[train_indices]
  
  X_test <- X[-train_indices, ]
  y_test <- y[-train_indices]
  
  best_model <- NULL
  best_cv_error <- Inf
  
  for (alpha in alphas) {
    
    cvfit <- cv.glmnet(X_train, y_train, alpha = alpha, lambda = lambda, 
                       nfolds = nfolds, family = 'binomial') # Cross validation
    
    lambda_best <- cvfit$lambda.min
    
    # Fit model using best lambda
    model <- glmnet(X_train, y_train, alpha = alpha, lambda = lambda_best,
                    family = 'binomial')
    
    # Predict on test set
    y_pred <- predict(model, newx = X_test)
    
    # Calculate mean squared error on test set
    cv_error <- mean((y_test - y_pred)^2)
    
    # Update best model if current model has lower error
    if (cv_error < best_cv_error) {
      best_model <- model
      best_cv_error <- cv_error
    }
    
    final_model <- glmnet(X, y, alpha = best_model$alpha, 
                          lambda = best_model$lambda, family = 'binomial')
    
  }
  
  return(best_model)
}