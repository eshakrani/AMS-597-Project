library(glmnet)


#' 
#' @param X Input matrix of co-variates, n by p.
#' @param y Response vector, n by 1.
#' @param family Output distribution of the y vector (gaussian or binomial)
#' @param measure 
#' @param lambda Regularization parameter(s).
#' @param alpha ElasticNet mixing parameter(s). 
#' @param bagging Logical
#' @param topP Indicating whether to perform feature pre screening
#' @param K Number of top predictors to use.
#' @param ensemble Logical indicating whether to use ensemble learning.
#' @param nfolds controls the number of folds for cross validated models
#' @param test_size controls the test set size for model selection in cross validated models
#' @param R Number of iterations for bootstrap
#' 
#' @example  
#' 
modelFit <- function(X,y, family = c("gaussian","binomial"), measure = c("mse","auc"), 
                     lambda = c(), alpha = c(), bagging = FALSE, topP = FALSE, 
                     K = 10, ensemble = FALSE, nfolds = 5, test_size = 0.2,
                     R = 100) {

  
  data <- checkData(X,y)
  X <- data[,-1]
  y <- data[,1]
  
  checkAssumptions(family = family, measure = measure, lambda = lambda, alpha = alpha
                   , bagging = bagging, topP = topP, K = K, ensemble = ensemble
                   , R = R)
  # Check assumptions was having issues, will fix later.
  
  if (identical(family, c("gaussian","binomial"))) {
    family <- family[1]
  }
  
  # Automatically choose the first option if measure is left as default
  if (identical(measure, c("mse","auc"))) {
    measure <- measure[1]
  }
  
  checkAssumptions(family, measure, lambda, alpha, bagging, topP, K, ensemble
                   , nfolds, test_size, R)
  
  
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
      model <- fitLinearRegressor(X, y, alpha = alpha, lambda = lambda,
                                    nfolds = nfolds, test_size = test_size)
      
      return(model)  
      
    }
    
    else if (family == 'binomial') {
      
      model <- fitLinearClassification(X, y, alpha = alpha, lambda = lambda,
                                         nfolds = nfolds, test_size = test_size)
  
      return(model)    
    }
  
  }
  
  if (bagging) {
    
    #' The bagging procedure is simple, only implement for the regression models
    #' For the weight on the bagging procedure to get the final prediction
    #' one possibility is to a uniform weighting. Another is to have an option
    #' when the data is highly skewed/imbalanced on the classification side 
    #' to weight the models that predict the in balanced category better more heavily. 
    
    if (family == 'gaussian') {
      
      y_pred_avg <- rep(0,length(y))
      for (i in 1:R) {
        id <- sample(1:nrow(X), nrow(X), replace = T)
        
        X_bootstrap <- X[id,]
        y_bootstrap <- y[id]
        
        model <- fitLinearRegressor(X_bootstrap, y_bootstrap, alpha = alpha, lambda = lambda, family = family)
        y_pred <- predict(model, newx = X)
        y_pred_avg <- y_pred_avg + 1/R * y_pred
        
      }
      
      return(y_pred_avg)
  
    
    }
    
    if (family == 'binomial') {
      
      y_pred_avg <- rep(0,length(y))
      for (i in 1:R) {
        id <- sample(1:nrow(X), nrow(X), replace = T)
        
        X_bootstrap <- X[id,]
        y_bootstrap <- y[id]
        # needs to be changed to matchthe gaussian version.
        model <- cv.glmnet(X_bootstrap, y_bootstrap, alpha = alpha, lambda = lambda, family = family)

        y_pred <- predict(model, newx = X, type = 'response')
        y_pred_avg <- y_pred_avg + 1/R * y_pred
        
        
        
      }
      
      return(y_pred_avg)
      
    }
  }
}

#' @name 
#' Fit Linear Regression
#' 
#' @description 
#' This function searches for the best fit linear regression model using:
#' elastic net penalty, cross validation, train-test split, and grid search.
#' All model fitting is done using glmnet.  
#' 
#' @param X The matrix of predictor variables.
#' @param y The vector of response variable.
#' @param loss The loss function to be used. Currently not used in the function, but included for potential future extensions.
#' @param lambda The penalty parameter controlling the amount of regularization. Default is 0 (no regularization).
#' @param alphas A numeric vector specifying the mixing parameter for elastic net regularization. 
#'        Alpha = 1 corresponds to Lasso regression, Alpha = 0 corresponds to Ridge regression, 
#'        and values in between represent a combination of Lasso and Ridge. 
#' @param nfolds Number of folds for cross-validation. Default is 5.
#' @param test_size Size of the test set
#' 
#' @return Best fit model over a grid search of lambdas and alphas using cv.glmnet
#' 
#' @import glmnet
#' 
#' @export

fitLinearRegressor <- function(X, y, lambda = NULL, alphas, nfolds = 5, test_size = 0.2, family = 'gaussian') {
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
  final_model <- NULL
  best_alpha <- NULL
  
  for (alpha in alphas) {

    cvfit <- cv.glmnet(X_train, y_train, alpha = alpha, lambda = lambda, nfolds = nfolds, family = family) # Cross validation
    
    lambda_best <- cvfit$lambda.min
    
    # Fit model using best lambda
    model <- glmnet(X_train, y_train, alpha = alpha, lambda = lambda_best, family = family)
    
    # Predict on test set
    y_pred <- predict(model, newx = X_test)
    
    # Calculate mean squared error on test set
    cv_error <- mean((y_test - y_pred)^2)
    
    # Update best model if current model has lower error
    if (cv_error < best_cv_error) {
      best_model <- model
      best_cv_error <- cv_error
      best_alpha <- alpha
    }
    
    final_model <- glmnet(X, y, alpha = best_alpha, lambda = best_model$lambda)
    
  }
  
  return(final_model)
}




#' @name 
#' Fit Logistic Regression
#' 
#' @description 
#' This function searches for the best fit logistic regression model using:
#' elastic net penalty, cross validation, train-test split, and grid search.
#' All model fitting is done using glmnet.  
#' 
#' @param X The matrix of predictor variables.
#' @param y The vector of response variable.
#' @param loss The loss function to be used. Currently not used in the function, but included for potential future extensions.
#' @param lambda The penalty parameter controlling the amount of regularization. Default is 0 (no regularization).
#' @param alphas A numeric vector specifying the mixing parameter for elastic net regularization. 
#'        Alpha = 1 corresponds to Lasso regression, Alpha = 0 corresponds to Ridge regression, 
#'        and values in between represent a combination of Lasso and Ridge. 
#' @param nfolds Number of folds for cross-validation. Default is 5.
#' @param test_size Size of the test set
#' 
#' @return Best fit model over a grid search of lambdas and alphas using cv.glmnet
#' 
#' @import glmnet
#' 
#' @export

fitLogisticRegressor <- function(X, y, loss, lambda = NULL, alphas, nfolds = 5,  test_size = 0.2,family = 'binomial') {
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
  final_model <- NULL
  best_alpha <- NULL
  
  for (alpha in alphas) {
    
    cvfit <- cv.glmnet(X_train, y_train, alpha = alpha, lambda = lambda, 
                       nfolds = nfolds, family = 'binomial') # Cross validation
    
    lambda_best <- cvfit$lambda.min
    
    # Fit model using best lambda
    model <- glmnet(X_train, y_train, alpha = alpha, lambda = lambda_best,
                    family = 'binomial')
    
    
    # NEEDS TO BE FIXED, has to calculate non MSE loss metric.
    # Predict on test set
    #y_pred <- predict(model, newx = X_test)
    
    # Update best model if current model has lower error
    if (cv_error < best_cv_error) {
      best_model <- model
      best_cv_error <- cv_error
      best_alpha <- alpha
    }
    
    final_model <- glmnet(X, y, alpha = best_alpha, 
                          lambda = best_model$lambda, family = 'binomial')
    
  }
  
  return(final_model)
}